"""Portable collection context and its attached observations."""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass
from pathlib import Path
from zipfile import ZipFile

import jsonschema
import pyarrow as pa
import pyarrow.parquet as pq


def _schema() -> dict:
    packaged = Path(__file__).with_name("context.schema.json")
    source = Path(__file__).resolve().parents[2] / "inst/schema/context.schema.json"
    return json.loads((packaged if packaged.exists() else source).read_text())


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def _index(records: list[dict], name: str) -> dict:
    result = {r["id"]: r for r in records}
    _require(len(result) == len(records), f"Duplicate {name} identifiers")
    return result


def _code_type(value: object, storage: str) -> bool:
    if storage in ("integer", "number"):
        return (
            type(value) in (int, float)
            and math.isfinite(value)
            and (storage != "integer" or value == math.floor(value))
        )
    return isinstance(value, str if storage == "string" else bool)


@dataclass(frozen=True)
class ResearchData:
    """Arrow observations, collection roster, and context; no inference is certified.

    Tables are immutable Arrow objects. Validate again after editing the context.
    Python analysis and mutation adapters are outside the current reader's scope.
    """

    data: pa.Table
    roster: pa.Table
    context: dict

    def validate(self) -> None:
        """Check structure, variable references, keys, and data/roster agreement."""
        ctx = self.context
        jsonschema.Draft7Validator(_schema()).validate(ctx)
        variables = _index(ctx["variables"], "variable")
        sampling = _index(ctx["sampling"], "sampling")
        _index(ctx["weights"], "weight")
        _index(ctx["assignments"], "assignment")
        columns = [v["column"] for v in variables.values()]
        _require(len(set(columns)) == len(columns), "Duplicate variable columns")
        _require(
            len(self.roster.column_names) == len(set(self.roster.column_names))
            and set(columns) == set(self.roster.column_names),
            "Registry must describe every roster column exactly once",
        )
        _require(
            len(self.data.column_names) == len(set(self.data.column_names))
            and set(self.data.column_names) <= set(columns),
            "Unregistered or duplicated current columns",
        )
        refs = list(ctx["units"]["keys"])
        for design in sampling.values():
            for stage in design["stages"]:
                refs.extend([stage["unit"], stage["strata"], stage["probability"]])
                if stage["fpc"]:
                    refs.append(stage["fpc"]["variable"])
        for weight in ctx["weights"]:
            _require(
                weight["sampling"] is None or weight["sampling"] in sampling,
                "Unknown sampling reference",
            )
            variance = weight["variance"]
            reps = variance["replicates"]
            refs.extend([weight["variable"], *reps])
            refs.extend(rule["variable"] for rule in weight["applies"])
            _require(
                len(variance["rscales"]) in (0, len(reps)),
                "rscales must match replicate columns",
            )
            _require(
                (not reps if variance["method"] == "taylor" else len(reps) >= 2),
                "Inconsistent replicate columns and variance method",
            )
            _require(
                variance["method"] != "Fay" or variance["rho"] is not None,
                "Fay variance requires rho",
            )
            _require(
                variance["method"] == "Fay" or variance["rho"] is None,
                "rho applies only to Fay variance",
            )
        for assignment in ctx["assignments"]:
            refs.extend(assignment[k] for k in ("treatment", "unit", "blocks"))
        for event in ctx["history"]:
            refs.extend(event["inputs"])
            refs.extend(event["outputs"])
        _require(
            all(ref in variables for ref in refs if ref is not None),
            "Unknown variable reference",
        )
        for weight in ctx["weights"]:
            for rule in weight["applies"]:
                storage = variables[rule["variable"]]["storage"]
                _require(
                    all(_code_type(v, storage) for v in rule["values"]),
                    "Applicability values must match variable storage type",
                )
        for assignment in ctx["assignments"]:
            storage = variables[assignment["treatment"]]["storage"]
            _require(
                all(_code_type(v, storage) for v in assignment["conditions"]),
                "Assignment conditions must match treatment storage type",
            )
        expected_types = {
            "integer": pa.types.is_int32,
            "number": pa.types.is_float64,
            "string": lambda t: pa.types.is_string(t) or pa.types.is_large_string(t),
            "boolean": pa.types.is_boolean,
        }
        for variable in variables.values():
            column = variable["column"]
            storage = variable["storage"]
            _require(
                expected_types[storage](self.roster[column].type),
                f"Storage type disagrees for {column}",
            )
            if storage == "number":
                _require(
                    all(
                        v is None or math.isfinite(v)
                        for v in self.roster[column].to_pylist()
                    ),
                    f"Nonfinite values in {column}",
                )
            for codes in (variable["missing"], variable["categories"]):
                encoded = [
                    (
                        "number"
                        if type(c["value"]) in (int, float)
                        else type(c["value"]),
                        c["value"],
                    )
                    for c in codes
                ]
                _require(
                    len(set(encoded)) == len(encoded), f"Duplicate codes: {column}"
                )
                for code in codes:
                    _require(
                        _code_type(code["value"], storage),
                        f"Code type disagrees for {column}",
                    )

        keys = [variables[k]["column"] for k in ctx["units"]["keys"]]

        def keyed(table: pa.Table) -> tuple[list[tuple], dict]:
            _require(
                set(keys) <= set(table.column_names), "Observation keys are missing"
            )
            for key in keys:
                _require(
                    pa.types.is_int32(table[key].type)
                    or pa.types.is_string(table[key].type)
                    or pa.types.is_large_string(table[key].type),
                    "Keys must be integer or string columns",
                )
            values = list(zip(*(table[k].to_pylist() for k in keys), strict=True))
            _require(
                all(all(v is not None for v in row) for row in values),
                "Missing observation key",
            )
            _require(len(set(values)) == len(values), "Duplicate observation key")
            return values, {key: index for index, key in enumerate(values)}

        _, roster_index = keyed(self.roster)
        current_keys, _ = keyed(self.data)
        _require(
            all(key in roster_index for key in current_keys),
            "Current observation absent from roster",
        )
        indices = pa.array([roster_index[k] for k in current_keys], type=pa.int64())
        for column in self.data.column_names:
            _require(
                self.data[column]
                .combine_chunks()
                .equals(self.roster[column].take(indices).combine_chunks()),
                f"Current values disagree with roster: {column}",
            )


def read_superdf(file: str | Path, max_bytes: int = 256 * 1024**2) -> ResearchData:
    """Read a bounded, integrity-checked ZIP/Parquet/JSON bundle.

    No files are extracted. SHA-256 verifies payload integrity, not scientific
    truth or authenticity. Only schema version 0.2.0 is accepted.
    """
    expected = {"manifest.json", "context.json", "data.parquet", "roster.parquet"}
    with ZipFile(file) as archive:
        entries = archive.infolist()
        _require(
            len(entries) == 4 and {e.filename for e in entries} == expected,
            "Unexpected or duplicate bundle entries",
        )
        _require(
            max_bytes > 0 and sum(e.file_size for e in entries) <= max_bytes,
            "Bundle exceeds uncompressed size limit",
        )
        manifest = json.loads(archive.read("manifest.json"))
        _require(
            set(manifest) == {"format", "sha256"}
            and manifest["format"] == "superdf-bundle/1",
            "Unsupported bundle manifest",
        )
        payloads = expected - {"manifest.json"}
        _require(
            set(manifest["sha256"]) == payloads, "Manifest must hash every payload"
        )
        blobs = {name: archive.read(name) for name in payloads}
        for name, blob in blobs.items():
            _require(
                hashlib.sha256(blob).hexdigest() == manifest["sha256"][name],
                f"Bundle integrity failure: {name}",
            )
    result = ResearchData(
        data=pq.read_table(pa.BufferReader(blobs["data.parquet"])),
        roster=pq.read_table(pa.BufferReader(blobs["roster.parquet"])),
        context=json.loads(blobs["context.json"]),
    )
    result.validate()
    return result
