"""Produce a fixed conformance bundle using Arrow and JSON, without studyframe."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile

import pyarrow as pa
import pyarrow.parquet as pq


def write_example(destination: str | Path, context: dict | None = None) -> Path:
    """Write the illustrative study; optional context supports negative fixtures."""
    destination = Path(destination)
    if context is None:
        context = json.loads(Path(__file__).with_name("context.json").read_text())
    table = pa.table(
        {
            "id": pa.array(range(1, 9), type=pa.int32()),
            "z": pa.array([0, 1] * 4, type=pa.int32()),
            "y": pa.array([1, 5, 2, 6, 3, 7, 4, 8], type=pa.float64()),
        }
    )
    buffer = pa.BufferOutputStream()
    pq.write_table(table, buffer)
    parquet = buffer.getvalue().to_pybytes()
    payloads = {
        "data.parquet": parquet,
        "roster.parquet": parquet,
        "context.json": json.dumps(context, ensure_ascii=False).encode("utf-8"),
    }
    manifest = {
        "format": "studyframe-bundle/1",
        "sha256": {
            name: hashlib.sha256(blob).hexdigest() for name, blob in payloads.items()
        },
    }
    with ZipFile(destination, "x", compression=ZIP_DEFLATED) as archive:
        for name, blob in payloads.items():
            archive.writestr(name, blob)
        archive.writestr("manifest.json", json.dumps(manifest).encode("utf-8"))
    return destination


def write_cases(destination: Path) -> None:
    """Regenerate the small independent-producer fixtures used by R tests."""
    source = Path(__file__).parent
    template = json.loads((source / "context.json").read_text())
    cases = json.loads((source / "cases.json").read_text())
    destination.mkdir(parents=True)
    for case in cases:
        context = copy.deepcopy(template)
        for patch in case["patch"]:
            target = context
            for key in patch["path"][:-1]:
                target = target[key]
            target[patch["path"][-1]] = patch["value"]
        write_example(destination / f"{case['name']}.studyframe", context)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    parser.add_argument(
        "--cases",
        action="store_true",
        help="Write all conformance cases to a new directory",
    )
    args = parser.parse_args()
    if args.cases:
        write_cases(args.destination)
    else:
        write_example(args.destination)
