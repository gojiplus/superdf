"""Produce a fixed conformance bundle using Arrow and JSON, without studyframe."""

from __future__ import annotations

import hashlib
import json
import sys
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


if __name__ == "__main__":
    write_example(sys.argv[1])
