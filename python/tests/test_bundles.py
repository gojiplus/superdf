"""Exercise the actual R writer against the independently implemented reader."""

import copy
import hashlib
import json
import subprocess
from dataclasses import replace
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile

import pyarrow as pa
import pytest
from studyframe import read_studyframe


@pytest.fixture(scope="session")
def bundles(tmp_path_factory):
    directory = tmp_path_factory.mktemp("r-bundles")
    root = Path(__file__).resolve().parents[2]
    script = """
pkgload::load_all('.', quiet = TRUE)
source('inst/examples/workflows.R')
args <- commandArgs(trailingOnly = TRUE)
examples <- write_example_bundles(args[[1]])
view <- rename_studyframe(examples$survey[c(8, 2, 1), ], c(income = 'monthly income'))
write_studyframe(view, file.path(args[[1]], 'view.studyframe'))
codes <- studyframe(data.frame(id = c('अ', 'é', 'x'), y = c(1, -9, NA)),
  'id', 'person', 'unicode', variables = list(variable_spec('y',
  missing = list(list(value = -9, reason = 'refused')))))
write_studyframe(codes, file.path(args[[1]], 'codes.studyframe'))
"""
    subprocess.run(
        ["Rscript", "-e", script, str(directory)],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    )
    return directory


@pytest.mark.parametrize("name", ["survey", "replicates", "experiment", "waves"])
def test_r_reference_workflows_read_in_python(bundles, name):
    dataset = read_studyframe(bundles / f"{name}.studyframe")
    dataset.validate()
    assert dataset.data.equals(dataset.roster)
    assert dataset.context["schema_version"] == "0.3.0"


def test_view_retains_roster_and_stable_variable_identity(bundles):
    dataset = read_studyframe(bundles / "view.studyframe")
    assert dataset.data.num_rows == 3
    assert dataset.roster.num_rows == 24
    assert dataset.data["id"].to_pylist() == [8, 2, 1]
    variable = next(v for v in dataset.context["variables"] if v["id"] == "income")
    assert variable["column"] == "monthly income"
    assert dataset.context["history"][-1]["operation"] == "rename"


def test_unknowns_codes_unicode_and_nulls_remain_distinct(bundles):
    dataset = read_studyframe(bundles / "codes.studyframe")
    assert dataset.data["id"].to_pylist() == ["अ", "é", "x"]
    assert dataset.data["y"].to_pylist() == [1.0, -9.0, None]
    assert dataset.context["sampling"] == []
    variable = next(v for v in dataset.context["variables"] if v["id"] == "y")
    assert variable["units"] is None
    assert variable["missing"] == [{"value": -9, "reason": "refused"}]
    assert variable["status"] == "unknown"


def test_reader_refuses_semantic_inconsistency(bundles):
    dataset = read_studyframe(bundles / "survey.studyframe")
    ctx = copy.deepcopy(dataset.context)
    ctx["weights"][0]["variable"] = "absent"
    with pytest.raises(ValueError, match="Unknown variable"):
        replace(dataset, context=ctx).validate()
    duplicated = dataset.data.take(pa.array([0, 0], type=pa.int64()))
    with pytest.raises(ValueError, match="Duplicate observation"):
        replace(dataset, data=duplicated).validate()
    changed = dataset.data.set_column(
        dataset.data.column_names.index("income"),
        "income",
        pa.array([999] * dataset.data.num_rows, type=pa.int32()),
    )
    with pytest.raises(ValueError, match="disagree with roster"):
        replace(dataset, data=changed).validate()


@pytest.mark.parametrize("scale", [float("inf"), float("nan")])
def test_nonfinite_metadata_is_rejected(bundles, scale):
    dataset = read_studyframe(bundles / "replicates.studyframe")
    ctx = copy.deepcopy(dataset.context)
    ctx["weights"][0]["variance"]["scale"] = scale
    with pytest.raises(ValueError, match="Numeric metadata must be finite"):
        replace(dataset, context=ctx).validate()


def test_integrity_and_size_limits(bundles, tmp_path):
    source = bundles / "survey.studyframe"
    with pytest.raises(ValueError, match="size limit"):
        read_studyframe(source, max_bytes=10)
    with ZipFile(source) as archive:
        entries = {n: archive.read(n) for n in archive.namelist()}
    entries["context.json"] = b"{}"
    damaged = tmp_path / "damaged.studyframe"
    with ZipFile(damaged, "w", ZIP_DEFLATED) as archive:
        for name, value in entries.items():
            archive.writestr(name, value)
    with pytest.raises(ValueError, match="integrity failure"):
        read_studyframe(damaged)


def test_schema_version_checked_even_with_valid_hashes(bundles, tmp_path):
    with ZipFile(bundles / "survey.studyframe") as archive:
        entries = {n: archive.read(n) for n in archive.namelist()}
    ctx = json.loads(entries["context.json"])
    ctx["schema_version"] = "9000"
    entries["context.json"] = json.dumps(ctx).encode()
    manifest = json.loads(entries["manifest.json"])
    manifest["sha256"]["context.json"] = hashlib.sha256(
        entries["context.json"]
    ).hexdigest()
    entries["manifest.json"] = json.dumps(manifest).encode()
    target = tmp_path / "future.studyframe"
    with ZipFile(target, "w", ZIP_DEFLATED) as archive:
        for name, value in entries.items():
            archive.writestr(name, value)
    with pytest.raises(Exception, match="0.3.0"):
        read_studyframe(target)
