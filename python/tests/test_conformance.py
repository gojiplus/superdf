"""Independent producer cases accepted/refused by both language readers."""

import copy
import json
import runpy
import subprocess
from pathlib import Path

import jsonschema
import pytest
from superdf import read_superdf

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "inst/conformance"
CASES = json.loads((FIXTURES / "cases.json").read_text())


@pytest.fixture(scope="module")
def produced(tmp_path_factory):
    directory = tmp_path_factory.mktemp("independent-producer")
    write_example = runpy.run_path(str(FIXTURES / "producer.py"))["write_example"]
    template = json.loads((FIXTURES / "context.json").read_text())
    for case in CASES:
        context = copy.deepcopy(template)
        for patch in case["patch"]:
            target = context
            for key in patch["path"][:-1]:
                target = target[key]
            target[patch["path"][-1]] = patch["value"]
        write_example(directory / f"{case['name']}.superdf", context)
    return directory


@pytest.mark.parametrize("case", CASES, ids=lambda c: c["name"])
def test_python_reader_conforms_to_independent_cases(produced, case):
    path = produced / f"{case['name']}.superdf"
    if case["valid"]:
        dataset = read_superdf(path)
        assert dataset.data.num_rows == 8
        assert (
            dataset.context["evidence"][0]["producer"] == "SuperDF conformance example"
        )
    else:
        with pytest.raises((ValueError, jsonschema.ValidationError)):
            read_superdf(path)


def test_r_reader_and_analysis_conform_to_same_cases(produced):
    script = r"""
pkgload::load_all('.', quiet=TRUE)
folder <- commandArgs(trailingOnly=TRUE)[[1]]
paths <- list.files(folder, pattern='[.]superdf$', full.names=TRUE)
results <- lapply(paths, function(path) {
  x <- tryCatch(read_superdf(path), error=identity)
  valid <- !inherits(x, 'error')
  request <- analysis_request('effect','y',assignment='trial')
  list(name=sub('[.]superdf$', '', basename(path)), valid=valid,
    analysis_ok=if (valid) check_analysis(x, request)$ok else NULL)
})
cat(jsonlite::toJSON(results, auto_unbox=TRUE))
"""
    completed = subprocess.run(
        ["Rscript", "-e", script, str(produced)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    actual = {item["name"]: item for item in json.loads(completed.stdout)}
    for case in CASES:
        assert actual[case["name"]]["valid"] == case["valid"], case["name"]
        if case["valid"]:
            assert actual[case["name"]]["analysis_ok"] == case["analysis_ok"], case[
                "name"
            ]
