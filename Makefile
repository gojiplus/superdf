.PHONY: test test-r test-python lint format document check build ci

test: test-r test-python

test-r:
	Rscript -e 'testthat::test_local(stop_on_failure = TRUE)'

test-python:
	uv run --extra dev pytest

lint:
	Rscript -e 'pkgload::load_all(quiet = TRUE); lints <- lintr::lint_package(); print(lints); if (length(lints)) quit(status = 1)'
	uv run --extra dev ruff check python inst/conformance/producer.py
	uv run --extra dev ruff format --check python inst/conformance/producer.py

format:
	air format R tests inst/examples
	uv run --extra dev ruff format python inst/conformance/producer.py

document:
	Rscript -e 'roxygen2::roxygenise()'

check:
	Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning", check_dir = "build/check")'

build:
	uv build

ci: lint test check build
