.PHONY: test test-r lint format document check build ci

test: test-r

test-r:
	Rscript -e 'testthat::test_local(stop_on_failure = TRUE)'

lint:
	Rscript -e 'pkgload::load_all(quiet = TRUE); lints <- lintr::lint_package(); print(lints); if (length(lints)) quit(status = 1)'

format:
	air format R tests inst/examples

document:
	Rscript -e 'roxygen2::roxygenise()'

check:
	Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning", check_dir = "build/check")'

build:
	R CMD build .

ci: lint test check build
