# studyframe (development version)

* Focus this repository on the R package. Preserve the earlier Python reader on
  `archive/python-reader`; remove its distribution, dependencies, and test suite
  from the active build. Independent-producer bundle fixtures retain an
  interoperability check in the R tests without requiring Python.
* Analysis, import, and consistency-check errors use structured `rlang`/`cli`
  conditions with a shared `studyframe_error` superclass. Analysis and import
  issue records retain their codes and metadata; analysis errors retain their
  underlying causes for debugging.
* Analysis requests share scalar validation and reject nonportable request
  containers. Missing-data, preview, and recode policies require exact names;
  partial policy names no longer select an action implicitly.
* `regression_request()` supports explicit model-based associations with numeric
  predictors and HC2 or clustered CR2/`stata` uncertainty. A real Qualtrics
  example carries collection evidence through the analysis and RMCP handoff.

# studyframe 0.3.0

* Rename the experimental SuperDF package to `studyframe`. The constructor,
  S3 classes, transformation and I/O functions, Python reader, and bundle format
  use the new name. The repository URL remains unchanged. Earlier experimental
  bundles and function names are not supported by compatibility aliases.
* Retain roster completeness, source evidence, instrument mappings, and
  planned versus realized declarations alongside observations.
* Share analysis preparation between `check_analysis()` and `run_analysis()`;
  return structured issues and versioned analysis receipts.
* Import supported Qualtrics CSV/QSF representations, preserve unresolved
  collection facts, and join responses to an external roster without overwriting
  its assignments. Response-only imports retain unknown instrument information.
* Recode measurements with explicit mappings and preserve declared missing-code
  semantics when retaining unmapped values.
* Match numeric join keys exactly, including distinct doubles that print alike.
* Preserve valid empty views and their attached collection rosters through filtering
  and portable bundle round trips.
* Reject nonfinite numeric metadata before JSON serialization and refuse
  undefined inferential results instead of reporting them as successful analyses.
* Include an executable R vignette and shared R/Python producer conformance cases.
