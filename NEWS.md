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
* Reject nonfinite numeric metadata before JSON serialization and refuse
  undefined inferential results instead of reporting them as successful analyses.
* Include an executable R vignette and shared R/Python producer conformance cases.
