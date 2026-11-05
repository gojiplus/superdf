# Producer conformance example

The portable interface is the bundle and its versioned JSON schema. A producer
does not need to import studyframe to create one. This repository ships an R
package; the Python file here is a standalone producer example.

`producer.py` uses PyArrow and Python's standard library to write eight synthetic
observations, a matching roster, the hand-authored `context.json`, and a manifest
with SHA-256 hashes of the three payloads. Its protocol is illustrative; neither
the fixture nor its evidence hash establishes that a real randomization occurred.

```sh
python -m pip install pyarrow
python inst/conformance/producer.py example.studyframe
```

The destination must not already exist. The four ZIP members are `data.parquet`,
`roster.parquet`, `context.json`, and `manifest.json`. Consult the
[schema guide](../schema/README.md) and [schema](../schema/context.schema.json)
for field definitions and reference constraints.

Read and analyze the example in R:

```r
library(studyframe)
study <- read_studyframe("example.studyframe")
request <- analysis_request("effect", "y", assignment = "trial")
stopifnot(check_analysis(study, request)$ok)
result <- run_analysis(study, request)
result$fit
```

`cases.json` supplies positive and negative cases. The producer mutates the
context before writing, so every case has a valid integrity manifest. The R
reader checks structural validity: unknown evidence references, duplicate
evidence IDs, incompatible roster scope, and unsupported schema versions fail.
Unknown instrument versions remain valid. A received-response roster and a planned
assignment are valid metadata but cannot satisfy the requested effect analysis.
R checks and executes supported inference.

The generated bundles are checked into `tests/testthat/fixtures/conformance/`.
`make test` reads them in R without launching Python or installing Python
dependencies. The fixture provenance records the producer and Arrow versions.
To regenerate candidates for review, use a new directory:

```sh
python inst/conformance/producer.py --cases /path/to/new-fixture-directory
```

Review the generated payloads before replacing the checked-in fixtures. These
fixtures cover the bounded profile implemented here; they do not certify
arbitrary survey designs or vendor exports. This is a producer example and an R
reader check, not a maintained Python reader or a Python distribution.
