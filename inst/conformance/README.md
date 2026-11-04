# Producer conformance example

The portable interface is the bundle and its versioned JSON schema. A producer
does not need to import the R or Python SuperDF package to create one.

`producer.py` uses PyArrow and Python's standard library to write eight synthetic
observations, a matching roster, the hand-authored `context.json`, and a manifest
with SHA-256 hashes of the three payloads. Its protocol is illustrative; neither
the fixture nor its evidence hash establishes that a real randomization occurred.

```sh
python inst/conformance/producer.py example.superdf
```

The destination must not already exist. The four ZIP members are `data.parquet`,
`roster.parquet`, `context.json`, and `manifest.json`. Consult the
[schema guide](../schema/README.md) and [schema](../schema/context.schema.json)
for field definitions and reference constraints.

Read and analyze the example in R:

```r
library(superdf)
study <- read_superdf("example.superdf")
request <- analysis_request("effect", "y", assignment = "trial")
stopifnot(check_analysis(study, request)$ok)
result <- run_analysis(study, request)
result$fit
```

Validate the same bundle in Python:

```python
from superdf import read_superdf

study = read_superdf("example.superdf")
study.validate()
```

`cases.json` supplies shared positive and negative cases. Tests mutate the context
before writing, so every case has a valid integrity manifest. The R and Python
readers must agree on structural validity: unknown evidence references, duplicate
evidence IDs, incompatible roster scope, and unsupported schema versions fail.
Unknown instrument versions remain valid. A received-response roster and a planned
assignment are valid metadata but cannot satisfy the requested effect analysis.
The Python reader checks structure; R checks and executes supported inference.

Run the cross-language tests from the repository with `pytest
python/tests/test_conformance.py`. They require R's development dependencies as
well as the Python development environment. These fixtures cover the bounded
profile implemented here; they do not certify arbitrary survey designs or vendor
exports.
