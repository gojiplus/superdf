# studyframe: data with collection context

studyframe keeps observations together with the information needed to interpret them:
observational units, measurements, sampling, weights, assignment, and derivations.
An analysis selects the relevant declarations and checks its requirements before
calling an established statistical package.

This is an experimental, breaking revision of the original metadata-only package.
It implements a bounded schema and tested adapters, not a complete scientific
metadata standard. A well-formed declaration does not establish that its scientific
claims or identification assumptions are true.

The R package is now named **studyframe**, formerly SuperDF. The GitHub repository
currently retains its original name. This development revision is not on CRAN.

## Install the R package

From a local checkout:

```sh
R CMD INSTALL .
```

```r
library(studyframe)
```

The core package declares and validates data and context. Install `survey` for
survey analysis, `estimatr` for experimental analysis, and `arrow` plus `zip` for
portable bundle I/O:

```r
install.packages(c("survey", "estimatr", "arrow", "zip"))
```

The [collection-to-inference vignette](vignettes/collection-to-inference.Rmd)
executes a complete simulated handoff. Schema 0.3 and `.studyframe` bundles are
experimental; this rename does not provide compatibility aliases for the earlier
unreleased API or bundle format.

## Declare once, use across analyses

```r
library(studyframe)

observations <- data.frame(
  id = 1:8,
  school = rep(1:4, each = 2),
  district = rep(1:2, each = 4),
  weight = rep(2, 8),
  income = c(12, 15, 18, 11, 20, 24, 16, 22)
)

study <- studyframe(
  observations,
  keys = "id",
  unit = "pupil",
  dataset_id = "pupil_survey",
  variables = list(variable_spec("income", units = "USD/month")),
  sampling = list(sampling_spec(
    "schools",
    population = "Pupils in the study districts",
    stages = list(sampling_stage(
      "school", method = "srs", strata = "district", replacement = "with"
    )),
    source = "Study sampling protocol"
  )),
  weights = list(weight_spec(
    "main", "weight", sampling = "schools", source = "Released design weights"
  ))
)

result <- estimate_mean(study, outcome = "income", weight = "main")
result$fit
result$analysis
```

Weights are selected explicitly. Measurement waves and row applicability are
checked. Sampling and assignment are separate declarations. Missing information
stays unknown; it does not silently become simple random sampling.

## Preserve the collection roster

```r
district_one <- study[study$district == 1, ]
estimate_mean(district_one, "income", "main", domain = TRUE)

renamed <- rename_studyframe(study, c(income = "monthly_income"))
estimate_mean(renamed, "income", "main")
```

Stable variable IDs survive renaming. The survey design is constructed on the
full applicable roster before the domain is selected. `domain = TRUE` is an
analyst declaration, not a claim that any arbitrary restriction is ignorable.

**A filtered view still contains and exports the full collection roster.** It is
not a redacted release. To distribute fewer original records, explicitly construct
a new dataset and reconsider its design declarations.

`derive_studyframe()` adds a defined measurement without overwriting original facts.
`recode_studyframe()` records an explicit value mapping into a new measurement and
requires a policy for unmapped values.
`join_studyframe()` requires keys and a unique right-hand match. `rbind()` accepts
disjoint views of the same collection; it rejects incompatible collections.
Aggregation requires an explicit new observational unit and dataset. Direct
replacement operations are refused, and adapter-time validation detects mutations
that bypass the supported methods.

## Data and context travel together

Schema 0.3 separates received responses from invited, sampled, and randomized
rosters. Ordinary dataframe imports start with unknown completeness. Experimental
inference requires an asserted-complete randomized roster scoped to its assignment,
with source evidence. Evidence records carry source locations, optional hashes,
capture times, producer identities, and versions. Instrument references connect
measurements to question and subquestion IDs. Unknown facts remain unknown;
planned sampling or assignment declarations cannot stand in for realized designs.

```r
write_studyframe(study, "study.studyframe")
received <- read_studyframe("study.studyframe")
write_codebook(received, "codebook.md")
```

One ZIP bundle contains `data.parquet`, `roster.parquet`, `context.json`, and an
integrity manifest. It contains no executable R expressions. Writers refuse to
overwrite an existing destination. RDS also preserves the R object, but the bundle
is the cross-language format. Bare data extraction is explicit:
`as.data.frame(study)` removes context.

The Python reader uses the same schema and validates references and row alignment:

```python
from studyframe import read_studyframe

study = read_studyframe("study.studyframe")
study.validate()
observations = study.data.to_pandas()
print(study.context["weights"])
```

Python currently reads and validates bundles; it does not provide analysis or
transformation adapters. The schema is packaged from the R package's single source.

The [producer conformance kit](inst/conformance/README.md) builds a bundle using
only Arrow and JSON. Both language readers run the same positive and negative cases.

## Check a request before fitting

```r
request <- analysis_request("mean", "income", weight = "main")
requirements <- check_analysis(study, request)
requirements$issues
if (requirements$ok) result <- run_analysis(study, request)
```

Checking prepares the design without fitting an outcome estimator. Issues include
stable codes, categories, affected declarations, evidence references, and remedies.
Estimation uses the same requirements and returns a native statistical fit together
with the request, input identity, selected declarations, assumptions, omissions,
and interval convention. A passing check supports execution of the declared
analysis; it does not verify the study's scientific assertions.

`run_analysis()` raises `studyframe_analysis_error` with an `issues` field when
requirements fail. Catch that class to inspect the same issue records returned by
`check_analysis()`; underlying errors are retained in `parent` for debugging.
Issue records are plain data and can be serialized without the R condition or
its backtrace. Import errors similarly provide issues under
`studyframe_import_error`; both inherit from `studyframe_error`.

Request scalar fields must be ordinary strings or logical values. Classed
scalars and one-cell matrices are rejected because they can change shape during
JSON serialization. Missing-data, preview, and recode policy names require exact
matches rather than abbreviations.

For an observational linear model, use `regression_request()` with numeric
predictor identifiers, an explicit target, and the assumptions supporting the
analysis. The adapter uses `estimatr::lm_robust()` with HC2 uncertainty for
unclustered observations or CR2/`stata` uncertainty for declared clusters. It
records the selected observations, joint missingness exclusions, clustering, and
model assumptions. Its coefficients describe associations; this route does not
certify causal identification or population representativeness, and it does not
apply sampling weights.

## Start with a Qualtrics export

```r
responses <- read_qualtrics("responses.csv", "instrument.qsf")
write_codebook(responses, "codebook.md")
context(responses)$diagnostics
```

The importer maps supported questions through stable import IDs, preserves explicit
recodes and source hashes, and can join responses to an authoritative external
roster. Response-only imports are also supported when the QSF is unavailable;
instrument identity and coding remain unresolved. Preview records require an
explicit policy. An exported response table does not establish the original
sampling or randomized roster.

See the [collection workflow](inst/examples/qualtrics.md) for supported question
types, a complete executable example, and composition with `qualtRics` downloads.

## Supported analyses and limits

| Component | Implemented |
|---|---|
| Sampling | Explicit SRS stages; one-stage with replacement; multistage without replacement with FPC at every stage |
| Replicate weights | BRR, Fay, jackknife, bootstrap, and explicitly scaled `other` specifications through R `survey` |
| Survey analysis | Numeric means; explicit weight, wave, domain, and missing-data policies |
| Assignment | Two-condition complete randomization, including blocks and clusters, with constant declared assignment probability |
| Experimental analysis | `estimatr::difference_in_means()` on a complete roster and complete numeric outcomes |
| Regression | Unweighted numeric linear models with an intercept; HC2 or clustered CR2/`stata` uncertainty, explicit target and assumptions |
| Measurement | Definitions, units, category labels, named missingness codes, measurement occasion |
| Portability | Primitive integer, double, string, and boolean columns; one current table and its attached roster |

PPS, unknown collection mechanisms, and Bernoulli assignment can be recorded but
are not translated by the current adapters. Taylor analysis of explicitly adjusted
weights is refused because calibration and adjustment variance structures are not
yet represented. Replicate specifications can carry adjustment effects when the
producer supplies the appropriate replicates. Multiple imputation, variable
eligibility logic, longitudinal estimators, arbitrary joint inclusion probabilities,
and general multi-table studies are future work. Collection import currently
covers a bounded set of Qualtrics CSV/QSF representations.

Factors, dates, nested columns, infinity, and NaN require explicit conversion.
Numeric missing codes remain in storage and are converted only for analysis.
`missing = "omit"` records an omission; it does not justify a nonresponse assumption.
Unsupported dataframe libraries may drop the class or alter its contents; only
documented studyframe operations provide the preservation contract.

See the [schema and requirements](https://github.com/gojiplus/superdf/blob/main/inst/schema/README.md),
[four reproducible workflows](https://github.com/gojiplus/superdf/blob/main/inst/examples/workflows.R), and
[API documentation](https://gojiplus.github.io/superdf/reference/index.html).

## Development

```sh
Rscript -e 'pak::local_install_dev_deps()'
uv sync --extra dev
make ci
```

`make ci` runs R and Python linting, statistical reference tests, actual R-to-Python
bundle tests, `R CMD check`, and a Python package build. `make document` regenerates
the R reference documentation. The README is the package website's main page.
`make format` uses [Air](https://posit-dev.github.io/air/) for R and Ruff for Python.

The four deterministic workflows cover stratified cluster sampling and domains,
replicate-weight inference, blocked cluster randomization, and wave-specific
weights. Tests compare estimates, standard errors, and degrees of freedom against
direct backend calls and deliberately break alignment and applicability.

For a stochastic collection-to-inference demonstration, run:

```r
source(system.file("examples", "simulated_study.R", package = "studyframe"))
run_simulated_study("simulation", repetitions = 500L)
```

This creates a finite population, samples schools, randomizes treatment, collects
observed outcomes, joins shuffled records, and writes and reloads a portable bundle
before analysis. Repeated runs report bias, estimated versus empirical uncertainty,
and interval coverage against known truth. See the
[simulation design](https://github.com/gojiplus/superdf/blob/main/inst/examples/README.md)
for the two inferential targets and limitations.

## License

MIT. See [LICENSE](LICENSE).
