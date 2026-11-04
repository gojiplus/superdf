# SuperDF: data with collection context

SuperDF keeps observations together with the information needed to interpret them:
observational units, measurements, sampling, weights, assignment, and derivations.
An analysis selects the relevant declarations and checks its requirements before
calling an established statistical package.

This is an experimental, breaking revision of the original metadata-only package.
It implements a bounded schema and tested adapters, not a complete scientific
metadata standard. A well-formed declaration does not establish that its scientific
claims or identification assumptions are true.

## Declare once, use across analyses

```r
library(superdf)

observations <- data.frame(
  id = 1:8,
  school = rep(1:4, each = 2),
  district = rep(1:2, each = 4),
  weight = rep(2, 8),
  income = c(12, 15, 18, 11, 20, 24, 16, 22)
)

study <- superdf(
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

renamed <- rename_superdf(study, c(income = "monthly_income"))
estimate_mean(renamed, "income", "main")
```

Stable variable IDs survive renaming. The survey design is constructed on the
full applicable roster before the domain is selected. `domain = TRUE` is an
analyst declaration, not a claim that any arbitrary restriction is ignorable.

**A filtered view still contains and exports the full collection roster.** It is
not a redacted release. To distribute fewer original records, explicitly construct
a new dataset and reconsider its design declarations.

`derive_superdf()` adds a defined measurement without overwriting original facts.
`join_superdf()` requires keys and a unique right-hand match. `rbind()` accepts
disjoint views of the same collection; it rejects incompatible collections.
Aggregation requires an explicit new observational unit and dataset. Direct
replacement operations are refused, and adapter-time validation detects mutations
that bypass the supported methods.

## Data and context travel together

```r
write_superdf(study, "study.superdf")
received <- read_superdf("study.superdf")
write_codebook(received, "codebook.md")
```

One ZIP bundle contains `data.parquet`, `roster.parquet`, `context.json`, and an
integrity manifest. It contains no executable R expressions. Writers refuse to
overwrite an existing destination. RDS also preserves the R object, but the bundle
is the cross-language format. Bare data extraction is explicit:
`as.data.frame(study)` removes context.

The Python reader uses the same schema and validates references and row alignment:

```python
from superdf import read_superdf

study = read_superdf("study.superdf")
study.validate()
observations = study.data.to_pandas()
print(study.context["weights"])
```

Python currently reads and validates bundles; it does not provide analysis or
transformation adapters. The schema is packaged from the R package's single source.

## Supported analyses and limits

| Component | Implemented |
|---|---|
| Sampling | Explicit SRS stages; one-stage with replacement; multistage without replacement with FPC at every stage |
| Replicate weights | BRR, Fay, jackknife, bootstrap, and explicitly scaled `other` specifications through R `survey` |
| Survey analysis | Numeric means; explicit weight, wave, domain, and missing-data policies |
| Assignment | Two-condition complete randomization, including blocks and clusters, with constant declared assignment probability |
| Experimental analysis | `estimatr::difference_in_means()` on a complete roster and complete numeric outcomes |
| Measurement | Definitions, units, category labels, named missingness codes, measurement occasion |
| Portability | Primitive integer, double, string, and boolean columns; one current table and its attached roster |

PPS, unknown collection mechanisms, and Bernoulli assignment can be recorded but
are not translated by the current adapters. Taylor analysis of explicitly adjusted
weights is refused because calibration and adjustment variance structures are not
yet represented. Replicate specifications can carry adjustment effects when the
producer supplies the appropriate replicates. Multiple imputation, variable
eligibility logic, longitudinal estimators, arbitrary joint inclusion probabilities,
general multi-table studies, and collection-system imports are future work.

Factors, dates, nested columns, infinity, and NaN require explicit conversion.
Numeric missing codes remain in storage and are converted only for analysis.
`missing = "omit"` records an omission; it does not justify a nonresponse assumption.
Unsupported dataframe libraries may drop the class or alter its contents; only
documented SuperDF operations provide the preservation contract.

See the [schema and requirements](https://github.com/soodoku/superdf/blob/main/inst/schema/README.md),
[four reproducible workflows](https://github.com/soodoku/superdf/blob/main/inst/examples/workflows.R), and
[API documentation](https://soodoku.github.io/superdf/reference/index.html).

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

## License

MIT. See [LICENSE](LICENSE).
