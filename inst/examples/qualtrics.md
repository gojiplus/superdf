# From Qualtrics collection to inference

`read_qualtrics()` joins response measurements to documented collection context.
It runs locally, without an account or API credentials. The first supported path
uses numeric CSV exports with stable import IDs and a matching QSF instrument
snapshot. Response-only imports are useful for older studies, but retain unknown
coding and instrument identity.

Before collection, create a stable unit identifier outside Qualtrics and retain the
complete invitation, sampling, or randomized assignment roster. Pass that identifier
through embedded data on each response. Capture the published instrument and its
QSF, the collection dates, and the numeric recode mapping. Keep each instrument
revision separately. A QSF exported later can reflect a different draft: its hash
identifies that file, not what a respondent actually saw. Record the collection-time
hash alongside the collection manifest and use it as `expected_qsf_sha256`.

Export responses using numeric values and stable internal identifiers. Qualtrics
CSV exports commonly have three header rows: export tags, question labels, and
JSON `ImportId` values. studyframe uses `ImportId`, so editing a display label does not
silently change question identity. Single-header CSVs with those same import IDs
are also accepted. IDs are normalized into schema-compatible column names;
normalization collisions fail. Response IDs must be unique. Duplicate submissions
for a roster unit fail until the study supplies an explicit resolution rule.

```r
library(studyframe)

study <- read_qualtrics(
  "responses.csv", "instrument.qsf",
  roster = assignment_roster,
  respondent_key = "embeddedData-unit_id",
  column_types = c(unit_id = "integer", QID1 = "number"),
  expected_qsf_sha256 = collection_manifest$qsf_sha256,
  instrument_version = "published-wave1"
)
check_analysis(study, analysis_request("effect", "QID1", assignment = "offer_assignment"))
result <- estimate_effect(study, "QID1", "offer_assignment")
write_studyframe(study, "collected.studyframe")
```

`assignment_roster` is a studyframe with a realized assignment declaration, source
evidence, and a declared complete randomized roster scoped to that assignment.
The join retains every roster unit and its original assignment. Unknown incoming
units, mismatched key types, repeated submissions, and attempted overwrites fail.
Nonrespondents remain present with missing measurements. The current experimental
adapter requires complete outcomes; it stops when those outcomes are absent.

Run the complete synthetic example to see this contract, including generation of
an external assignment file, a vendor-shaped instrument and response export,
shuffled response order, import, portable bundle round-trip, codebook, and an
`estimatr` analysis:

```r
source(system.file("examples", "qualtrics_workflow.R", package = "studyframe"))
result <- run_qualtrics_workflow("qualtrics-example")
result$analysis$fit
```

This example is simulated, not a real partner deployment. A real archival CSV has
also been exercised locally in response-only mode. Validation of a real paired
CSV/QSF collected under this prospective workflow, and an independent second
study, remain field-validation work; the synthetic fixtures do not satisfy those
criteria.

## What is carried forward

CSV and QSF SHA-256 hashes, source locations and capture timestamps accompany the
data. Supported question variables point to their QSF question and subquestion
IDs. Single-answer numeric codes receive labels from explicit QSF `RecodeValues`;
unmapped or ambiguous recodes fail. A hash match verifies the expected artifact,
not the scientific accuracy of its declarations. Source files themselves are not
embedded in the bundle; retain them in an accessible, appropriately controlled
archive. Their local paths can be identifying, so review source URIs before sharing.

The bounded question support is single-answer multiple choice (SAVR, SAHR, SACOL,
DL, SB), simple/multiline text entry (SL, ML, ESTB), text-entry forms (FORM), and
choice-attached text fields. Forms produce one mapped variable per exported
subquestion. Matrix, multiple-selection, loop-and-merge and ambiguous mappings
are not interpreted; unsupported QSF question exports fail explicitly. Numeric
text-entry outcomes require `column_types`; their numeric-looking contents alone
do not establish a measurement scale. Response-only mode preserves question
values as strings and labels from the CSV without inventing a categorical codebook.

Blanks stay missing with unknown reason. They do not become refusal, ineligibility,
or randomized non-exposure. `Finished` is retained as a boolean, not used as an
eligibility filter: reaching the end can include screenouts. Known preview/test,
offline-preview and synthetic records require an explicit `preview` policy;
the default is to stop. `preview = "exclude"` records the policy and removes those
records, while retaining genuine incomplete responses. Unknown or absent response
types remain unresolved diagnostics. Survey randomizers are recorded as unsupported
assignment evidence; their settings never create a Bernoulli or complete-randomization
declaration. A received-response dataset is never labeled a complete randomized roster.

```r
archive <- read_qualtrics("old_responses.csv", preview = "include")
context(archive)$diagnostics
write_codebook(archive, "archival-codebook.md")
```

This import preserves collection uncertainty. Inferring a population mean or
experimental effect still requires the appropriate design, weights, roster and
measurements. Import diagnostics travel with the bundle and analysis record;
analysis-specific requirements decide whether a requested analysis is supported.

## Existing API workflows

Use `qualtRics` to retrieve responses, with its existing authentication and retry
behavior. studyframe adds no HTTP or credential client. With credentials already
registered using `qualtRics`, capture the unconverted numeric export locally:

```r
responses <- qualtRics::fetch_survey(
  surveyID = "SV_your_survey",
  import_id = TRUE, label = FALSE, convert = FALSE,
  breakout_sets = TRUE, add_column_map = TRUE
)
saveRDS(qualtRics::extract_colmap(responses), "response-column-map.rds")
utils::write.csv(as.data.frame(responses), "responses.csv", row.names = FALSE, na = "")
study <- read_qualtrics("responses.csv", "instrument.qsf")
```

Keep the column map with the original export; the single-header path uses the
instrument for question labels. This code is a documented composition of existing
APIs, not a live-account acceptance test. It intentionally leaves instrument
publication/version capture and the authoritative collection roster explicit.

Official references: [Qualtrics export options](https://www.qualtrics.com/support/survey-platform/data-and-analysis-module/data/download-data/export-options/),
[QSF import/export](https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/),
[response types and Finished](https://www.qualtrics.com/support/survey-platform/data-and-analysis-module/data/download-data/understanding-your-dataset/),
[question-definition schema](https://developer-docs.qualtrics.com/docs/public-api/surveyDefinitions/schemas/question-definition/),
and [qualtRics fetch_survey](https://docs.ropensci.org/qualtRics/reference/fetch_survey.html).
