# Collection context schema 0.3.0

The objective is to preserve the relationship between observations and the
inferences they support. studyframe carries facts and assumptions with data; analysis
requests determine which declarations are needed. Structural validity, consistency
with observations, and sufficiency for a particular analysis are separate checks.

`context.schema.json` is the authoritative Draft 7 JSON Schema, shipped with the R
package and available to independent producers. The current version is experimental;
no migration or backward-compatibility layer is provided.

## Requirements and precedents

| Requirement | Source | Implementation evidence |
|---|---|---|
| Ordered sampling units, strata, replacement, probabilities and FPC | [survey::svydesign](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svydesign.html) | Stratified and two-stage reference tests |
| Retain original design when selecting domains | [subset.survey.design](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/subset.survey.design.html) | Domain test retains roster before subsetting |
| Replicate method, scaling, centering, combined weights, Fay parameter | [survey::svrepdesign](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svrepdesign.html) | Explicitly scaled delete-one release and backend comparisons |
| Distinguish collection characteristics from analysis settings | [Stata svyset](https://www.stata.com/manuals/svysvyset.pdf) | Sampling declarations are separate from variance specifications |
| Assignment mechanism, blocks, assignment units and probabilities | [randomizr::declare_ra](https://declaredesign.org/r/randomizr/reference/declare_ra.html) | Complete randomization counts checked on original assignment units |
| Supported experimental estimators | [estimatr::difference_in_means](https://declaredesign.org/r/estimatr/reference/difference_in_means.html) | Blocked cluster reference test; attrition refusal |
| Measurement meaning, collection history and unknowns | [DDI product overview](https://ddialliance.org/product_overview) | Variable declarations, named missing codes and source/status fields |
| Separate data, design and analysis while keeping them bound | [Python svy](https://svylab.com/docs/svy/getting_started.html) | Language-independent schema and bundles from an independent producer read by R |
| Weight applicability depends on analysis wave and sample | [ANES guide](https://electionstudies.org/wp-content/uploads/2026/04/anes_timeseries_2024_userguidecodebook_accessible_html.html) | Synthetic two-wave fixture; no ANES data or inferred ANES declarations are shipped |

This is informed by DDI terminology, not a claim of DDI conformance. Existing
package objects are not embedded as opaque serialized payloads. Adapters map the
portable fields into those packages' public interfaces.

## Field dictionary

All object fields shown in JSON Schema are required; a nullable field explicitly
stores `null` when unknown. Empty component arrays mean no declarations are
available. They do not mean that no design feature existed.

| Path | Meaning and constraints |
|---|---|
| `schema_version` | Exactly `0.3.0`; separate from a dataset's own version |
| `dataset.id/version/author/notes` | Dataset identity, release, responsibility and free-text context |
| `units.observation` | What one row represents; descriptive, not automatically inferred |
| `units.keys` | Nonempty ordered list of variable IDs forming a unique, nonmissing key; integer or string values |
| `variables[].id` | Stable identifier matching `[A-Za-z][A-Za-z0-9_]*`; unique within the dataset |
| `variables[].column` | Current storage name; may contain spaces; renaming changes this mapping, not the ID |
| `variables[].storage` | `integer` (32-bit), `number` (64-bit float), `string`, or `boolean` |
| `variables[].label/units` | Definition and measurement units; neither is a statistical-model role |
| `variables[].categories` | Typed value/label records; distinct codes; an empty list leaves the coding undocumented |
| `variables[].missing` | Typed value/reason records; reasons distinguish refusal, attrition, inapplicability and others; storage is unchanged |
| `variables[].wave` | Optional measurement occasion, used to check declared weight applicability |
| `source/status` | Component-level provenance; documented, assumed, or unknown. These are user assertions, not software verification |
| `sampling[].id/state/population` | Named collection mechanism, knowledge state, and population/frame description |
| `sampling[].stages` | Ordered stages from largest to smallest sampling unit |
| `stages[].unit/strata` | Variable references; sampling strata are distinct from assignment blocks |
| `stages[].method/replacement` | Explicit method and replacement status; omitted method/replacement defaults to unknown |
| `stages[].probability` | Optional stage inclusion-probability variable, not a treatment probability |
| `stages[].fpc` | Variable plus explicit kind: population size or sampling fraction |
| `weights[].id/variable/role` | A named weight and its column; base, final, or analysis role |
| `weights[].sampling` | Related sampling declaration; can be null for a replicate-weight release with undisclosed design |
| `weights[].waves` | Supported measurement occasions; empty is undocumented, not universal |
| `weights[].applies` | Conjunction of typed membership rules `{variable, values}` identifying the reference sample for this weight |
| `weights[].adjustments` | Descriptions of producer adjustments; retained even when no adapter supports them |
| `weights[].variance` | Explicit producer/analyst variance convention; not an intrinsic claim about the data-generating process |
| `variance.method/replicates` | Taylor or specified replication method and ordered column references |
| `variance.combined` | Whether replicate columns already include the full-sample weights |
| `variance.scale/rscales` | Overall and per-replicate multipliers; custom method requires both |
| `variance.mse/rho` | Center at full-sample estimate versus replicate mean; rho belongs only to Fay |
| `variance.lonely_psu` | Explicit analysis policy; defaults to fail; wrappers restore global R options |
| `assignments[].treatment/unit/blocks` | Realized assignment, assignment unit, and optional block references |
| `assignments[].mechanism/prob/conditions` | Mechanism, constant second-condition probability, and ordered control/treatment codes |
| `roster.kind/source/completeness/scope/evidence` | What the attached roster represents, the attributed assertion, and its declaration scope; ordinary imports default to unknown completeness |
| `evidence[].id/uri/sha256/producer/captured_at/version/description` | Source artifact identity and provenance; hash, capture time, and version may be unknown; sources are never fetched during validation or analysis |
| `variables[].instrument` | Optional source evidence ID plus vendor question/subquestion IDs; instrument versions belong to the identified source artifact |
| `evidence` on variables/designs/weights | References to evidence registry IDs; all references must resolve |
| `phase` on sampling/weights/assignments | planned or realized; planned declarations cannot establish realized inference conditions |
| `diagnostics[]` | Preserved source/import uncertainties using the shared issue shape; these are not universal blockers for unrelated analyses |
| `history[]` | Operation, details, parent fingerprints, input/output variable IDs and row counts; not a general executable transformation language |

Current and reference tables share the variable registry. A view may omit columns
but must retain its keys. Declared design columns missing from a view cause the
relevant adapter to refuse analysis. The reference table keeps them available for
other views and for auditing.

## Semantic checks beyond JSON Schema

- Unique declaration and evidence IDs and column mappings; every reference resolves.
- Asserted-complete rosters name their kind, source, and supporting evidence. Complete
  sampled/randomized rosters also name a matching sampling/assignment scope.
- Distinct artifact records may retain different snapshots of the same URI/version.
  Conflicting reuse of the same evidence ID is refused; a source hash identifies
  bytes but does not certify its claims.
- Every roster column is registered with its actual storage type.
- Current keys are unique, nonmissing, and contained in the roster.
- Every current value equals its roster value at the matching key.
- Category and missing codes have compatible types and are not repeated.
- Replication columns, scales and method parameters agree structurally.
- Analysis-time checks verify positive applicable weights, sampling probabilities,
  FPC interpretation, outcome wave, allocation counts, and cluster/block consistency.

The attached in-memory seal detects accidental mutation through unsupported R
operations. It is not an authentication mechanism. Bundle hashes detect file
corruption; semantic validation is still required after hash verification.
Operation parent fingerprints and result input fingerprints use SHA-256 over R
serialization version 2. They identify the R object used at that step, not a
cross-language canonical representation or a bundle checksum. Results also retain
the backend, studyframe, and R versions.

## Adapter boundary

Survey designs are constructed on all rows satisfying the selected weight's
applicability rules. A narrower current view requires the explicit domain
declaration. Full-sample weights are treated as supplied; the adapter does not
estimate a response model or recreate adjustments. Taylor analysis refuses
explicitly adjusted weights until their additional variance structures are modeled.

FPC kinds are normalized to population sizes before calling survey, including when
different stages use different representations. This avoids relying on backend
magnitude heuristics. A census of entirely singleton strata is currently refused.

The effect adapter requires a sourced, asserted-complete randomized roster scoped
to its chosen assignment. It does not infer completeness from the supplied rows.
It uses that declared roster, checks that assignment is constant
within assignment units, and compares realized counts with the declared complete
randomization. It estimates with the backend's design-specific difference in
means. It does not infer randomization from balance, justify noninterference,
extrapolate using sampling weights, or treat responder-only data as a new experiment.

Sampling and assignment declarations can coexist. Supporting a combination of
sampling and experimental inference requires a dedicated adapter; none is selected
automatically. The current effect target is the randomized roster.

## Operation contract

| Operation | Behavior |
|---|---|
| Row selection/reordering | Retain roster; check unique current keys; record restriction |
| Column selection | Retain registry and roster; keys cannot be dropped; adapters check needed columns |
| Rename | Update current and roster names and ID-to-column mapping |
| Derive | Add a new variable and definition; retain inputs; rows outside the view receive NA for that derived variable |
| Join | Many-to-one left join on explicit, type-compatible keys; declare every new measurement; apply to full roster |
| Bind | Only disjoint views of the same context and roster; incompatible collections require a new explicit declaration |
| Aggregate | Refuse implicit preservation; require explicit construction at the new observational unit |
| Unknown mutation | Refuse if detected; ordinary non-studyframe extraction makes loss of the contract explicit |

The implementation deliberately does not claim preservation through arbitrary
dplyr, data.table, or pandas operations. It does not model general relational
studies, all instruments, every selection mechanism, or causal identification.

## Next schema decisions

Expand only with a failing end-to-end example and a backend mapping or a clear
representation-only use case. Priority additions are structured calibration,
joint inclusion probabilities/PPS, richer assignment margins, unit relationships
across multiple tables, variable eligibility, and multiple imputation. Each must
specify what transformations do to the new information and how insufficiency is
reported. Preserve the distinction between facts, assumptions, and analysis policy.

## Structured analysis requirements

`analysis_request()` names the analysis kind and its design choices.
`check_analysis()` returns `ok`, `issues`, `request`, and `input_digest` without
calling outcome estimators. Survey design objects may be constructed during the
check. `run_analysis()` uses the same preparation path and fits its prepared design
once. Existing mean/effect wrappers delegate to it.

Issues have `code`, `category`, `path`, `message`, `sources`, and `remedy`. Categories
are invalid, missing, choice, unsupported, and dependency. Independent missing
outcome/design/roster requirements are reported together; design preparation can
then report a typed requirement or a backend preparation error. A numerical backend
failure during estimation remains possible and is reported as a typed issue.
Estimation raises `studyframe_analysis_error` with its issue records. Consumers must
not derive codes by parsing English messages.

Receipts retain the request, selected declarations, roster assertion, source records,
counts and omissions, assumptions, input fingerprint, and package versions. A check
is about support for the declared request; it cannot prove source assertions,
noninterference, or nonresponse assumptions. A previous check applies to its input
revision, and execution always prepares and validates the current revision again.

Result and receipt formats carry explicit `result_version` and `receipt_version`
identifiers, currently `0.3.0`. The interval convention is 95 percent Student t:
survey means use design degrees of freedom and effects use the backend's returned
degrees of freedom. For a mean's native fit, pass `df = result$analysis$design_df`
to `stats::confint()` to obtain that convention; survey's default is normal Wald.
Unpaired effects need at least two assignment units per condition in every block;
paired designs need at least two pairs. Mixed paired and larger blocks require a
separate explicit variance adapter and are not translated by this version.
