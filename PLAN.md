# R package development plan

The objective is to keep observations connected to the collection facts and
assumptions needed for a requested inference. The first product is a research
handoff that another analyst or an agent can use without reconstructing the study.
This is a development plan, not a description of completed functionality.

The current 0.2.0 implementation demonstrates primitive tables, attached rosters,
sampling/weight/assignment declarations, supported transformations, portable
bundles, and adapters for survey means and complete-outcome randomized effects.
The simulated study exercises collection through inference and repeated coverage.
It does not establish that arbitrary imported responses represent the original
sampling or assignment roster.

## Deliverables and order

| Order | Deliverable | Completion criterion |
|---|---|---|
| 1 | Roster and source-evidence semantics | A completers-only import cannot acquire a complete-assignment claim merely by becoming the reference table |
| 2 | One public analysis requirements interface | R and RMCP receive the same structured diagnostics and use the same prepared design for estimation |
| 3 | One Qualtrics import and prospective collection workflow | A real, de-identified study can be handed to a second analyst with recovered coding and explicit unresolved facts |
| 4 | A small set of familiar transformations | Partners can complete their actual preparation workflow while preservation/invalidation is tested |
| 5 | Release candidate and producer conformance kit | An independent producer can create a supported bundle and a consumer can reproduce the intended analysis |

Work on the bounded RMCP adapter can proceed now against 0.2.0; the complete
metadata improvements above are not prerequisites for testing transport. Its
initial effect route must request a sourced completeness assertion and report it
as asserted, not verified. Public adoption must not promise that this solves the
missing-roster problem.

## 1. Evolve the schema around actual evidence

Draft the next experimental schema version before implementation. Make an explicit
breaking revision; there is no requirement to maintain the old R API or migrate
unpublished bundles automatically. Keep the schema language-neutral and give R
and Python the same positive and negative conformance fixtures.

| Component | Minimum proposed semantics |
|---|---|
| Roster | Kind: received responses, invited units, sampled units, or randomized units; observational unit and stable keys; source reference; scope of any completeness assertion |
| Evidence | Immutable artifact identity/hash, producer, source location, capture time, and artifact/instrument version when available |
| Declarations | Component-level author/source and whether a value is recorded, asserted, assumed, or unknown; conflicting claims remain visible |
| Instrument mapping | Instrument version plus vendor question/subquestion identity mapped to each stable variable ID; export coding representation recorded separately |
| Disposition | Received/started/completed and other available statuses as explicit variables; do not invent unobserved invitation records |
| Missingness | Preserve source-supported distinctions between unasked, unanswered, refused, and unknown; a blank alone establishes none of them |
| Plan versus realization | Distinguish a proposed allocation/sampling protocol from the realized assignments, selected units, and measurement records |

Completeness is always scoped: all respondents is different from all invited or
randomized units. Reconciliation to an available roster can be checked; truth of
an external assertion cannot be established by a hash or an equality of row sets.
Default an ordinary dataframe import to unknown collection completeness.

Allow independent producers to contribute attributed components: a survey platform
can supply an instrument record, a panel provider recruitment and weights, and the
researcher an external assignment roster. Avoid requiring a producer to assert
facts outside its role. Start with a small source registry, not a general ontology.

Decide the bundle change explicitly. Version the manifest if adding source files;
verify their hashes, sizes, and safe archive names. A remote source pointer should
include an identity/version and be marked unavailable when it cannot be retrieved;
do not fetch arbitrary sources during analysis. Bundle validity and sufficient
evidence for a particular request remain distinct.

Acceptance cases include a complete externally randomized roster, a completers-only
CSV, a filtered delivery, two conflicting source declarations, a changed recode
with unchanged instrument text, and an instrument version mismatch. Both readers
must preserve unknown values and reject structural contradictions.

## 2. Make requirements a public R interface

Introduce a structured analysis request and a public checker, then refactor the
existing adapters around a shared prepare/check/fit path. Proposed interface names
are `analysis_request()` and `check_analysis()`; final names should follow the
existing package vocabulary. The checker must not implement a second estimator.

Return records with stable code, category, schema/variable reference, explanation,
source references, and remedy. Distinguish invalid data/context, missing facts,
analyst choices, unsupported methods, and dependency failures. Report all useful
independent requirements together. A passing check means the declared request is
supported; it does not certify randomization, nonresponse assumptions, or causal
identification. Structured R conditions should carry these records when estimation
fails, so consumers do not parse English exceptions.

The request selects an outcome, target, named sampling/assignment/weight declaration,
domain, and missing-data policy. Preserve the current separation of population
means and randomized-roster effects. Use an adapter-specific requirements table
instead of a universal metadata completeness score. For example, unknown sampling
need not prevent a separately documented randomized-roster effect.

Return a stable analysis receipt with request, selected declarations/evidence,
analyzed scope, omissions, assumptions, estimator, interval convention, degrees
of freedom, package versions, and input bundle/object identity. Row identifiers
can remain in the local object; agent summaries need counts and revision identity.

Acceptance: check and fit agree on supported requests; invalid and unsupported
requests produce stable codes; changing a relevant declaration invalidates a
previous check; no adapter silently switches to ordinary regression. Retain direct
backend numerical tests, domain tests, replicate conventions, and simulation checks.

## 3. Finish one collection workflow

Start with externally documented complete randomization, a known assignment roster,
Qualtrics outcome collection, and complete numeric outcomes. The full workflow is
roster and assignment -> instrument snapshot -> response export -> keyed left join
-> declared analysis -> portable handoff and reproducible inference.

Provide file import first: responses plus the matching QSF/instrument evidence and
optional roster. Reuse `qualtRics` for authenticated download rather than creating
another API client. Do not require API credentials for the file path. Separate the
download layer from source parsing so fixtures can exercise both.

Automatically recover available question text, coded values, stable vendor IDs,
and recorded disposition. Ask only for missing facts relevant to the selected
analysis. Provide a generated codebook and observed-response descriptions even
when population inference is unavailable. Preserve unresolved import diagnostics.

Initially support a declared set of question types, including explicit handling of
multi-column questions; refuse ambiguous mappings. Test changed recodes, duplicate
response IDs, repeat responses to one randomized unit, test/preview records, and
shuffled outcomes. Never infer refusal from blanks or complete randomization from
balanced counts. Native Qualtrics randomizers need a separate supported mapping.

For the next study, provide a collection template carrying stable study/unit IDs
and recording the actual instrument and coding versions. Returning response fields
must not overwrite authoritative assignments. Nonresponse remains on the original
roster; the existing complete-outcome effect adapter refuses it.

Completion requires a second analyst to use a real partner export, not only the
synthetic fixture. Compare its result to a reviewed direct R analysis and measure
reconstruction effort on a second study.

## 4. Fit existing R preparation workflows

Begin at import and analysis checkpoints. A pre-existing transformed table can be
declared with unknown prior lineage; do not claim recovery of history. Support a
small tested set of selection, rename, derivation/recode, and many-to-one join
operations used by pilot partners. Specify preservation versus invalidation for
each metadata component and return a new revision after changes.

Do not advertise arbitrary dplyr/data.table compatibility. Assess whether a study
wrapper with ordinary table views is easier than the current strict dataframe
subclass using the pilot workflows, before undertaking a broad redesign. Exports
must clearly describe whether the attached original roster is included; filtered
views currently include it and are not redacted deliveries.

## 5. Package and adoption gates

Ship the complete R path first. Keep the existing thin Python reader/validator
against the same schema and fixtures. RMCP is an alternate consumer of the R
implementation, not a second statistics engine. Python authoring and native backend
adapters wait for a concrete producer or analyst who needs them.

For each supported profile, publish a minimal producer example, field definitions,
positive/negative fixtures, and a validator. An external vendor should be able to
produce a compliant bundle without importing the R package. Learn from survey,
randomizr, estimatr, and DDI without claiming full DDI conformance.

Release gates: local lint/tests, R CMD check, actual R-to-Python bundle tests,
installed-package tests, package documentation, RMCP protocol tests, and one
independently repeated partner handoff. Do not treat the synthetic coverage study
as evidence of customer adoption. Test three partners doing the same narrow job;
track second-study use, setup time, analyst reconstruction effort, and support.

Defer full Python parity, enterprise metadata catalogs, general relational studies,
automatic handling of arbitrary selection/nonresponse, and extra estimators until
a concrete failing workflow justifies them. Roster evidence and usable diagnostics
come before expanding the statistical menu.
