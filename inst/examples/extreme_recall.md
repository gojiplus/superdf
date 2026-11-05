# A real Qualtrics export through inference and RMCP

This example reproduces **Table 3, model 1 in the checked-in analysis code** of
`notnews/extreme_recall_2023`, source commit
`bc3a816e15d5fe2c9e3c90efde0012b9ea959628`. It does not claim an exact reproduction
of a published table: the original scoring code does not set a random seed, and
no matching manuscript result was recovered. The source CSV's January 2023
filename is stale; response timestamps place collection on January 6–7, 2024.

The model relates reverse-scaled recall position to coded ideological extremity,
with respondent-clustered standard errors. This is a selected-sample association.
The questionnaire's state/issue randomizer is retained as a separate planned
declaration, with unknown mechanism and no asserted-complete assignment roster.
It does not justify turning this regression into a causal effect.

## Run locally

Use the development studyframe build containing `regression_request()`
(version 0.3.0.9000 or its successor). Released 0.3.0 does not contain that adapter.
The source repository and its data must already be available locally. Nothing is
downloaded or uploaded by the example. The output directory must be new; keep it
in an ignored/private location because the bundles contain respondent data.

The original preprocessing requires dplyr, tidyr, stringr, readr, readxl, and
scales. The comparison additionally requires fixest and estimatr; bundle I/O uses
arrow and zip. Install missing packages with standard `install.packages()`.

From the studyframe checkout:

```r
pkgload::load_all()
source("inst/examples/extreme_recall_source.R")
source("inst/examples/extreme_recall.R")
run_extreme_recall(
  repo = "/path/to/extreme_recall_2023",
  output = "build/extreme-recall-handoff",
  seed = 1L
)
```

The helper verifies nine input hashes, then evaluates only the relevant named
assignments from the original cleaning script and the verified outcome
transformation from the results script. It skips hardcoded working-directory
changes, plotting, and unrelated models. It fixes the RNG algorithm and seed,
restores the caller's RNG state and working directory, and records package
versions. Changed source bytes require reviewing and updating the manifest.

## What travels with the data

| Artifact | Row unit and content |
|---|---|
| `imported.studyframe` | All 904 imported response rows; original import diagnostics |
| `collected.studyframe` | Same responses, source evidence, questionnaire mappings, planned assignment, unknown sampling design, and derived attention flag |
| `selected.studyframe` | 733 retained responses; all 904 collection rows remain in its attached roster, with exclusion history |
| `analysis.studyframe` | 4,398 respondent × party × recall slots; explicit compound key, measurement definitions, source hashes, and link to the selected bundle |
| `request.json` | Outcome, predictor, respondent cluster, `stata` variance correction, omission policy, association target, and assumptions |
| `receipt.json` | Model choices, package versions, input digest, source evidence, used/missing counts, and inference limitations |

The slot expansion explicitly constructs a new observational unit. Its roster is
not relabeled as a complete randomized roster. The hashed parent bundle retains
the original response universe and exclusion history. Hand off the whole output
directory to traverse that lineage: parent bundles are referenced, not embedded.
The analysis bundle alone suffices to rerun its model. Evidence locations are
references; readers do not fetch or execute them.

Generated codebooks describe collection and analysis variables. `flow.json`,
`selection.csv`, `recodes.csv`, `joins.json`, `measurement.json`, and
`source_warnings.json` retain aggregate audit results. `coefficients.csv` holds
estimates and uncertainty; `same_party_sensitivity.json` reports a separately
labeled alternative restriction. `causal_requirements.json` demonstrates refusal
of an unsupported randomized-effect request.

## Transformations and join contract

- Retain a response only when whitespace-normalized Q4.1_1 equals
  `Extremely interested` and Q4.1_2 equals `Very interested`. Of 171 excluded
  responses, 7 declined consent and 164 consenting responses lacked both required
  answers. The original code's consent filter is commented out; the example
  reproduces its actual attention rule and verifies the resulting IDs.
- Expand each retained response into six slots: Q5.1_1–3 for Democratic recall and
  Q6.1_1–3 for Republican recall. `(ResponseId, question_open)` must be unique;
  the row count must be exactly six times the selected respondent count.
- Name lookup, politician attributes, latest selected DIME scores, party lookup,
  and respondent characteristics use the original left joins. Right-side keys
  must be unique; unmatched lookups remain missing. The helper verifies all seven
  key relations and conservation of recall slots. It does not certify name-code
  accuracy. No fuzzy-linking threshold or new matching rule is introduced.
- `recall_order = (4 - recall_position) / 3`; slots 1, 2, 3 become 1, 2/3, 1/3.
  Thus the fitted first-versus-third difference is two-thirds of the slope.
- `extremity` reproduces the original proportional-coded dynamic CF score, takes
  the absolute value for Democratic prompts, and rescales the pooled observed score to 0–1.
  Missing scores stay missing. Omission applies jointly to model variables and
  cluster ID, and is counted explicitly. It does not establish ignorability.

## Observed acceptance result

Seed 1 gives 3,029 modeled slots from 698 respondents; 1,369 slots lack usable
scores. Missing-score counts for recall positions 1–3 are 173, 484, and 712.
The reverse-recall-order slope is **0.1027543**, clustered SE **0.0079602**,
95% interval **[0.0871255, 0.1183831]**, with 697 degrees of freedom.

The runner compares the adapter's coefficients and standard errors against the
original `fixest::feols` specification, and compares every coefficient, SE,
p-value, interval, and degree of freedom against a direct
[`estimatr::lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
call using `se_type = "stata"`. All comparisons use tolerance 1e-10. Data,
collection roster, and context must also survive the bundle roundtrip exactly.

Three source qualifications travel with this result:

1. Ambiguous Bush/Clinton coding uses weighted length-two permutations recycled
   within typed-name groups, not independent per-response draws. Seeds 1–4 give
   slopes 0.1027543–0.1029258. This sensitivity is not measurement-error-adjusted
   inference; the reported interval conditions on the selected coding realization.
2. The original proportional outcome bypasses a prior wrong-party mask and
   includes 77 wrong-party recalls. The primary reproduction preserves this.
   A separately reported same-party restriction, holding scaling and seed fixed,
   uses 2,952 observations and gives slope 0.0998067, SE 0.0055698.
3. DIME emits 126 parse failures in an unused field (`before.switch.ICPSR`).
   The inspected model-relevant ID, cycle, dynamic-score, and party fields have
   no parse failures. Source warnings are recorded rather than hidden.

## Use the same bundle through RMCP

With the study-tools branch of RMCP and the development R package installed in
its R library, run from the RMCP checkout:

```sh
R_LIBS=/path/to/r-library .venv/bin/python examples/analyze_study.py \
  /path/to/analysis.studyframe /path/to/request.json \
  --output /path/to/rmcp.json
```

This uses a real stdio MCP client: inspect the bundle, obtain its path/hash
reference, check the request, and run only if it passes. The tool returns
coefficient-level uncertainty and an analysis receipt, without returning the
observations or native R model. A changed bundle fails the hash check. Metadata
responses are bounded at 256 KiB and remain inline rather than entering RMCP's
shared large-result cache. Existing mean/effect tools retain their behavior.
