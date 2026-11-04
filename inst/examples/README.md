# Simulated collection through inference

`simulated_study.R` exercises actual stochastic collection, rather than filling a
table with fixed example values. Load studyframe and source this file, then run
`run_simulated_study("simulation", repetitions = 500L)` with a new output directory.
For development, use `pkgload::load_all()` and source the file from the checkout.

## Design

1. Generate a finite population of 320 pupils in 80 equally sized schools, split
   between two districts. Baseline scores include district, school, and pupil
   variation. Define both potential follow-up outcomes, with a constant treatment
   effect of two points. Hold this population fixed across repetitions.
2. Sample 12 of 40 schools without replacement in each district and include all
   four pupils in each selected school. Record inclusion probabilities, inverse
   probability weights, strata, and school population sizes for the FPC.
3. Randomly assign six of the twelve sampled schools in each district to treatment.
   Record the randomized roster and its assignment design before attaching outcomes.
4. Collect baseline and realized follow-up outcomes for the 96 sampled pupils.
   Shuffle their arrival order and join by pupil ID. Unobserved potential outcomes
   and unsampled pupils remain outside the analysis bundle.
5. Export and reload the data and context together. Every Monte Carlo repetition
   takes this serialization path before estimation.
6. Estimate the population baseline mean using `survey`, and the treatment effect
   among randomized pupils using `estimatr`. Save estimates, standard errors,
   95% intervals, and the selected analysis context.

The survey interval uses a t distribution with design degrees of freedom (22),
as described in the [survey documentation](https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/surveysummary.html).
The effect interval comes from
[`estimatr::difference_in_means`](https://declaredesign.org/r/estimatr/reference/difference_in_means.html).
Sampling and assignment coexist in the schema but serve separate analyses. The
effect interval is conditional on the randomized roster; it is not an adapter for
general population treatment effects combining sampling and assignment uncertainty.
With the constant effect used here, the roster effect is two in every repetition.

## Outputs and interpretation

The output directory contains a reusable `collected.studyframe` bundle, a generated
`codebook.md`, the single-run estimates in `single_run.csv`, and `analysis.rds`
containing fits and context. `replications.csv` contains every repeated estimate;
`summary.csv` reports bias, its Monte Carlo standard error, empirical standard
deviation, root-mean-square reported standard error, coverage, and its Monte Carlo
standard error. Population seed is 812, demonstration seed is 9001, and repetition
seeds start at 10001. There is no search for favorable seeds.

This tests one supported design with full response, equal school sizes, additive
effects, and a fixed population. Coverage estimates have simulation error; 500
repetitions give roughly one percentage point of standard error near 95% coverage.
This is evidence that the pipeline behaves correctly in this case, not validation
of every design expressible in the schema. Existing tests separately check refusal
of incomplete follow-up and loss of randomized units; the simulation does not
claim to solve nonresponse or attrition.

## Observed validation run

On 2026-11-04, 500 repetitions using R 4.6.0, survey 4.5, and estimatr 1.0.6 gave:

| Target | Truth | Mean estimate | Bias (Monte Carlo SE) | Empirical SD | RMS reported SE | 95% coverage (Monte Carlo SE) |
|---|---:|---:|---:|---:|---:|---:|
| Population baseline mean | 57.2028 | 57.2468 | 0.0441 (0.0329) | 0.7365 | 0.7705 | 94.6% (1.01 percentage points) |
| Randomized-roster effect | 2.0000 | 2.1274 | 0.1274 (0.0845) | 1.8897 | 1.9045 | 96.0% (0.88 percentage points) |

Both observed biases are within two Monte Carlo standard errors of zero; coverage
is consistent with 95% at this simulation precision. This does not establish exact
unbiasedness or exact coverage. The single demonstration draw estimated an effect
of 0.1487 with interval [-3.6657, 3.9630], illustrating the uncertainty of one small
cluster experiment even when its generating effect is known to the simulator.
Potential outcomes and that known truth are available only to the simulation's
scoring code, not to either estimation adapter.
