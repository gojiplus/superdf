# Independent-producer fixtures

These ten bundles contain eight synthetic observations, not respondent data.
They were generated with `inst/conformance/producer.py`, `context.json`, and
`cases.json` using Python 3.13.16 and PyArrow 26.0.1. No studyframe package was
imported by the producer. Each ZIP contains Arrow Parquet tables, context JSON,
and a SHA-256 integrity manifest.

The supported assignment has treated outcomes 5, 6, 7, 8 and control outcomes
1, 2, 3, 4, giving a difference in means of 4. The negative cases intentionally
alter context while keeping valid payload hashes. Their expected structural and
analysis validity is specified in `inst/conformance/cases.json`.

To regenerate candidates in a new directory:

```sh
python inst/conformance/producer.py --cases /path/to/new-fixture-directory
```

Review the payloads and R test results before replacing these files. ZIP timestamps
and Parquet encoding may vary with the toolchain; byte equality is not the
conformance criterion. The R tests read the stored fixtures without Python.
