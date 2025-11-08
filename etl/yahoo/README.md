ETL: Yahoo Webscope S5 (A1–A4)

Overview
- Origin: Yahoo Webscope S5 labeled anomaly detection dataset (A1–A4).
- Goal: Parse CSVs, standardize schema, and publish package‑ready lists for each benchmark.
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly" or "changepoint" when applicable in A3/A4).

Source Data
- CSV folders: `etl/yahoo/source/A1Benchmark`, `A2Benchmark`, `A3Benchmark`, `A4Benchmark`
- Webscope info: https://webscope.sandbox.yahoo.com/catalog.php?datatype=s

Final Data (published in package)
- `data/A1Benchmark.RData`
- `data/A2Benchmark.RData`
- `data/A3Benchmark.RData`
- `data/A4Benchmark.RData`

ETL Code
- `1-build-yahoo-benchmarks.R`:
  - A1/A2: load 3‑column CSVs (idx,value,event) and standardize.
  - A3/A4: load 4‑column CSVs (idx,value,anomaly,cp), fold into `event` + `type`.
  - Saves final RData files listed above.

Notes
- Event labels are logical; `type` marks anomaly vs. change‑point when provided by S5.

