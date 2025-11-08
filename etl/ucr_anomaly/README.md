ETL: UCR Time Series Anomaly Archive

Overview
- Origin: UCR Time Series Anomaly Archive — labeled anomalous intervals across multiple domains.
- Goal: Optionally zip original TXT files, prepare grouped index of series, and build package‑ready lists for selected domains (ECG, NASA, Internal Bleeding, Power Demand).
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly").

Source Data
- Labeled RData (per domain): `etl/ucr_anomaly/source/ucr_ecg.RData`, `ucr_nasa.RData`, `ucr_int_bleeding.RData`, `ucr_power_demand.RData`
- (Optional) Original TXT: `etl/ucr_anomaly/original/` (if present; used by zipping step)
- Archive info: https://www.cs.ucr.edu/~eamonn/discords/ — and catalog mirrors (see Papers With Code dataset page)

Intermediate Artifacts
- Zipped originals: `etl/ucr_anomaly/intermediate/zip/*.RData`
- Grouped index of all zipped series: `etl/ucr_anomaly/intermediate/grouped/ucr.RData`

Final Data (published in package)
- `data/ucr_ecg.RData`
- `data/ucr_nasa.RData`
- `data/ucr_int_bleeding.RData`
- `data/ucr_power_demand.RData`

ETL Code
- `1-zip-and-group.R`: zip original TXT files to RData and build grouped index.
- `2-build-ecg.R`: build final ECG list from labeled source RData.
- `3-build-internal_bleeding.R`: build final Internal Bleeding list.
- `4-build-nasa.R`: build final NASA list.
- `5-build-power_demand.R`: build final Power Demand list.

Notes
- Each final list entry has `idx`, `value`, `event`, and `type` fields.

