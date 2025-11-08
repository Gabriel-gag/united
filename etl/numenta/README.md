ETL: Numenta Anomaly Benchmark (NAB)

Overview
- Origin: NAB (Numenta) — real and synthetic time series with labeled anomalies.
- Goal: Zip original CSVs, attach labels, prepare ungrouped source series, and publish package‑ready lists by collection.
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly").

Source Data
- Original CSVs (if present): `etl/numenta/original/<group>/`
- Labels (RDS): `etl/numenta/labels/<group>/`
- Labeled ungrouped series (RData): `etl/numenta/source/<group>/*.RData`
- Groups: `artificialWithAnomaly`, `realAdExchange`, `realAWSCloudwatch`, `realKnownCause`, `realTraffic`, `realTweets`
- NAB reference: https://github.com/numenta/NAB

Intermediate Artifacts
- Zipped raw CSVs: `etl/numenta/intermediate/zip/<group>/*.RData`
- Grouped lists (by group): `etl/numenta/intermediate/grouped/numenta_<group>.RData`
- All labeled groups combined: `etl/numenta/intermediate/grouped/numenta_grp_all.RData`

Final Data (published in package)
- `data/nab_artificialWithAnomaly.RData`
- `data/nab_realAdExchange.RData`
- `data/nab_realAWSCloudwatch.RData`
- `data/nab_realKnownCause.RData`
- `data/nab_realTraffic.RData`
- `data/nab_realTweets.RData`

ETL Code
- `1-zip-labels-and-group.R`: zip originals, attach labels, build ungrouped labeled series and grouped lists.
- `source/artificialWithAnomaly/2-build-artificialWithAnomaly.R`: build final `data/nab_artificialWithAnomaly.RData` from source RData.
- `source/realAdExchange/2-build-realAdExchange.R`: build final list.
- `source/realAWSCloudwatch/2-build-realAWSCloudwatch.R`: build final list.
- `source/realKnownCause/2-build-realKnownCause.R`: build final list.
- `source/realTraffic/2-build-realTraffic.R`: build final list.
- `source/realTweets/2-build-realTweets.R`: build final list.

Notes
- `event` is logical; `type` marks anomaly. All series include an integer `idx` column.

