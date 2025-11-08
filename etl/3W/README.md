ETL: UCI 3W Oil Wells

Overview
- Origin: UCI Machine Learning Repository — 3W dataset (undesirable real events in oil wells).
- Goal: Standardize series per type (Type_0, 1, 2, 5, 6, 7, 8), derive event/change‑point labels, and publish package‑ready lists.
- Schema (final per series): `idx` (integer index), `value` and other sensor variables (numeric), `event` (logical), `type` ("Change Point" or empty).

Source Data
- Raw CSVs by type: `etl/3W/<type>/` (e.g., `etl/3W/5/`)
- Parquet samples for Type 3/4/9: `etl/3W/intermediate/grouped/parquet/<type>/`
- Reference: https://archive.ics.uci.edu/ml/datasets/3W+dataset

Intermediate Artifacts
- Zipped series (RData per CSV): `etl/3W/intermediate/zip/<type>/...RData`
- Ungrouped cleaned series: `etl/3W/intermediate/ungrouped/<type>/...sr.RData`
- Grouped lists: `etl/3W/intermediate/grouped/oil_3w_<Type_X>.RData`

Final Data (published in package)
- `data/oil_3w_Type_1.RData`
- `data/oil_3w_Type_2.RData`
- `data/oil_3w_Type_4.RData`
- `data/oil_3w_Type_5.RData`
- `data/oil_3w_Type_6.RData`
- `data/oil_3w_Type_7.RData`
- `data/oil_3w_Type_8.RData`

ETL Code
- `1-zip-and-group.R`: zip raw CSVs to RData; build ungrouped and grouped intermediate lists.
- `2-build-parquet-groups.R`: read parquet series (Types 3/4/9) and build grouped artifacts.
- `3-build-type-1.R`: standardize Type 1, produce final `data/oil_3w_Type_1.RData`.
- `3-build-type-2.R`: derive change points for Type 2, produce final RData.
- `3-build-type-4.R`: derive change points from class transitions, produce final RData.
- `3-build-type-5.R`: derive change points for initial series, produce final RData.
- `3-build-type-8.R`: standardize and derive change points, produce final RData.

Notes
- Variable names are normalized to lower‑case with underscores.
- Change‑point logic follows documented class transitions in originals.

