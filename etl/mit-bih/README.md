ETL: MIT‑BIH Arrhythmia Database (Selected Leads)

Overview
- Origin: MIT‑BIH Arrhythmia Database (downloaded RData bundle).
- Goal: Extract selected ECG leads (MLII, V1, V2, V5), standardize schema, derive event markers from annotated beats, and publish package‑ready lists.
- Schema (final per series): `idx` (integer index), `value` (numeric ECG value), `event` (logical, true where annotated beats occur), `seq` (factor beat symbol or NA), `seqlen` (window length, 50).

Source Data
- Downloaded bundle (RData) from: https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData
- Loaded transiently by the ETL script; no static raw files kept in repo.

Final Data (published in package)
- `data/mit_bih_MLII.RData`
- `data/mit_bih_V1.RData`
- `data/mit_bih_V2.RData`
- `data/mit_bih_V5.RData`

ETL Code
- `1-download-and-build.R`: downloads the bundle (URL override via env `MIT_BIH_DATA_URL`), extracts signals for each lead, converts annotations to `event`, saves lists per lead.

Notes
- Events are set to TRUE where `seq` is non‑NA (i.e., annotated beats); `type` is not used for MIT‑BIH.

