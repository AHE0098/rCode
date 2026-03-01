# SpecificR Harness (Beginner-Friendly)

This repo is a **starter workspace** you can open in **RStudio** to generate sample deck data for another app (like a Node/JS app).

If you are new to GitHub workflows, the short version is:
1. Open this folder as an RStudio Project.
2. Run one command.
3. Check files in `out/`.

---

## What this repo does

It runs one script (`scripts/specificR_run.R`) that:
- reads config settings,
- creates example deck data (or transforms your future input files),
- validates deck structure + basic quality checks,
- writes machine-readable output files to `out/`.

---

## Quick start (copy/paste)

```bash
Rscript scripts/specificR_run.R --config config/example_config.yml
```

After running, you should see:
- `out/run_manifest.json`
- `out/decks.json`
- `out/summary.csv`
- `out/qc_report.json`
- `out/run.log`

---

## If you're using RStudio (step-by-step)

1. In RStudio, click **File > Open Project...**.
2. Choose `rCode.Rproj` in this repo.
3. Open **Terminal** tab in RStudio.
4. Run:
   ```bash
   Rscript scripts/specificR_run.R --config config/example_config.yml
   ```
5. In the Files pane, open the `out/` folder and inspect outputs.

---

## Inputs and outputs

### Inputs
- Optional input files can go in `in/`.
- Runtime settings come from config file + CLI flags.

Example CLI overrides:
```bash
Rscript scripts/specificR_run.R --config config/example_config.yml --seed 1337 --n_decks 3 --strict true
```

### Outputs (stable contract)
All outputs are always written to `out/`:

- `run_manifest.json`: run metadata + list of generated files
- `decks.json`: list of decks and cards (JSON schema for Node)
- `summary.csv`: summary row per deck
- `qc_report.json`: validation warnings/errors
- `run.log`: plain text run log

---

## Testing and checks

Run tests:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

The GitHub Actions workflow also runs tests and verifies `out/run_manifest.json` is created.

---

## Reproducibility

This project includes `renv.lock` so package versions can be restored.

Typical workflow:

```r
renv::restore()
```

Then run the script again.

---

## Project layout

```text
.
├── rCode.Rproj
├── README.md
├── scripts/
│   ├── specificR_run.R
│   └── specificR_lib.R
├── config/
│   └── example_config.yml
├── in/
│   └── README.md
├── out/
│   ├── .gitkeep
│   └── README.md
├── tests/
│   ├── testthat.R
│   └── testthat/
│       └── test_specificR.R
├── renv/
│   └── settings.json
├── renv.lock
└── .github/workflows/R-CI.yml
```
