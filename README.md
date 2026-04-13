# Mouse Colony Manager

Mouse Colony Manager is an R Shiny application for maintaining a local, curator-friendly mouse colony workspace from SoftMouse exports. It keeps app-owned operational state such as notes, reservations, breeder flags, cohort labels, and pairing plans in a local SQLite database, while preserving the original upstream colony data for import refreshes.

## Highlights

- Import current SoftMouse `.xlsx` exports into a local working database.
- Optionally merge a local colony snapshot and resolve source conflicts before committing an import.
- Optionally seed or refresh breeder flags from a census `.csv` or `.xlsx`.
- Filter the colony by ID, genotype, sex, age, line, generation, live status, reservation state, cohort, and local flags.
- Save filter presets and export filtered or traced subsets to CSV.
- Trace ancestors and descendants from one or more seed mice.
- Draw pedigrees from selected mice, browser subsets, lineage results, or manual IDs, then export PNG or PDF outputs.
- Track curator-owned operational data separately from imported source fields.

## App Pages

1. `Import` loads a SoftMouse export, optional local snapshot, and optional breeder census into the working database.
2. `Dashboard` gives a quick census view and recent import activity.
3. `Colony Browser` subsets the colony with combinable filters, saved presets, and CSV export.
4. `Lineage Trace` walks ancestors and descendants from seed IDs and exports the traced result.
5. `Pedigree` renders pedigrees from selected subsets with the bundled local `ggped` renderer.
6. `Operations` stores local annotations, reservations, and pairing plans without overwriting upstream source data.

## Quick Start

### Prerequisites

- A recent version of R
- Optional: RStudio
- Chrome or Edge if you want to run the browser automation suite

### Install Dependencies

Run the setup script from the repository root:

```r
source("setup_project.R")
```

This installs the app and test dependencies, snapshots them with `renv`, and uses the bundled `ggped/` package source for pedigree rendering.

### Launch the App

```r
source("run_app.R")
```

You can also run:

```r
shiny::runApp(".")
```

## Expected Inputs

- `SoftMouse export (.xlsx)`  
  Required. This should be the latest export downloaded from SoftMouse.
- `Local colony snapshot (.xlsx)`  
  Optional. Use this to preserve local values or mice missing from the latest SoftMouse export.
- `Breeder census (.csv or .xlsx)`  
  Optional. Use this to seed or refresh breeder flags.

## Local Storage

- The working database is stored at `data/mouse_colony_manager.sqlite`.
- Imported files are archived under `data/import_archive/`.
- App-owned fields such as notes, reservations, breeder status, experiment readiness, and pairing plans are stored locally and preserved separately from imported source data.

## Testing

The repository includes both `testthat` coverage and a true browser click-through suite built with simulated colony data.

```bash
Rscript tests/testthat.R
Rscript tests/ui_clickthrough.R
```

The browser suite uses `shinytest2` and requires a locally available Chrome or Edge installation. It exercises import, operations edits, subsetting, preset save/load, lineage tracing, pedigree rendering/export, reload, and reimport stability. Current validation notes are recorded in `UI_TEST_NOTES.md`.

## Repository Layout

- `app.R` app bootstrap and dependency checks
- `run_app.R` convenience launcher
- `R/` Shiny modules, storage logic, lineage code, and pedigree rendering helpers
- `ggped/` bundled local pedigree renderer package
- `tests/testthat/` unit and integration tests
- `tests/ui_clickthrough.R` headless browser automation workflow

## License

The main application is released under the MIT License. The repository also includes a bundled `ggped/` package used for pedigree rendering; see `ggped/DESCRIPTION` for its upstream package metadata.
