UI Test Notes

Run date: 2026-04-13

Commands

- `Rscript tests/testthat.R`
- `Rscript tests/ui_clickthrough.R`

Status

- Unit/integration suite: passing (`119` tests)
- Headless browser click-through: passing

Resolved During This Pass

- Operations page selectize refresh logic was causing reactive churn and unreliable saves while switching mice. The observer now refreshes choices only when data or external selection changes, which stabilized annotation and reservation saves during browser automation.

Open Follow-Up

- On initial app load, the browser console still reports one Shiny client error:
  `Uncaught TypeError: Cannot read properties of undefined (reading 'value')`
  This appears during startup before manual interaction and did not block import, filtering, lineage tracing, pedigree drawing, save/reload, or reimport. It should be traced to one of the startup input update paths on hidden tabs.

Notes

- The click-through automation script creates an isolated temp harness and uses simulated SoftMouse/snapshot files to exercise import, operations edits, browser filtering, lineage tracing, pedigree drawing/export, reload, and reimport persistence.
- Rerunning `tests/ui_clickthrough.R` will save screenshots and downloaded artifacts under a temp `artifacts` directory printed at the end of the run.
