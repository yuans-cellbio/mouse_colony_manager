# Mouse Colony Manager App

## Summary
- Replace `CYB5R4_Colony.Rmd` as the primary workflow with a modular Shiny app and keep the Rmd only as a legacy report/export until feature parity is reached.
- Build a hybrid local master: import SoftMouse Excel exports as upstream snapshots, then store app-owned operational state locally for breeder status, experiment eligibility, reservations, notes, cohort labels, and pairing plans.
- Design local-first but shared-ready: one primary curator edits data, while other lab users mainly browse, filter, trace lineage, and export results.
- Wrap pedigree rendering behind an internal plotting API so the app is not tied to one package; target `ggpedigree` first, but keep the current patched `ggped` path as a fallback until layout and aesthetics are verified.

## Key Changes
- Extract the current hardcoded import/merge/genotype parsing logic from `CYB5R4_Colony.Rmd` into reusable data services. Remove date-specific file paths from the workflow and instead load the newest SoftMouse export plus the latest local snapshot automatically.
- Normalize one canonical mouse record schema with imported fields plus derived fields: `mouse_id`, sex, DOB, age in days/weeks, alive/ended status, breeder status, mouseline, generation, sire/dam IDs, mate ID, raw genotype text, parsed genotype components, and app-owned ops fields.
- Store persistence in SQLite rather than Excel as the working database. Keep raw import snapshots, current mouse state, local annotations, reservations, pairings, and lightweight change history so re-imports do not erase local work.
- Consolidate the overlapping lineage helpers in `helper_functions/helper_functions.R` into one query engine that supports `direction = up/down/both`, fixed generation depth or exhaustive search, and separate sibling toggles for focus mouse, ancestors, and descendants.
- Add a Shiny UI with five views: `Dashboard`, `Colony Browser`, `Lineage Trace`, `Pedigree`, and `Operations`. Use a custom `bslib` light theme, card-based layout, persistent filter drawer, and export actions for CSV/PDF/PNG.
- Dashboard content should include active mice, breeders, recent births/deaths, mice eligible for experiments, genotype distribution by line, and an action queue for items like missing parent data, aging pups, or pending reservation conflicts.
- Colony Browser should support combined filters for ID exact/contains, genotype text and parsed alleles, sex, DOB range, age range, breeder/live toggles, mouseline, generation, and local flags. Save named filter presets for common experiment queries.
- Lineage Trace should let the user enter one or more seed mice and return both a table subset and a graph-ready subset using the lineage query settings above.
- Pedigree should draw from either the current filtered subset or a lineage result, with selectable label fields and feature overlays. Export pedigree plots directly from the app.
- Operations should manage curator-owned local state: breeder designation, experiment-ready flags, reservation status, pairing plans, local notes, and cohort labels. These are local app fields, not direct SoftMouse edits in v1.

## Interfaces
- Canonical import pipeline: `import_softmouse(path) -> normalize_mouse_records() -> merge_with_local_state() -> write_snapshot()`.
- Lineage query interface: `trace_lineage(seed_ids, direction, max_generations, exhaustive, include_focus_siblings, include_ancestor_siblings, include_descendant_siblings)`.
- Pedigree interface: `build_pedigree_data(mouse_ids, label_fields, feature_fields)` and `draw_pedigree(pedigree_data, engine = c("ggpedigree", "legacy"))`.
- Experiment candidate interface: saved query presets that return a filtered table plus exportable cohort files; this replaces the current one-off “Mice ready to use” export block.

## Test Plan
- Import tests: newest SoftMouse export is detected correctly, historical local-only mice are preserved, and repeated imports do not duplicate mice.
- Genotype tests: allele parsing is stable, ordering is normalized, empty values become explicit missing values, and parsed columns match current notebook behavior.
- Lineage tests: up/down/both traversal, fixed-depth versus exhaustive search, and sibling toggles all return the expected mouse sets on known family structures.
- Persistence tests: local annotations survive re-import, reservations stay attached to the correct mouse, and curator edits do not overwrite imported source fields unintentionally.
- Pedigree tests: subsets with multiple breeders, half-siblings, and missing founders render without layout regressions; exports work from both lineage-driven and filter-driven selections.
- UX acceptance: a user can open the app, understand colony status from the dashboard, find a mouse by mixed filters, trace its lineage, draw a pedigree, and export an experiment candidate list without using the Rmd.

## Assumptions And Defaults
- Shiny is the app framework for v1 because it fits the current R codebase and is the most practical path to a later shared deployment.
- SQLite is the local working database; Excel remains an import/export format, not the operational store.
- SoftMouse remains an upstream source, but v1 does not push edits back directly.
- One curator is the primary editor; shared users are mostly read/export consumers.
- Do not build a standalone pedigree package in v1. First stabilize an internal plotting wrapper; only promote it to a package after the API and aesthetics settle.
- Standardize the R environment with `renv` early, since this shell currently does not expose `Rscript` on PATH and reproducible package setup will matter for both local and shared deployment.
