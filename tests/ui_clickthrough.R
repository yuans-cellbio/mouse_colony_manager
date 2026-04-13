options(shinytest2.load_timeout = 120000)

library(jsonlite)
library(openxlsx)
library(readr)
library(shinytest2)
library(testthat)

workflow_source_row <- function(mouse_id,
                                sex,
                                dob,
                                age,
                                state = "Active",
                                genotype = NA_character_,
                                mouse_line = NA_character_,
                                generation = NA_character_,
                                sire_id = NA_character_,
                                dam_id = NA_character_,
                                mate_id = NA_character_,
                                protocol = NA_character_,
                                comment = NA_character_,
                                end_date = NA,
                                end_type = NA_character_,
                                alt_id = NA_character_,
                                mouse_sid = NA_character_,
                                first_gene = NA_character_,
                                second_gene = NA_character_) {
  tibble::tibble(
    Physical.Tag = mouse_id,
    Sex = sex,
    Date.of.Birth = as.Date(dob),
    End.Date = if (all(is.na(end_date))) as.Date(NA) else as.Date(end_date),
    End.Type = end_type,
    Age = age,
    State = state,
    Genotype = genotype,
    Mouseline = mouse_line,
    Generation = generation,
    Sire.Tag = sire_id,
    Dam.Tag = dam_id,
    `Mating.Partner's.Tag` = mate_id,
    Alt..ID = alt_id,
    Mouse.SID = mouse_sid,
    `1st.Gene` = first_gene,
    `2nd.Gene` = second_gene,
    Protocol = protocol,
    Comment = comment
  )
}

write_workflow_source <- function(path, df) {
  openxlsx::write.xlsx(as.data.frame(df), path, overwrite = TRUE)
  path
}

make_workflow_softmouse <- function(version = c("v1", "v2")) {
  version <- match.arg(version)

  base_rows <- dplyr::bind_rows(
    workflow_source_row("M100", "M", "2025-10-01", "28 wk", genotype = "Cyb5r4(Wt/Wt)", mouse_line = "InbredA", generation = "F0", mate_id = "F100", first_gene = "Cyb5r4"),
    workflow_source_row("F100", "F", "2025-10-01", "28 wk", genotype = "Cyb5r4(Wt/Wt)", mouse_line = "InbredA", generation = "F0", mate_id = "M100", first_gene = "Cyb5r4"),
    workflow_source_row("M110", "M", "2025-12-15", "17 wk", genotype = "Cyb5r4(Ko/Wt)", mouse_line = "InbredA", generation = "F1", sire_id = "M100", dam_id = "F100", mate_id = "F110", first_gene = "Cyb5r4"),
    workflow_source_row("F110", "F", "2025-12-15", "17 wk", genotype = "Cyb5r4(Ko/Wt)", mouse_line = "InbredA", generation = "F1", sire_id = "M100", dam_id = "F100", mate_id = "M110", first_gene = "Cyb5r4"),
    workflow_source_row("I201", "M", "2026-02-10", "9 wk", genotype = "Cyb5r4(Ko/Ko), Lysm-Cre(+/-)", mouse_line = "InbredA", generation = "F2", sire_id = "M110", dam_id = "F110", first_gene = "Cyb5r4", second_gene = "Lysm-Cre"),
    workflow_source_row("I202", "F", "2026-02-10", "9 wk", genotype = "Cyb5r4(Ko/Ko), Lysm-Cre(+/+)", mouse_line = "InbredA", generation = "F2", sire_id = "M110", dam_id = "F110", first_gene = "Cyb5r4", second_gene = "Lysm-Cre"),
    workflow_source_row("RM300", "M", "2025-11-01", "24 wk", genotype = "Cyb5r4(Wt/Wt), Ins1Cre(+/-)", mouse_line = "OutbredCross", generation = "F0", first_gene = "Cyb5r4", second_gene = "Ins1Cre"),
    workflow_source_row("RF301", "F", "2025-11-05", "23 wk", genotype = "Cyb5r4(Fl/Wt), Ins1Cre(-/-)", mouse_line = "OutbredCross", generation = "F0", first_gene = "Cyb5r4", second_gene = "Ins1Cre"),
    workflow_source_row("RF302", "F", "2025-11-07", "23 wk", genotype = "Cyb5r4(Wt/Wt), Ins1Cre(+/+)", mouse_line = "OutbredCross", generation = "F0", first_gene = "Cyb5r4", second_gene = "Ins1Cre"),
    workflow_source_row("R401", "M", "2026-02-20", "7 wk", genotype = "Cyb5r4(Fl/Wt), Ins1Cre(+/-)", mouse_line = "OutbredCross", generation = "F1", sire_id = "RM300", dam_id = "RF301", first_gene = "Cyb5r4", second_gene = "Ins1Cre"),
    workflow_source_row("R402", "F", "2026-02-22", "7 wk", genotype = "Cyb5r4(Wt/Wt), Ins1Cre(+/+)", mouse_line = "OutbredCross", generation = "F1", sire_id = "RM300", dam_id = "RF302", protocol = "SoftProtocolV1", comment = "soft export note", first_gene = "Cyb5r4", second_gene = "Ins1Cre"),
    workflow_source_row("RF500", "F", "2025-10-15", "26 wk", genotype = "Rosa26(+/-), GeneX(Wt/Wt)", mouse_line = "RotationDam", generation = "F0", first_gene = "Rosa26", second_gene = "GeneX"),
    workflow_source_row("RM501", "M", "2025-10-20", "25 wk", genotype = "Rosa26(+/+), GeneX(Ko/Wt)", mouse_line = "RotationDam", generation = "F0", first_gene = "Rosa26", second_gene = "GeneX"),
    workflow_source_row("RM502", "M", "2025-10-25", "24 wk", genotype = "Rosa26(+/-), GeneX(Fl/Wt)", mouse_line = "RotationDam", generation = "F0", first_gene = "Rosa26", second_gene = "GeneX"),
    workflow_source_row("F601", "F", "2026-02-28", "6 wk", genotype = "Rosa26(+/+), GeneX(Ko/Wt)", mouse_line = "RotationDam", generation = "F1", sire_id = "RM501", dam_id = "RF500", first_gene = "Rosa26", second_gene = "GeneX"),
    workflow_source_row("M602", "M", "2026-03-01", "6 wk", genotype = "Rosa26(+/-), GeneX(Fl/Wt)", mouse_line = "RotationDam", generation = "F1", sire_id = "RM502", dam_id = "RF500", first_gene = "Rosa26", second_gene = "GeneX")
  )

  if (identical(version, "v1")) {
    return(base_rows)
  }

  updated_rows <- base_rows |>
    dplyr::mutate(
      End.Date = dplyr::if_else(Physical.Tag == "I202", as.Date("2026-04-10"), End.Date),
      End.Type = dplyr::if_else(Physical.Tag == "I202", "Retired", End.Type),
      Age = dplyr::if_else(Physical.Tag == "I202", "8 wk", Age),
      State = dplyr::if_else(Physical.Tag == "I202", "Ended", State),
      Protocol = dplyr::if_else(Physical.Tag == "R402", "SoftProtocolV2", Protocol),
      Comment = dplyr::if_else(Physical.Tag == "R402", "soft export note v2", Comment)
    )

  dplyr::bind_rows(
    updated_rows,
    workflow_source_row(
      "R403", "M", "2026-04-01", "2 wk",
      genotype = "Cyb5r4(Wt/Wt), Ins1Cre(+/-)",
      mouse_line = "OutbredCross",
      generation = "F1",
      sire_id = "RM300",
      dam_id = "RF302",
      first_gene = "Cyb5r4",
      second_gene = "Ins1Cre"
    )
  )
}

make_workflow_snapshot <- function() {
  dplyr::bind_rows(
    workflow_source_row(
      "R402", "F", "2026-02-22", "7 wk",
      genotype = "Cyb5r4(Wt/Wt), Ins1Cre(+/+)",
      mouse_line = "OutbredCross",
      generation = "F1",
      sire_id = "RM300",
      dam_id = "RF302",
      protocol = "SnapshotProtocol",
      comment = "snapshot override note",
      first_gene = "Cyb5r4",
      second_gene = "Ins1Cre"
    ),
    workflow_source_row(
      "L900", "M", "2025-01-01", "62 wk",
      state = "Ended",
      genotype = "Cyb5r4(Wt/Wt)",
      mouse_line = "Legacy",
      generation = "F1",
      sire_id = "OLDM1",
      dam_id = "OLDF1",
      end_date = "2026-03-15",
      end_type = "Retired",
      comment = "historic local-only record",
      first_gene = "Cyb5r4"
    )
  )
}

choose_browser <- function() {
  candidates <- c(
    Sys.getenv("CHROMOTE_CHROME", unset = NA_character_),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  candidates <- unique(stats::na.omit(candidates))
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop("Could not locate Chrome or Edge for chromote.", call. = FALSE)
  }

  existing[[1]]
}

create_ui_harness <- function(repo_dir) {
  harness_dir <- file.path(tempdir(), paste0("mousecolony-ui-", format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(harness_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(harness_dir, "data"), recursive = TRUE, showWarnings = FALSE)

  copied <- file.copy(
    from = file.path(repo_dir, c("app.R", "DESCRIPTION", "R")),
    to = harness_dir,
    recursive = TRUE
  )

  if (!all(copied)) {
    stop("Failed to create the isolated UI harness.", call. = FALSE)
  }

  harness_dir
}

create_scenario_files <- function(target_dir) {
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    soft_v1 = write_workflow_source(file.path(target_dir, "soft_v1.xlsx"), make_workflow_softmouse("v1")),
    soft_v2 = write_workflow_source(file.path(target_dir, "soft_v2.xlsx"), make_workflow_softmouse("v2")),
    snapshot = write_workflow_source(file.path(target_dir, "snapshot_v1.xlsx"), make_workflow_snapshot())
  )
}

js_string <- function(x) {
  jsonlite::toJSON(unname(x), auto_unbox = TRUE)
}

log_step <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), paste(..., collapse = ""), "\n")
}

set_named_input <- function(app, id, value, wait_ = TRUE, timeout_ = 15000, allow_no_input_binding_ = FALSE) {
  args <- stats::setNames(list(value), id)
  args$wait_ <- wait_
  args$timeout_ <- timeout_
  args$allow_no_input_binding_ <- allow_no_input_binding_
  do.call(app$set_inputs, args)
}

set_selectize_input <- function(app, id, value, timeout = 30000, wait_for_server = TRUE) {
  previous_value <- tryCatch(app$get_value(input = id), error = function(...) NULL)

  app$wait_for_js(
    sprintf(
      "(function() { var el = document.getElementById(%s); return !!(el && el.selectize); })();",
      js_string(id)
    ),
    timeout = timeout
  )

  value <- as.character(value)

  if (length(value) == 0) {
    app$run_js(sprintf(
      "(function() { var el = document.getElementById(%s); if (!el || !el.selectize) { throw new Error('Selectize input not found: ' + %s); } el.selectize.clear(false); })();",
      js_string(id),
      js_string(id)
    ))
  } else {
    value_json <- jsonlite::toJSON(unname(value), auto_unbox = FALSE)
    app$run_js(sprintf(
      "(function() {
         var el = document.getElementById(%s);
         var values = %s;
         if (!el || !el.selectize) { throw new Error('Selectize input not found: ' + %s); }
         values.forEach(function(val) {
           if (!Object.prototype.hasOwnProperty.call(el.selectize.options, val)) {
             el.selectize.addOption({ value: val, text: val });
           }
         });
         el.selectize.setValue(values.length === 1 ? values[0] : values, false);
       })();",
      js_string(id),
      value_json,
      js_string(id)
    ))
  }

  if (!isTRUE(wait_for_server)) {
    Sys.sleep(0.5)
    return(invisible(NULL))
  }

  expected_json <- jsonlite::toJSON(unname(value), auto_unbox = FALSE)
  if (length(value) == 1) {
    app$wait_for_js(
      sprintf(
        "(function() { if (!window.Shiny || !Shiny.shinyapp) return false; return Shiny.shinyapp.$inputValues[%s] === %s; })();",
        js_string(id),
        js_string(value)
      ),
      timeout = timeout
    )
  } else {
    app$wait_for_js(
      sprintf(
        "(function() { if (!window.Shiny || !Shiny.shinyapp) return false; var current = Shiny.shinyapp.$inputValues[%s]; if (!Array.isArray(current)) { current = current == null ? [] : [current]; } return JSON.stringify(current) === %s; })();",
        js_string(id),
        js_string(expected_json)
      ),
      timeout = timeout
    )
  }

  tryCatch(
    app$wait_for_idle(timeout = 5000),
    error = function(...) {
      Sys.sleep(0.5)
    }
  )

  if (isTRUE(wait_for_server) && length(value) > 0 && !identical(previous_value, value)) {
    observed_value <- app$wait_for_value(
      input = id,
      ignore = list(previous_value, NULL),
      timeout = timeout
    )
    expect_equal(as.character(observed_value), value)
  } else if (!isTRUE(wait_for_server)) {
    Sys.sleep(0.5)
  }
}

upload_named_file <- function(app, id, path, wait_ = TRUE, timeout_ = 15000) {
  args <- stats::setNames(list(path), id)
  args$wait_ <- wait_
  args$timeout_ <- timeout_
  do.call(app$upload_file, args)
}

set_shiny_input <- function(app, id, value, timeout = 30000) {
  previous_value <- tryCatch(app$get_value(input = id), error = function(...) NULL)
  payload <- if (length(value) == 1) {
    js_string(as.character(value))
  } else {
    jsonlite::toJSON(unname(as.character(value)), auto_unbox = FALSE)
  }

  app$run_js(sprintf(
    "(function() { if (!window.Shiny || !Shiny.setInputValue) { throw new Error('Shiny.setInputValue is not available.'); } Shiny.setInputValue(%s, %s, { priority: 'event' }); })();",
    js_string(id),
    payload
  ))

  if (length(value) == 0) {
    Sys.sleep(0.5)
    return(invisible(NULL))
  }

  if (!identical(previous_value, value)) {
    observed_value <- app$wait_for_value(
      input = id,
      ignore = list(previous_value, NULL),
      timeout = timeout
    )
    expect_equal(as.character(observed_value), as.character(value))
  }
}

wait_for_text <- function(app, selector, pattern, timeout = 30000, fixed = TRUE) {
  deadline <- Sys.time() + timeout / 1000

  repeat {
    text_value <- tryCatch(app$get_text(selector), error = function(...) "")
    matched <- if (fixed) grepl(pattern, text_value, fixed = TRUE) else grepl(pattern, text_value, perl = TRUE)

    if (isTRUE(matched)) {
      return(invisible(text_value))
    }

    if (Sys.time() > deadline) {
      stop(sprintf("Timed out waiting for text '%s' in selector '%s'. Last text was: %s", pattern, selector, text_value), call. = FALSE)
    }

    Sys.sleep(0.5)
  }
}

wait_for_selector <- function(app, selector, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      "(function() { var el = document.querySelector(%s); return !!(el && (el.offsetParent !== null || ['INPUT','SELECT','TEXTAREA'].includes(el.tagName))); })();",
      js_string(selector)
    ),
    timeout = timeout
  )
}

click_nav <- function(app, label, wait_for = NULL, timeout = 30000) {
  app$run_js(sprintf(
    "(function() { var label = %s; var link = Array.from(document.querySelectorAll('a')).find(function(el) { return (el.textContent || '').trim() === label; }); if (!link) { throw new Error('Navigation link not found: ' + label); } link.click(); })();",
    js_string(label)
  ))
  app$wait_for_idle(timeout = timeout)

  if (!is.null(wait_for)) {
    wait_for_selector(app, wait_for, timeout = timeout)
  }
}

click_row <- function(app, selector, timeout = 30000) {
  wait_for_selector(app, selector, timeout = timeout)
  app$click(selector = selector)
  app$wait_for_idle(timeout = timeout)
}

click_text_match <- function(app, selector, pattern, timeout = 30000) {
  app$run_js(sprintf(
    "(function() { var pattern = %s; var nodes = Array.from(document.querySelectorAll(%s)); var node = nodes.find(function(el) { return (el.textContent || '').indexOf(pattern) >= 0; }); if (!node) { throw new Error('Could not find text match: ' + pattern); } node.click(); })();",
    js_string(pattern),
    js_string(selector)
  ))
  app$wait_for_idle(timeout = timeout)
}

wait_for_option <- function(app, selector, value, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      "(function() { var sel = document.querySelector(%s); if (!sel) return false; return Array.from(sel.querySelectorAll('option')).some(function(opt) { return opt.value === %s; }); })();",
      js_string(selector),
      js_string(value)
    ),
    timeout = timeout
  )
}

wait_for_plot <- function(app, timeout = 60000) {
  app$wait_for_js(
    "(function() { var root = document.getElementById('pedigree-plot'); if (!root) return false; return !!root.querySelector('img, canvas'); })();",
    timeout = timeout
  )
}

table_text <- function(app, selector) {
  app$get_text(selector)
}

main <- function() {
  repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  harness_dir <- create_ui_harness(repo_dir)
  scenario_files <- create_scenario_files(file.path(harness_dir, "scenario"))
  artifacts_dir <- file.path(harness_dir, "artifacts")
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(CHROMOTE_CHROME = choose_browser(), NOT_CRAN = "true")

  log_step("Launching isolated headless app harness at ", harness_dir)
  app <- shinytest2::AppDriver$new(app_dir = harness_dir, name = "mouse-colony-ui", load_timeout = 120000, timeout = 15000)
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(1600, 1200)

  wait_for_text(app, ".app-status-text", "Working database is empty", timeout = 30000)

  log_step("Previewing first import with simulated SoftMouse + snapshot files")
  upload_named_file(app, "import-softmouse_file", scenario_files$soft_v1)
  upload_named_file(app, "import-snapshot_file", scenario_files$snapshot)
  app$click("import-preview_import")
  wait_for_text(app, ".app-status-text", "Import preview ready.", timeout = 60000)
  wait_for_text(app, "#import-conflicts", "protocol", timeout = 30000)
  wait_for_text(app, "#import-conflicts", "source_comment", timeout = 30000)
  wait_for_text(app, "#import-missing_parents", "L900", timeout = 30000)

  log_step("Completing first import")
  app$click("import-complete_import")
  wait_for_text(app, ".app-status-text", "Import complete.", timeout = 90000)
  wait_for_text(app, "#import-imports", "soft_v1.xlsx", timeout = 30000)
  app$get_screenshot(file = file.path(artifacts_dir, "01-import-complete.png"))

  log_step("Checking dashboard metrics after import")
  click_nav(app, "Dashboard")
  wait_for_text(app, "#dashboard-metric_active", "16", timeout = 30000)
  wait_for_text(app, "#dashboard-metric_ended", "1", timeout = 30000)

  log_step("Editing operational records and creating rotating pairings through the Operations page")
  click_nav(app, "Operations", wait_for = "#operations-mouse_id")
  set_selectize_input(app, "operations-mouse_id", "M110")
  set_named_input(app, "operations-is_breeder", TRUE)
  set_named_input(app, "operations-cohort_label", "InbredCore")
  set_named_input(app, "operations-local_flags", "inbred sire")
  app$click("operations-save_annotation")
  wait_for_text(app, ".app-status-text", "Annotation saved for M110", timeout = 30000)

  set_selectize_input(app, "operations-mouse_id", "F110")
  set_named_input(app, "operations-is_breeder", TRUE)
  set_named_input(app, "operations-cohort_label", "InbredCore")
  set_named_input(app, "operations-local_flags", "inbred dam")
  app$click("operations-save_annotation")
  wait_for_text(app, ".app-status-text", "Annotation saved for F110", timeout = 30000)

  set_selectize_input(app, "operations-mouse_id", "I201")
  set_named_input(app, "operations-experiment_ready", TRUE)
  set_named_input(app, "operations-cohort_label", "InbredCore")
  set_named_input(app, "operations-local_flags", "inbred hold")
  set_named_input(app, "operations-notes", "Priority pedigree candidate")
  app$click("operations-save_annotation")
  wait_for_text(app, ".app-status-text", "Annotation saved for I201", timeout = 30000)

  set_selectize_input(app, "operations-mouse_id", "I202")
  set_named_input(app, "operations-experiment_ready", TRUE)
  set_named_input(app, "operations-cohort_label", "InbredCore")
  set_named_input(app, "operations-local_flags", "inbred litter")
  app$click("operations-save_annotation")
  wait_for_text(app, ".app-status-text", "Annotation saved for I202", timeout = 30000)

  set_selectize_input(app, "operations-mouse_id", "I201")
  set_named_input(app, "operations-project_name", "Inbred stability study")
  set_named_input(app, "operations-reserved_by", "QA")
  set_named_input(app, "operations-reserved_until", as.Date("2026-04-30"))
  set_named_input(app, "operations-reservation_status", "hold")
  set_named_input(app, "operations-reservation_notes", "Retain for pedigree validation")
  app$click("operations-save_reservation")
  wait_for_text(app, ".app-status-text", "Reservation saved for I201", timeout = 30000)

  pairing_inputs <- list(
    list(sire = "M110", dam = "F110", date = as.Date("2026-04-10"), status = "active", notes = "Sibling breeding scenario"),
    list(sire = "RM300", dam = "RF301", date = as.Date("2026-04-11"), status = "active", notes = "Rotating sire scenario A"),
    list(sire = "RM300", dam = "RF302", date = as.Date("2026-04-12"), status = "active", notes = "Rotating sire scenario B"),
    list(sire = "RM501", dam = "RF500", date = as.Date("2026-04-12"), status = "planned", notes = "Rotating dam scenario A"),
    list(sire = "RM502", dam = "RF500", date = as.Date("2026-04-13"), status = "active", notes = "Rotating dam scenario B")
  )

  for (entry in pairing_inputs) {
    set_shiny_input(app, "operations-sire_id", entry$sire)
    set_shiny_input(app, "operations-dam_id", entry$dam)
    set_named_input(app, "operations-planned_on", entry$date, wait_ = FALSE)
    set_named_input(app, "operations-pair_status", entry$status, wait_ = FALSE)
    set_named_input(app, "operations-pair_notes", entry$notes, wait_ = FALSE)
    app$click("operations-save_pairing")
    wait_for_text(app, ".app-status-text", "Pairing plan saved.", timeout = 30000)
  }

  wait_for_text(app, "#operations-pairings", "RM300::RF301", timeout = 30000)
  wait_for_text(app, "#operations-pairings", "RM300::RF302", timeout = 30000)
  wait_for_text(app, "#operations-pairings", "RM502::RF500", timeout = 30000)

  log_step("Applying browser filters for related inbred mice and saving a preset")
  click_nav(app, "Colony Browser", wait_for = "#browser-id_query")
  set_shiny_input(app, "browser-id_query", "I20")
  set_shiny_input(app, "browser-id_mode", "contains")
  set_shiny_input(app, "browser-genotype_query", "Lysm-Cre")
  set_shiny_input(app, "browser-flag_query", "inbred")
  set_shiny_input(app, "browser-alive_filter", "live")
  set_shiny_input(app, "browser-reservation_filter", "hold")
  app$click("browser-apply_filters")
  wait_for_text(app, ".app-status-text", "Applied colony filters.", timeout = 30000)
  wait_for_text(app, "#browser-table", "I201", timeout = 30000)

  browser_table_text <- table_text(app, "#browser-table")
  expect_true(grepl("I201", browser_table_text, fixed = TRUE))
  expect_false(grepl("I202", browser_table_text, fixed = TRUE))

  click_row(app, "#browser-table table tbody tr:nth-child(1)")

  set_named_input(app, "browser-preset_name", "ui-inbred-hold")
  app$click("browser-save_preset")
  wait_for_text(app, ".app-status-text", "Saved preset ui-inbred-hold", timeout = 30000)
  app$get_screenshot(file = file.path(artifacts_dir, "02-browser-filtered.png"))

  filtered_csv <- app$get_download("browser-download_csv", filename = "filtered_colony.csv")
  filtered_data <- readr::read_csv(filtered_csv, show_col_types = FALSE)
  expect_equal(filtered_data$mouse_id, "I201")

  log_step("Tracing lineage from the inbred offspring")
  click_nav(app, "Lineage Trace", wait_for = "#lineage-seed_ids")
  set_named_input(app, "lineage-seed_ids", "I201", wait_ = FALSE)
  set_named_input(app, "lineage-direction", "both", wait_ = FALSE)
  set_named_input(app, "lineage-generations", 2, wait_ = FALSE)
  set_named_input(app, "lineage-focus_siblings", TRUE, wait_ = FALSE)
  app$click("lineage-run_lineage")
  wait_for_text(app, ".app-status-text", "Lineage trace complete.", timeout = 30000)
  lineage_table_text <- table_text(app, "#lineage-table")
  expect_true(grepl("M100", lineage_table_text, fixed = TRUE))
  expect_true(grepl("F100", lineage_table_text, fixed = TRUE))
  expect_true(grepl("I202", lineage_table_text, fixed = TRUE))

  lineage_csv <- app$get_download("lineage-download_csv", filename = "lineage_trace.csv")
  lineage_data <- readr::read_csv(lineage_csv, show_col_types = FALSE)
  expect_true(all(c("M100", "F100", "M110", "F110", "I201", "I202") %in% lineage_data$mouse_id))

  log_step("Drawing a pedigree from the filtered browser subset")
  click_nav(app, "Pedigree", wait_for = "#pedigree-draw")
  set_named_input(app, "pedigree-source", "lineage", wait_ = FALSE)
  set_named_input(app, "pedigree-label_fields", c("mouse_id", "sex", "cohort_label"), wait_ = FALSE)
  set_shiny_input(app, "pedigree-feature_fields", "geno_cyb5r4")
  set_named_input(app, "pedigree-canvas_mode", "manual", wait_ = FALSE)
  set_named_input(app, "pedigree-canvas_width_in", 30, wait_ = FALSE)
  set_named_input(app, "pedigree-canvas_height_in", 16, wait_ = FALSE)
  app$click("pedigree-draw")
  wait_for_text(app, ".app-status-text", "Pedigree ready with the installed ggped renderer.", timeout = 60000)
  wait_for_plot(app, timeout = 60000)
  wait_for_text(app, "#pedigree-subset_table", "I201", timeout = 30000)
  app$get_screenshot(file = file.path(artifacts_dir, "03-pedigree.png"))

  pedigree_png <- app$get_download("pedigree-download_png", filename = "pedigree.png")
  expect_true(file.exists(pedigree_png))
  expect_gt(file.info(pedigree_png)$size, 0)

  log_step("Reloading the app to verify persistence")
  app$stop()

  app <- shinytest2::AppDriver$new(app_dir = harness_dir, name = "mouse-colony-ui-reload", load_timeout = 120000, timeout = 15000)
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(1600, 1200)

  click_nav(app, "Operations", wait_for = "#operations-mouse_id")
  set_selectize_input(app, "operations-mouse_id", "I201")
  wait_for_text(app, "#operations-reservations", "Inbred stability study", timeout = 30000)
  wait_for_text(app, "#operations-pairings", "RM300::RF301", timeout = 30000)
  wait_for_text(app, "#operations-pairings", "RM300::RF302", timeout = 30000)

  log_step("Running a second import to verify reimport stability and retained local state")
  click_nav(app, "Import", wait_for = "#import-softmouse_file")
  upload_named_file(app, "import-softmouse_file", scenario_files$soft_v2)
  upload_named_file(app, "import-snapshot_file", scenario_files$snapshot)
  app$click("import-preview_import")
  wait_for_text(app, ".app-status-text", "Import preview ready.", timeout = 60000)
  app$click("import-complete_import")
  wait_for_text(app, ".app-status-text", "Import complete.", timeout = 90000)
  wait_for_text(app, "#import-imports", "soft_v2.xlsx", timeout = 30000)

  click_nav(app, "Colony Browser", wait_for = "#browser-id_query")
  set_named_input(app, "browser-load_preset", "ui-inbred-hold")
  app$click("browser-apply_preset")
  wait_for_text(app, ".app-status-text", "Preset loaded. Click Apply Filters to update the table.", timeout = 30000)
  app$click("browser-apply_filters")
  wait_for_text(app, ".app-status-text", "Applied colony filters.", timeout = 30000)
  wait_for_text(app, "#browser-table", "I201", timeout = 30000)

  app$click("browser-clear_filters")
  wait_for_text(app, ".app-status-text", "Filter inputs cleared. Click Apply Filters to refresh the subset.", timeout = 30000)
  set_shiny_input(app, "browser-id_query", "R403")
  set_shiny_input(app, "browser-id_mode", "exact")
  set_shiny_input(app, "browser-alive_filter", "all")
  app$click("browser-apply_filters")
  wait_for_text(app, "#browser-table", "R403", timeout = 30000)

  app$click("browser-clear_filters")
  wait_for_text(app, ".app-status-text", "Filter inputs cleared. Click Apply Filters to refresh the subset.", timeout = 30000)
  set_shiny_input(app, "browser-id_query", "I202")
  set_shiny_input(app, "browser-id_mode", "exact")
  set_shiny_input(app, "browser-alive_filter", "ended")
  app$click("browser-apply_filters")
  wait_for_text(app, "#browser-table", "I202", timeout = 30000)

  click_nav(app, "Operations", wait_for = "#operations-mouse_id")
  set_selectize_input(app, "operations-mouse_id", "I201")
  wait_for_text(app, "#operations-reservations", "Inbred stability study", timeout = 30000)

  log_step("UI click-through completed successfully")
  cat("\nArtifacts saved under:\n", artifacts_dir, "\n", sep = "")
}

main()
