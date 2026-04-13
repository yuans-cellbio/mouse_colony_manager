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

metric_value <- function(metrics, metric_name) {
  metrics$value[metrics$metric == metric_name][[1]]
}

seed_workflow_operations <- function(db_path) {
  save_annotation(db_path, "M110", is_breeder = TRUE, cohort_label = "InbredCore", local_flags = "inbred sire")
  save_annotation(db_path, "F110", is_breeder = TRUE, cohort_label = "InbredCore", local_flags = "inbred dam")
  save_annotation(db_path, "I201", experiment_ready = TRUE, cohort_label = "InbredCore", local_flags = "inbred hold", notes = "Priority pedigree candidate")
  save_annotation(db_path, "I202", experiment_ready = TRUE, cohort_label = "InbredCore", local_flags = "inbred litter")
  save_annotation(db_path, "RM300", is_breeder = TRUE, cohort_label = "OutbredRotation", local_flags = "rotating sire")
  save_annotation(db_path, "RF301", is_breeder = TRUE, experiment_ready = TRUE, cohort_label = "OutbredRotation", local_flags = "rotating female")
  save_annotation(db_path, "RF302", is_breeder = TRUE, experiment_ready = TRUE, cohort_label = "OutbredRotation", local_flags = "rotating female")
  save_annotation(db_path, "RF500", is_breeder = TRUE, cohort_label = "RotationDam", local_flags = "rotating female")
  save_annotation(db_path, "RM501", is_breeder = TRUE, cohort_label = "RotationDam", local_flags = "rotating sire")
  save_annotation(db_path, "RM502", is_breeder = TRUE, cohort_label = "RotationDam", local_flags = "rotating sire")

  save_reservation(
    db_path, "I201",
    project_name = "Inbred stability study",
    reserved_by = "QA",
    reserved_until = as.Date("2026-04-30"),
    status = "hold",
    notes = "Retain for pedigree validation"
  )
  save_reservation(
    db_path, "R401",
    project_name = "Outbred cohort",
    reserved_by = "QA",
    reserved_until = as.Date("2026-05-05"),
    status = "reserved",
    notes = "Reserve rotating sire offspring"
  )

  save_pairing(db_path, "M110", "F110", planned_on = as.Date("2026-04-10"), status = "active", notes = "Sibling breeding scenario")
  save_pairing(db_path, "RM300", "RF301", planned_on = as.Date("2026-04-11"), status = "active", notes = "Rotating sire scenario A")
  save_pairing(db_path, "RM300", "RF302", planned_on = as.Date("2026-04-12"), status = "active", notes = "Rotating sire scenario B")
  save_pairing(db_path, "RM501", "RF500", planned_on = as.Date("2026-04-12"), status = "planned", notes = "Rotating dam scenario A")
  save_pairing(db_path, "RM502", "RF500", planned_on = as.Date("2026-04-13"), status = "active", notes = "Rotating dam scenario B")
}

create_workflow_fixture <- function() {
  temp_dir <- tempfile("mousecolony-workflow-")
  dir.create(temp_dir)

  db_path <- file.path(temp_dir, "colony.sqlite")
  soft_v1_path <- write_workflow_source(file.path(temp_dir, "soft_v1.xlsx"), make_workflow_softmouse("v1"))
  soft_v2_path <- write_workflow_source(file.path(temp_dir, "soft_v2.xlsx"), make_workflow_softmouse("v2"))
  snapshot_path <- write_workflow_source(file.path(temp_dir, "snapshot_v1.xlsx"), make_workflow_snapshot())

  preview <- preview_manual_import(db_path, soft_v1_path, snapshot_path)
  resolutions <- tibble::tibble(
    mouse_id = c("R402", "R402"),
    field = c("protocol", "source_comment"),
    chosen_source = c("local_snapshot", "local_snapshot")
  )

  result <- run_manual_import(
    db_path = db_path,
    softmouse_path = soft_v1_path,
    snapshot_path = snapshot_path,
    conflict_resolutions = resolutions
  )

  list(
    temp_dir = temp_dir,
    db_path = db_path,
    soft_v1_path = soft_v1_path,
    soft_v2_path = soft_v2_path,
    snapshot_path = snapshot_path,
    preview = preview,
    import_result = result,
    resolutions = resolutions
  )
}

test_that("operations module saves cleanly and rotating pairings do not duplicate mice", {
  fixture <- create_workflow_fixture()

  refresh_nonce <- shiny::reactiveVal(Sys.time())
  data_r <- shiny::reactive({
    refresh_nonce()
    load_current_colony(fixture$db_path)
  })
  metadata_r <- shiny::reactive({
    refresh_nonce()
    get_colony_metadata(fixture$db_path)
  })
  selected_ids_rv <- shiny::reactiveVal("I201")
  refresh_data <- function() refresh_nonce(Sys.time())

  status_state <- new.env(parent = emptyenv())
  status_state$message <- NULL
  status_state$kind <- NULL
  set_status <- function(message, kind = "info") {
    status_state$message <- message
    status_state$kind <- kind
  }

  expect_no_warning(
    shiny::testServer(
      mod_operations_server,
      args = list(
        data = data_r,
        metadata = metadata_r,
        db_path = fixture$db_path,
        selected_ids_rv = selected_ids_rv,
        refresh_data = refresh_data,
        set_status = set_status
      ),
      {
        session$setInputs(mouse_id = "I201")
        session$setInputs(
          is_breeder = FALSE,
          experiment_ready = TRUE,
          cohort_label = "InbredCore",
          local_flags = "inbred hold",
          notes = "Priority pedigree candidate"
        )
        session$setInputs(save_annotation = 1)

        session$setInputs(
          project_name = "Inbred stability study",
          reserved_by = "QA",
          reserved_until = as.Date("2026-04-30"),
          reservation_status = "hold",
          reservation_notes = "Retain for pedigree validation"
        )
        session$setInputs(save_reservation = 1)

        session$setInputs(
          sire_id = "RM300",
          dam_id = "RF301",
          planned_on = as.Date("2026-04-11"),
          pair_status = "active",
          pair_notes = "Rotating sire scenario A"
        )
        session$setInputs(save_pairing = 1)
      }
    )
  )

  seed_workflow_operations(fixture$db_path)
  colony <- load_current_colony(fixture$db_path)

  expect_equal(nrow(colony), length(unique(colony$mouse_id)))
  expect_true(colony$experiment_ready[colony$mouse_id == "I201"])
  expect_equal(colony$reservation_status[colony$mouse_id == "I201"], "hold")
  expect_equal(colony$active_pairing_role[colony$mouse_id == "RM300"], "sire")
  expect_equal(colony$active_pairing_with[colony$mouse_id == "RM300"], "RF301, RF302")
  expect_equal(colony$active_pairing_role[colony$mouse_id == "RF500"], "dam")
  expect_equal(colony$active_pairing_with[colony$mouse_id == "RF500"], "RM501, RM502")
  expect_equal(status_state$kind, "success")
  expect_match(status_state$message, "Pairing plan saved", fixed = TRUE)
})

test_that("simulated breeding workflows preserve state across import, filters, lineage, and reimport", {
  fixture <- create_workflow_fixture()
  seed_workflow_operations(fixture$db_path)

  expect_equal(metric_value(fixture$preview$metrics, "Source conflicts"), 2)
  expect_equal(metric_value(fixture$preview$metrics, "Missing parent links"), 2)
  expect_equal(sort(fixture$preview$conflicts$field), c("protocol", "source_comment"))

  colony <- load_current_colony(fixture$db_path)

  pedigree_filters <- list(
    mouse_line = "InbredA",
    cohort_filter = "InbredCore",
    alive_filter = "live"
  )
  expect_true(save_filter_preset(fixture$db_path, "inbred-pedigree", pedigree_filters))

  loaded_preset <- load_filter_preset(fixture$db_path, "inbred-pedigree")
  expect_equal(loaded_preset$mouse_line, "InbredA")
  expect_equal(loaded_preset$cohort_filter, "InbredCore")
  expect_equal(loaded_preset$alive_filter, "live")

  rotating_females <- apply_mouse_filters(colony, list(
    sex = "F",
    mouse_line = "OutbredCross",
    alive_filter = "live",
    breeder_filter = "yes",
    ready_filter = "yes"
  ))
  expect_equal(sort(rotating_females$mouse_id), c("RF301", "RF302"))

  inbred_hold_subset <- apply_mouse_filters(colony, list(
    genotype_gene = "geno_cyb5r4",
    genotype_value = "Ko/Ko",
    cohort_filter = "InbredCore",
    reservation_filter = "hold",
    flag_query = "inbred"
  ))
  expect_equal(inbred_hold_subset$mouse_id, "I201")

  lineage <- trace_lineage(
    seed_ids = "I201",
    data = colony,
    direction = "both",
    max_generations = 2,
    include_focus_siblings = TRUE
  )
  expect_true(all(c("M100", "F100", "M110", "F110", "I201", "I202") %in% lineage$ids))

  run_manual_import(
    db_path = fixture$db_path,
    softmouse_path = fixture$soft_v2_path,
    snapshot_path = fixture$snapshot_path,
    conflict_resolutions = fixture$resolutions
  )

  reloaded <- load_current_colony(fixture$db_path)

  expect_true("R403" %in% reloaded$mouse_id)
  expect_false(reloaded$alive[reloaded$mouse_id == "I202"])
  expect_equal(reloaded$reservation_status[reloaded$mouse_id == "I201"], "hold")
  expect_equal(reloaded$cohort_label[reloaded$mouse_id == "M110"], "InbredCore")
  expect_equal(reloaded$active_pairing_with[reloaded$mouse_id == "RM300"], "RF301, RF302")
})

test_that("filtered breeding subsets can be drawn as pedigrees with scenario data", {
  skip_if_not(local_ggped_dependencies_available(), "The installed ggped package is not available.")

  fixture <- create_workflow_fixture()
  seed_workflow_operations(fixture$db_path)
  colony <- load_current_colony(fixture$db_path)

  filtered_subset <- apply_mouse_filters(colony, list(
    mouse_line = "InbredA",
    cohort_filter = "InbredCore",
    alive_filter = "live"
  ))

  expect_equal(sort(filtered_subset$mouse_id), c("F110", "I201", "I202", "M110"))

  payload <- build_pedigree_data(
    colony,
    mouse_ids = filtered_subset$mouse_id,
    label_fields = c("mouse_id", "sex", "cohort_label"),
    feature_fields = c("geno_cyb5r4", "geno_lysm_cre"),
    render_overrides = list(width_in = 30, height_in = 16)
  )
  plot_obj <- draw_pedigree(payload)

  expect_equal(payload$feature_map$source_field, c("geno_cyb5r4", "geno_lysm_cre"))
  expect_equal(payload$render_specs$plot_width_px, as.integer(round(30 * 110)))
  expect_equal(payload$render_specs$plot_height_px, as.integer(round(16 * 110)))
  expect_s3_class(plot_obj, "ggped_plot")
  expect_equal(attr(plot_obj, "engine_used"), "installed_ggped")
  expect_true(any(grepl("InbredCore", plot_obj$dat$Name, fixed = TRUE)))
})
