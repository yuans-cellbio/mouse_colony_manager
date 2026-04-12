test_that("annotations, reservations, and presets round-trip through SQLite", {
  temp_dir <- tempfile("mousecolony-db-")
  dir.create(temp_dir)
  db_path <- file.path(temp_dir, "colony.sqlite")

  soft <- tibble::tibble(
    source_type = "softmouse",
    source_path = "soft.xlsx",
    source_file = "soft.xlsx",
    imported_at = as.POSIXct("2026-04-12 10:00:00", tz = "UTC"),
    mouse_id = c("100", "101"),
    alt_id = c(NA, NA),
    mouse_sid = c(NA, NA),
    sex = c("M", "F"),
    dob = as.Date(c("2026-01-01", "2026-01-02")),
    end_date = as.Date(c(NA, NA)),
    end_type = c(NA, NA),
    age_source = c("10 wk", "10 wk"),
    age_days = c(70, 69),
    age_weeks = c(10, 9.9),
    age_label = c("10.0 wk", "9.9 wk"),
    status = c("Active", "Active"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneA(Ko/Wt)"),
    mouse_line = c("Line1", "Line1"),
    generation = c("F1", "F1"),
    first_gene = c("GeneA", "GeneA"),
    second_gene = c(NA, NA),
    protocol = c(NA, NA),
    sire_id = c(NA, "100"),
    dam_id = c(NA, NA),
    mate_id = c(NA, NA),
    source_comment = c(NA, NA),
    founder = c(TRUE, FALSE),
    geno_genea = c("Wt/Wt", "Ko/Wt")
  )

  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)
  write_snapshot(con, soft, tibble::tibble(), soft, force = TRUE)

  save_annotation(db_path, "100", is_breeder = TRUE, experiment_ready = TRUE, cohort_label = "C1", local_flags = "founder", notes = "important")
  save_reservation(db_path, "101", project_name = "Study A", reserved_by = "Lab", reserved_until = as.Date("2026-04-30"), status = "reserved", notes = "hold for imaging")
  save_filter_preset(db_path, "ready-mice", list(alive_filter = "live", ready_filter = "yes"))

  colony <- load_current_colony(db_path)
  preset <- load_filter_preset(db_path, "ready-mice")

  expect_true(colony$is_breeder[colony$mouse_id == "100"])
  expect_true(colony$experiment_ready[colony$mouse_id == "100"])
  expect_equal(colony$reservation_status[colony$mouse_id == "101"], "reserved")
  expect_equal(preset$ready_filter, "yes")
})

test_that("fresh SQLite store exposes an empty but fully typed colony schema", {
  temp_dir <- tempfile("mousecolony-empty-db-")
  dir.create(temp_dir)
  db_path <- file.path(temp_dir, "colony.sqlite")

  ensure_colony_store(db_path)

  colony <- load_current_colony(db_path)
  meta <- get_colony_metadata(db_path)

  expect_equal(nrow(colony), 0)
  expect_true(all(c("mouse_id", "is_breeder", "reservation_status") %in% names(colony)))
  expect_equal(nrow(meta$imports), 0)
  expect_true(all(c("preset_name", "preset_json", "updated_at") %in% names(meta$presets)))
})

test_that("app server boots cleanly when the working database is missing", {
  temp_dir <- tempfile("mousecolony-app-")
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "data"))

  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_no_error(
    shiny::testServer(colony_app_server, {
      df <- colony_data()
      expect_equal(nrow(df), 0)
      expect_true("mouse_id" %in% names(df))
    })
  )

  expect_true(file.exists(file.path(temp_dir, "data", "mouse_colony_manager.sqlite")))
})
