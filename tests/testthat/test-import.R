test_that("genotype parsing extracts sorted allele strings", {
  expect_equal(
    extract_genotype_component("Cyb5r4(Ko/Wt), Cmv-Cre(+/-)", "Cyb5r4"),
    "Ko/Wt"
  )

  expect_equal(
    extract_genotype_component("GeneA(b/a)", "GeneA"),
    "a/b"
  )

  expect_true(is.na(extract_genotype_component(NA, "GeneA")))
})

test_that("coerce_date_column parses month-day-year strings strictly", {
  expect_equal(
    coerce_date_column(c("12-18-2025", "02-01-2026", "02-06-2026")),
    as.Date(c("2025-12-18", "2026-02-01", "2026-02-06"))
  )
})

test_that("compute_age_columns falls back to age_source for implausible dates", {
  ages <- compute_age_columns(
    dob = as.Date(c("0002-01-20", "2026-01-01")),
    end_date = as.Date(c(NA, NA)),
    age_source = c("9w", "10w"),
    reference_date = as.Date("2026-04-12")
  )

  expect_equal(ages$age_weeks[[1]], 9)
  expect_equal(ages$age_label[[1]], "9.0 wk")
  expect_equal(ages$age_weeks[[2]], 14.4)
})

test_that("compute_age_columns can prefer age_source for ended mice with missing end dates", {
  ages <- compute_age_columns(
    dob = as.Date("2025-12-18"),
    end_date = as.Date(NA),
    age_source = "7w",
    reference_date = as.Date("2026-04-12"),
    prefer_age_source = TRUE
  )

  expect_equal(ages$age_weeks[[1]], 7)
  expect_equal(ages$age_label[[1]], "7.0 wk")
})

test_that("merge_import_sources prefers softmouse rows but preserves local-only mice", {
  imported_at <- as.POSIXct("2026-04-12 10:00:00", tz = "UTC")

  soft <- tibble::tibble(
    source_type = "softmouse",
    source_path = "soft.xlsx",
    source_file = "soft.xlsx",
    imported_at = imported_at,
    mouse_id = c("100", "101"),
    sex = c("M", "F"),
    dob = as.Date(c("2026-01-01", "2026-01-02")),
    end_date = as.Date(c(NA, NA)),
    end_type = c(NA, NA),
    age_source = c("10 wk", "10 wk"),
    age_days = c(70, 70),
    age_weeks = c(10, 10),
    age_label = c("10.0 wk", "10.0 wk"),
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

  snap <- tibble::tibble(
    source_type = "local_snapshot",
    source_path = "snap.xlsx",
    source_file = "snap.xlsx",
    imported_at = imported_at - 3600,
    mouse_id = c("101", "102"),
    sex = c("F", "M"),
    dob = as.Date(c("2026-01-02", "2025-12-01")),
    end_date = as.Date(c(NA, "2026-04-01")),
    end_type = c(NA, "Ended"),
    age_source = c("10 wk", "17 wk"),
    age_days = c(70, 120),
    age_weeks = c(10, 17.1),
    age_label = c("10.0 wk", "17.1 wk"),
    status = c("Active", "Ended"),
    alive = c(TRUE, FALSE),
    raw_genotype = c("GeneA(Ko/Wt)", "GeneA(Ko/Ko)"),
    mouse_line = c("Line1", "Line2"),
    generation = c("F1", "F0"),
    first_gene = c("GeneA", "GeneA"),
    second_gene = c(NA, NA),
    protocol = c(NA, NA),
    sire_id = c("100", NA),
    dam_id = c(NA, NA),
    mate_id = c(NA, NA),
    source_comment = c(NA, "historic"),
    founder = c(FALSE, TRUE),
    geno_genea = c("Ko/Wt", "Ko/Ko")
  )

  merged <- merge_import_sources(soft, snap)

  expect_equal(sort(merged$mouse_id), c("100", "101", "102"))
  expect_equal(merged$source_type[merged$mouse_id == "101"], "softmouse")
  expect_equal(merged$source_type[merged$mouse_id == "102"], "local_snapshot")
})

test_that("merge_import_sources repairs implausible softmouse dates from the snapshot", {
  imported_at <- as.POSIXct("2026-04-12 10:00:00", tz = "UTC")

  soft <- tibble::tibble(
    source_type = "softmouse",
    source_path = "soft.xlsx",
    source_file = "soft.xlsx",
    imported_at = imported_at,
    mouse_id = "4290",
    alt_id = NA_character_,
    mouse_sid = NA_character_,
    sex = "F",
    dob = as.Date("0002-01-20"),
    end_date = as.Date(NA),
    end_type = NA_character_,
    age_source = "9w",
    age_days = 739333,
    age_weeks = 105619,
    age_label = "105619.0 wk",
    status = "Stock",
    alive = TRUE,
    raw_genotype = "Cyb5r4(Fl/Fl), Lysm-Cre(+/-)",
    mouse_line = "CYB5R4FL",
    generation = "N5+N1+F5",
    first_gene = "Cyb5r4",
    second_gene = "Lysm-Cre",
    protocol = NA_character_,
    sire_id = NA_character_,
    dam_id = NA_character_,
    mate_id = NA_character_,
    source_comment = NA_character_,
    founder = FALSE,
    geno_cyb5r4 = "Fl/Fl",
    geno_lysm_cre = "+/-"
  )

  snap <- soft |>
    dplyr::mutate(
      source_type = "local_snapshot",
      source_path = "snap.xlsx",
      source_file = "snap.xlsx",
      imported_at = imported_at - 3600,
      dob = as.Date("2026-02-01"),
      age_days = 70,
      age_weeks = 10,
      age_label = "10.0 wk"
    )

  merged <- merge_import_sources(soft, snap)

  expect_equal(merged$dob[[1]], as.Date("2026-02-01"))
  expect_true(is_plausible_mouse_age_days(merged$age_days[[1]]))
  expect_true(merged$age_weeks[[1]] < 100)
})

test_that("find_latest_data_file uses filename timestamps", {
  temp_dir <- tempfile("mousecolony-files-")
  dir.create(temp_dir)
  file.create(file.path(temp_dir, "SoftMouse.NET-Mouse List-MeganMiller2026-04-01 1528.xlsx"))
  file.create(file.path(temp_dir, "SoftMouse.NET-Mouse List-MeganMiller2026-04-06 1446.xlsx"))

  latest <- find_latest_softmouse_export(temp_dir)
  expect_match(basename(latest), "2026-04-06 1446")
})
