test_that("boolean filters subset rows correctly", {
  df <- tibble::tibble(
    mouse_id = c("100", "101", "102"),
    sex = c("M", "F", "M"),
    dob = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    age_weeks = c(10, 11, 12),
    alive = c(TRUE, FALSE, TRUE),
    is_breeder = c(TRUE, FALSE, FALSE),
    experiment_ready = c(FALSE, TRUE, TRUE),
    mouse_line = c("L1", "L1", "L2"),
    generation = c("F1", "F1", "F2"),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneA(Ko/Wt)", "GeneB(Fl/Fl)"),
    cohort_label = c("C1", "C1", NA),
    local_flags = c("founder", "", "hold"),
    reservation_status = c("available", "reserved", "hold")
  )

  expect_equal(apply_mouse_filters(df, list(alive_filter = "live"))$mouse_id, c("100", "102"))
  expect_equal(apply_mouse_filters(df, list(alive_filter = "ended"))$mouse_id, "101")
  expect_equal(apply_mouse_filters(df, list(breeder_filter = "yes"))$mouse_id, "100")
  expect_equal(sort(apply_mouse_filters(df, list(ready_filter = "yes"))$mouse_id), c("101", "102"))
})

test_that("blank selectize-style values are ignored rather than filtering everything out", {
  df <- tibble::tibble(
    mouse_id = c("100", "101"),
    sex = c("M", "F"),
    dob = as.Date(c("2026-01-01", "2026-01-02")),
    age_weeks = c(10, 11),
    alive = c(TRUE, TRUE),
    is_breeder = c(FALSE, FALSE),
    experiment_ready = c(FALSE, FALSE),
    mouse_line = c("L1", "L2"),
    generation = c("F1", "F2"),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneB(Ko/Wt)"),
    cohort_label = c(NA, NA),
    local_flags = c("", ""),
    reservation_status = c("available", "available")
  )

  expect_equal(nrow(apply_mouse_filters(df, list(mouse_line = ""))), 2)
  expect_equal(nrow(apply_mouse_filters(df, list(generation = ""))), 2)
  expect_equal(apply_mouse_filters(df, list(mouse_line = "", id_query = "100", id_mode = "exact"))$mouse_id, "100")
})

test_that("genotype text query accepts regex patterns", {
  df <- tibble::tibble(
    mouse_id = c("100", "101", "102"),
    sex = c("M", "F", "M"),
    dob = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    age_weeks = c(10, 11, 12),
    alive = c(TRUE, TRUE, TRUE),
    is_breeder = c(FALSE, FALSE, FALSE),
    experiment_ready = c(FALSE, FALSE, FALSE),
    mouse_line = c("L1", "L1", "L2"),
    generation = c("F1", "F1", "F2"),
    raw_genotype = c("GeneA(Wt/Wt), Lysm-Cre(+/-)", "GeneA(Ko/Wt), Lysm-Cre(-/-)", "GeneB(Fl/Fl)"),
    cohort_label = c(NA, NA, NA),
    local_flags = c("", "", ""),
    reservation_status = c("available", "available", "available")
  )

  expect_equal(
    sort(apply_mouse_filters(df, list(genotype_query = "Lysm-Cre\\((\\+/-|-/-)\\)"))$mouse_id),
    c("100", "101")
  )
})
