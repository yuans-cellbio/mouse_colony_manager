test_that("build_pedigree_data adds placeholder parents for missing primary entries", {
  df <- tibble::tibble(
    mouse_id = c("1201", "1202"),
    sex = c("M", "F"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneA(Ko/Wt)"),
    age_label = c("10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1"),
    generation = c("F1", "F1"),
    sire_id = c("30037", NA),
    dam_id = c("30038", NA),
    mate_id = c(NA, NA)
  )

  payload <- build_pedigree_data(df, mouse_ids = "1201")
  ped <- payload$data

  expect_true(all(c("30037", "30038") %in% ped$personID))
  expect_equal(ped$sex[ped$personID == "30037"], "M")
  expect_equal(ped$sex[ped$personID == "30038"], "F")
  expect_true(all(is.na(ped$momID[ped$personID %in% c("30037", "30038")])))
  expect_true(all(is.na(ped$dadID[ped$personID %in% c("30037", "30038")])))
})

test_that("build_pedigree_data infers sex from sire and dam roles when missing", {
  df <- tibble::tibble(
    mouse_id = c("2001", "2002", "2003"),
    sex = c(NA, NA, "F"),
    alive = c(TRUE, TRUE, TRUE),
    raw_genotype = c(NA, NA, NA),
    age_label = c(NA, NA, NA),
    mouse_line = c("L1", "L1", "L1"),
    generation = c("F0", "F0", "F1"),
    sire_id = c(NA, NA, "2001"),
    dam_id = c(NA, NA, "2002"),
    mate_id = c(NA, NA, NA)
  )

  payload <- build_pedigree_data(df, mouse_ids = c("2001", "2002", "2003"))
  ped <- payload$data

  expect_equal(ped$sex[ped$personID == "2001"], "M")
  expect_equal(ped$sex[ped$personID == "2002"], "F")
})

test_that("pedigree render specs and config use the custom label column with fixed scaling", {
  df <- tibble::tibble(
    mouse_id = c("1201", "1202", "1203"),
    sex = c("F", "M", "M"),
    alive = c(TRUE, TRUE, TRUE),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneA(Ko/Wt)", "GeneA(Ko/Wt)"),
    age_label = c("10.0 wk", "10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1", "L1"),
    generation = c("F1", "F1", "F1"),
    sire_id = c("30044", "30044", "30044"),
    dam_id = c("30039", "30039", "30039"),
    mate_id = c(NA, NA, NA)
  )

  payload <- build_pedigree_data(
    df,
    mouse_ids = c("1201", "1202", "1203"),
    label_fields = c("mouse_id", "sex", "age_label", "raw_genotype")
  )
  config <- build_pedigree_plot_config(payload)

  expect_true(payload$render_specs$plot_width_px > 0)
  expect_true(payload$render_specs$plot_height_px > 0)
  expect_equal(config$label_column, "label")
  expect_false(config$label_scale_by_pedigree)
  expect_false(config$point_scale_by_pedigree)
  expect_false(config$status_include)
})

test_that("feature_to_numeric maps genotype dosage strings onto 0 to 1 intensity", {
  dosage <- feature_to_numeric(c("Wt/Wt", "Ko/Wt", "Ko/Ko", "+/-", "+/+", "-/-", NA))

  expect_equal(dosage[[1]], 0)
  expect_equal(dosage[[2]], 0.5)
  expect_equal(dosage[[3]], 1)
  expect_equal(dosage[[4]], 0.5)
  expect_equal(dosage[[5]], 1)
  expect_equal(dosage[[6]], 0)
  expect_true(is.na(dosage[[7]]))
})

test_that("build_pedigree_data respects manual canvas overrides", {
  df <- tibble::tibble(
    mouse_id = c("1201", "1202"),
    sex = c("F", "M"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("Cyb5r4(Wt/Wt)", "Cyb5r4(Ko/Wt)"),
    age_label = c("10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1"),
    generation = c("F1", "F1"),
    sire_id = c("30044", "30044"),
    dam_id = c("30039", "30039"),
    mate_id = c(NA, NA),
    geno_cyb5r4 = c("Wt/Wt", "Ko/Wt")
  )

  payload <- build_pedigree_data(
    df,
    mouse_ids = c("1201", "1202"),
    feature_fields = "geno_cyb5r4",
    render_overrides = list(width_in = 44, height_in = 19)
  )
  config <- build_pedigree_plot_config(payload)

  expect_equal(payload$render_specs$width_in, 44)
  expect_equal(payload$render_specs$height_in, 19)
  expect_equal(payload$render_specs$plot_width_px, as.integer(round(44 * 110)))
  expect_equal(payload$render_specs$plot_height_px, as.integer(round(19 * 90)))
  expect_true(config$focal_fill_include)
  expect_equal(config$focal_fill_legend_title, "cyb5r4")
})

test_that("build_pedigree_data tracks feature map for split-node rendering", {
  df <- tibble::tibble(
    mouse_id = c("1201", "1202"),
    sex = c("F", "M"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("Cyb5r4(Wt/Wt), Lysm-Cre(+/+)", "Cyb5r4(Fl/Wt), Lysm-Cre(+/-)"),
    age_label = c("10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1"),
    generation = c("F1", "F1"),
    sire_id = c("30044", "30044"),
    dam_id = c("30039", "30039"),
    mate_id = c(NA, NA),
    geno_cyb5r4 = c("Wt/Wt", "Fl/Wt"),
    geno_lysm_cre = c("+/+", "+/-")
  )

  payload <- build_pedigree_data(
    df,
    mouse_ids = c("1201", "1202"),
    feature_fields = c("geno_cyb5r4", "geno_lysm_cre")
  )

  expect_equal(payload$feature_map$source_field, c("geno_cyb5r4", "geno_lysm_cre"))
  expect_equal(payload$feature_map$display_name, c("cyb5r4", "lysm cre"))
  expect_true(all(payload$feature_fields %in% names(payload$data)))
})

test_that("legacy pedigree renderer builds a split-node plot when optional packages are installed", {
  skip_if_not_installed("ggped")
  skip_if_not_installed("kinship2")
  skip_if_not_installed("RColorBrewer")

  df <- tibble::tibble(
    mouse_id = c("1201", "1202"),
    sex = c("F", "M"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("Cyb5r4(Wt/Wt), Lysm-Cre(+/+)", "Cyb5r4(Fl/Wt), Lysm-Cre(+/-)"),
    age_label = c("10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1"),
    generation = c("F1", "F1"),
    sire_id = c("30044", "30044"),
    dam_id = c("30039", "30039"),
    mate_id = c(NA, NA),
    geno_cyb5r4 = c("Wt/Wt", "Fl/Wt"),
    geno_lysm_cre = c("+/+", "+/-")
  )

  payload <- build_pedigree_data(
    df,
    mouse_ids = c("1201", "1202"),
    feature_fields = c("geno_cyb5r4", "geno_lysm_cre")
  )
  plot_obj <- draw_pedigree(payload, engine = "legacy")

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(attr(plot_obj, "engine_used"), "legacy")
})

test_that("draw_pedigree uses multiline labels from the selected label fields", {
  skip_if_not_installed("ggpedigree")

  df <- tibble::tibble(
    mouse_id = c("1201", "1202"),
    sex = c("F", "M"),
    alive = c(TRUE, TRUE),
    raw_genotype = c("GeneA(Wt/Wt)", "GeneA(Ko/Wt)"),
    age_label = c("10.0 wk", "10.0 wk"),
    mouse_line = c("L1", "L1"),
    generation = c("F1", "F1"),
    sire_id = c("30044", "30044"),
    dam_id = c("30039", "30039"),
    mate_id = c(NA, NA)
  )

  payload <- build_pedigree_data(
    df,
    mouse_ids = c("1201", "1202"),
    label_fields = c("mouse_id", "sex", "age_label")
  )
  plot_obj <- draw_pedigree(payload)
  built <- ggplot2::ggplot_build(plot_obj)
  label_layers <- purrr::keep(built$data, ~ "label" %in% names(.x))
  labels <- unique(unlist(purrr::map(label_layers, "label"), use.names = FALSE))

  expect_true(any(grepl("1201\nF\n10.0 wk", labels, fixed = TRUE)))
})
