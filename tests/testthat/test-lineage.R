test_that("trace_lineage returns ancestors, descendants, and sibling options", {
  colony <- tibble::tibble(
    mouse_id = c("S1", "D1", "P1", "P2", "C1", "C2", "GC1"),
    sire_id = c(NA, NA, "S1", "S1", "P1", "P1", "C1"),
    dam_id = c(NA, NA, "D1", "D1", "P2", "P2", "C2"),
    mate_id = c("D1", "S1", "P2", "P1", "C2", "C1", NA),
    sex = c("M", "F", "M", "F", "M", "F", "M"),
    alive = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    raw_genotype = c(NA, NA, NA, NA, NA, NA, NA),
    age_label = c(NA, NA, NA, NA, NA, NA, NA),
    mouse_line = c("L", "L", "L", "L", "L", "L", "L"),
    generation = c("F0", "F0", "F1", "F1", "F2", "F2", "F3")
  )

  up_result <- trace_lineage("C1", colony, direction = "up", max_generations = 2)
  expect_true(all(c("P1", "P2", "S1", "D1") %in% up_result$ids))
  expect_false("GC1" %in% up_result$ids)

  down_result <- trace_lineage("P1", colony, direction = "down", max_generations = 2, include_descendant_siblings = TRUE)
  expect_true(all(c("C1", "C2", "GC1") %in% down_result$ids))

  sibling_result <- trace_lineage("C1", colony, direction = "both", max_generations = 1, include_focus_siblings = TRUE)
  expect_true("C2" %in% sibling_result$ids)
})

test_that("trace_lineage preserves output schema when the colony is empty", {
  result <- trace_lineage("C1", empty_colony_df(), direction = "both", max_generations = 2)

  expect_equal(result$ids, character(0))
  expect_true(all(c("mouse_id", "lineage_roles", "lineage_depth") %in% names(result$rows)))
  expect_true(all(c("mouse_id", "lineage_roles", "lineage_depth") %in% names(result$hits)))
  expect_true(all(c("mouse_id", "sire_id", "dam_id") %in% names(result$graph)))
})
