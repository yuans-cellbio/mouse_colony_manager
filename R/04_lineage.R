build_lineage_indexes <- function(df) {
  lineage_df <- df |>
    dplyr::transmute(
      mouse_id = normalize_mouse_id(mouse_id),
      sire_id = normalize_mouse_id(sire_id),
      dam_id = normalize_mouse_id(dam_id)
    ) |>
    dplyr::distinct()

  children_index <- lineage_df |>
    tidyr::pivot_longer(c(sire_id, dam_id), names_to = "parent_type", values_to = "parent_id") |>
    dplyr::filter(!is.na(parent_id)) |>
    dplyr::group_by(parent_id) |>
    dplyr::summarise(children = list(sort(unique(mouse_id))), .groups = "drop")

  list(
    lineage_df = lineage_df,
    children_index = children_index
  )
}

empty_lineage_hits <- function() {
  tibble::tibble(
    mouse_id = character(),
    lineage_roles = character(),
    lineage_depth = integer()
  )
}

empty_lineage_rows <- function(df = empty_colony_df()) {
  row_count <- nrow(df)

  dplyr::bind_rows(
    tibble::as_tibble(df),
    empty_colony_df()
  ) |>
    dplyr::slice_head(n = row_count) |>
    dplyr::mutate(
      lineage_roles = character(),
      lineage_depth = integer()
    )
}

empty_lineage_graph <- function(df = empty_colony_df()) {
  empty_lineage_rows(df) |>
    dplyr::select(
      mouse_id, sire_id, dam_id, mate_id, sex, alive,
      raw_genotype, age_label, mouse_line, generation,
      lineage_roles, lineage_depth
    )
}

empty_lineage_result <- function(df = empty_colony_df()) {
  list(
    ids = character(0),
    rows = empty_lineage_rows(df),
    graph = empty_lineage_graph(df),
    hits = empty_lineage_hits()
  )
}

get_parents_for_id <- function(id, lineage_df) {
  rows <- lineage_df[lineage_df$mouse_id == id, , drop = FALSE]
  unique(stats::na.omit(c(rows$sire_id, rows$dam_id)))
}

get_children_for_id <- function(id, children_index) {
  match_idx <- match(id, children_index$parent_id)
  if (is.na(match_idx)) {
    return(character(0))
  }

  children_index$children[[match_idx]] %||% character(0)
}

get_siblings_for_id <- function(id, lineage_df, children_index) {
  parents <- get_parents_for_id(id, lineage_df)
  if (length(parents) == 0) {
    return(character(0))
  }

  siblings <- unique(unlist(lapply(parents, get_children_for_id, children_index = children_index), use.names = FALSE))
  setdiff(siblings, id)
}

walk_generation_levels <- function(seed_ids, step_fn, max_generations = 3, exhaustive = FALSE) {
  levels <- list()
  frontier <- unique(seed_ids)
  visited <- unique(seed_ids)
  generation <- 0L

  repeat {
    if (!exhaustive && generation >= max_generations) {
      break
    }

    generation <- generation + 1L
    next_ids <- unique(unlist(lapply(frontier, step_fn), use.names = FALSE))
    next_ids <- setdiff(next_ids, visited)

    if (length(next_ids) == 0) {
      break
    }

    levels[[as.character(generation)]] <- next_ids
    visited <- c(visited, next_ids)
    frontier <- next_ids
  }

  levels
}

levels_to_hits <- function(levels, role) {
  if (length(levels) == 0) {
    return(tibble::tibble(mouse_id = character(0), lineage_role = character(0), lineage_depth = integer(0)))
  }

  purrr::imap_dfr(levels, function(ids, depth) {
    tibble::tibble(
      mouse_id = ids,
      lineage_role = role,
      lineage_depth = as.integer(depth)
    )
  })
}

trace_lineage <- function(seed_ids, data, direction = c("both", "up", "down"),
                          max_generations = 3, exhaustive = FALSE,
                          include_focus_siblings = FALSE,
                          include_ancestor_siblings = FALSE,
                          include_descendant_siblings = FALSE) {
  direction <- match.arg(direction)
  seed_ids <- normalize_mouse_id(seed_ids)
  seed_ids <- unique(stats::na.omit(seed_ids))
  lineage_df <- tibble::as_tibble(data)

  if (length(seed_ids) == 0 || nrow(lineage_df) == 0) {
    return(empty_lineage_result(lineage_df))
  }

  indexes <- build_lineage_indexes(lineage_df)
  lineage_only <- indexes$lineage_df
  children_index <- indexes$children_index

  ancestor_levels <- list()
  descendant_levels <- list()

  if (direction %in% c("both", "up")) {
    ancestor_levels <- walk_generation_levels(
      seed_ids,
      step_fn = function(id) get_parents_for_id(id, lineage_only),
      max_generations = max_generations,
      exhaustive = exhaustive
    )
  }

  if (direction %in% c("both", "down")) {
    descendant_levels <- walk_generation_levels(
      seed_ids,
      step_fn = function(id) get_children_for_id(id, children_index),
      max_generations = max_generations,
      exhaustive = exhaustive
    )
  }

  hits <- tibble::tibble(
    mouse_id = seed_ids,
    lineage_role = "seed",
    lineage_depth = 0L
  )

  hits <- dplyr::bind_rows(
    hits,
    levels_to_hits(ancestor_levels, "ancestor"),
    levels_to_hits(descendant_levels, "descendant")
  )

  if (include_focus_siblings) {
    focus_siblings <- unique(unlist(lapply(seed_ids, get_siblings_for_id, lineage_df = lineage_only, children_index = children_index), use.names = FALSE))
    hits <- dplyr::bind_rows(
      hits,
      tibble::tibble(mouse_id = focus_siblings, lineage_role = "focus_sibling", lineage_depth = 0L)
    )
  }

  if (include_ancestor_siblings && length(ancestor_levels) > 0) {
    ancestor_hits <- levels_to_hits(ancestor_levels, "ancestor")
    ancestor_siblings <- purrr::pmap_dfr(
      list(ancestor_hits$mouse_id, ancestor_hits$lineage_depth),
      function(mouse_id, lineage_depth) {
        tibble::tibble(
          mouse_id = get_siblings_for_id(mouse_id, lineage_df = lineage_only, children_index = children_index),
          lineage_role = "ancestor_sibling",
          lineage_depth = lineage_depth
        )
      }
    )
    hits <- dplyr::bind_rows(hits, ancestor_siblings)
  }

  if (include_descendant_siblings && length(descendant_levels) > 0) {
    descendant_hits <- levels_to_hits(descendant_levels, "descendant")
    descendant_siblings <- purrr::pmap_dfr(
      list(descendant_hits$mouse_id, descendant_hits$lineage_depth),
      function(mouse_id, lineage_depth) {
        tibble::tibble(
          mouse_id = get_siblings_for_id(mouse_id, lineage_df = lineage_only, children_index = children_index),
          lineage_role = "descendant_sibling",
          lineage_depth = lineage_depth
        )
      }
    )
    hits <- dplyr::bind_rows(hits, descendant_siblings)
  }

  hits <- hits |>
    dplyr::filter(!is.na(mouse_id)) |>
    dplyr::group_by(mouse_id) |>
    dplyr::summarise(
      lineage_roles = paste(sort(unique(lineage_role)), collapse = ", "),
      lineage_depth = min(lineage_depth, na.rm = TRUE),
      .groups = "drop"
    )

  rows <- lineage_df |>
    dplyr::filter(mouse_id %in% hits$mouse_id) |>
    dplyr::left_join(hits, by = "mouse_id") |>
    dplyr::arrange(lineage_depth, mouse_id)

  graph <- rows |>
    dplyr::select(mouse_id, sire_id, dam_id, mate_id, sex, alive, raw_genotype, age_label, mouse_line, generation, lineage_roles, lineage_depth)

  list(
    ids = rows$mouse_id,
    rows = rows,
    graph = graph,
    hits = hits
  )
}
