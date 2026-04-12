pedigree_feature_columns <- function(df) {
  base_fields <- c("alive", "is_breeder", "experiment_ready")
  unique(c(base_fields, genotype_column_names(df)))
}

feature_to_numeric <- function(x) {
  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (is.numeric(x)) {
    if (all(is.na(x))) {
      return(x)
    }

    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) {
      return(rep(1, length(x)))
    }

    return((x - rng[[1]]) / diff(rng))
  }

  x <- trim_na(x)
  levels <- sort(unique(stats::na.omit(x)))
  if (length(levels) == 0) {
    return(rep(NA_real_, length(x)))
  }

  rank_vals <- match(x, levels)
  if (length(levels) == 1) {
    return(ifelse(is.na(rank_vals), NA_real_, 1))
  }

  (rank_vals - 1) / (length(levels) - 1)
}

build_pedigree_data <- function(df, mouse_ids, label_fields = c("mouse_id", "age_label", "raw_genotype"),
                                feature_fields = character(0)) {
  mouse_ids <- normalize_mouse_id(mouse_ids)
  mouse_ids <- unique(stats::na.omit(mouse_ids))
  subset_df <- tibble::as_tibble(df) |>
    dplyr::filter(mouse_id %in% mouse_ids) |>
    dplyr::distinct(mouse_id, .keep_all = TRUE)

  if (nrow(subset_df) == 0) {
    return(list(data = subset_df, feature_fields = character(0), label_fields = label_fields))
  }

  subset_df <- subset_df |>
    dplyr::mutate(
      famID = 1L,
      personID = mouse_id,
      momID = dam_id,
      dadID = sire_id,
      spouseID = mate_id,
      status_flag = ifelse(alive %in% TRUE, 0, 1),
      label = build_mouse_label(subset_df, label_fields)
    )

  usable_feature_fields <- intersect(feature_fields, names(subset_df))
  if (length(usable_feature_fields) == 0) {
    subset_df$feature_alive <- feature_to_numeric(!subset_df$alive)
    usable_feature_fields <- "feature_alive"
  } else {
    for (feature_name in usable_feature_fields) {
      subset_df[[paste0(feature_name, "_num")]] <- feature_to_numeric(subset_df[[feature_name]])
    }
    usable_feature_fields <- paste0(usable_feature_fields, "_num")
  }

  list(
    data = subset_df,
    feature_fields = usable_feature_fields,
    label_fields = label_fields
  )
}

draw_pedigree_ggpedigree <- function(pedigree_data, interactive = FALSE) {
  if (!requireNamespace("ggpedigree", quietly = TRUE)) {
    stop("The ggpedigree package is not installed.")
  }

  ped <- pedigree_data$data
  fill_col <- pedigree_data$feature_fields[[1]] %||% NULL
  tooltip_cols <- unique(c("personID", "label", "raw_genotype", "age_label", "mouse_line", "generation"))
  tooltip_cols <- intersect(tooltip_cols, names(ped))

  plot <- ggpedigree::ggPedigree(
    ped = ped,
    famID = "famID",
    personID = "personID",
    momID = "momID",
    dadID = "dadID",
    spouseID = "spouseID",
    status_column = "status_flag",
    focal_fill_column = fill_col,
    tooltip_columns = tooltip_cols,
    interactive = interactive,
    code_male = "M",
    config = list(
      label_include = TRUE,
      plot_title = "Mouse pedigree"
    ),
    sexVar = "sex"
  )

  attr(plot, "engine_used") <- "ggpedigree"
  plot
}

draw_pedigree <- function(pedigree_data, interactive = FALSE) {
  draw_pedigree_ggpedigree(pedigree_data, interactive = interactive)
}
