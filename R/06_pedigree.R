pedigree_feature_columns <- function(df) {
  base_fields <- c("alive", "is_breeder", "experiment_ready")
  unique(c(genotype_column_names(df), base_fields))
}

count_label_lines <- function(labels) {
  labels <- dplyr::coalesce(as.character(labels), "")
  pmax(1L, stringr::str_count(labels, "\n") + 1L)
}

compute_pedigree_generation_levels <- function(df) {
  df <- tibble::as_tibble(df)
  if (nrow(df) == 0) {
    return(integer(0))
  }

  ids <- df$mouse_id
  row_lookup <- split(seq_len(nrow(df)), df$mouse_id)
  memo <- new.env(parent = emptyenv())
  visiting <- character(0)

  level_for <- function(id) {
    if (!id %in% ids) {
      return(1L)
    }

    if (exists(id, envir = memo, inherits = FALSE)) {
      return(get(id, envir = memo, inherits = FALSE))
    }

    if (id %in% visiting) {
      return(1L)
    }

    visiting <<- c(visiting, id)
    row_idx <- row_lookup[[id]][[1]]
    parent_ids <- stats::na.omit(c(df$sire_id[[row_idx]], df$dam_id[[row_idx]]))
    parent_levels <- if (length(parent_ids) == 0) {
      0L
    } else {
      vapply(parent_ids, level_for, integer(1))
    }
    visiting <<- setdiff(visiting, id)

    level <- 1L + max(parent_levels, 0L)
    assign(id, level, envir = memo)
    level
  }

  stats::setNames(vapply(ids, level_for, integer(1)), ids)
}

compute_pedigree_render_specs <- function(df) {
  df <- tibble::as_tibble(df)
  if (nrow(df) == 0) {
    return(list(
      node_count = 0L,
      leaf_count = 0L,
      generation_span = 0L,
      max_label_lines = 1L,
      width_in = 12,
      height_in = 8,
      plot_width_px = 1200L,
      plot_height_px = 800L,
      ped_width = 15
    ))
  }

  parent_ids <- unique(stats::na.omit(c(df$sire_id, df$dam_id)))
  child_counts <- table(c(stats::na.omit(df$sire_id), stats::na.omit(df$dam_id)))
  leaf_count <- sum(!df$mouse_id %in% names(child_counts))
  if (leaf_count == 0) {
    leaf_count <- max(1L, nrow(df))
  }

  generation_levels <- compute_pedigree_generation_levels(df)
  generation_span <- if (length(generation_levels) == 0) 1L else diff(range(generation_levels)) + 1L
  label_lines <- count_label_lines(df$label %||% df$mouse_id)
  max_label_lines <- max(label_lines, na.rm = TRUE)

  width_in <- max(14, min(60, 8 + leaf_count * 0.72 + max_label_lines * 0.9))
  height_in <- max(10, min(42, 4 + generation_span * 2.6 + max_label_lines * 0.4))
  ped_width <- max(15, min(80, leaf_count * 0.9 + 6))

  list(
    node_count = nrow(df),
    leaf_count = leaf_count,
    generation_span = generation_span,
    max_label_lines = max_label_lines,
    width_in = width_in,
    height_in = height_in,
    plot_width_px = as.integer(round(width_in * 110)),
    plot_height_px = as.integer(round(height_in * 90)),
    ped_width = ped_width,
    parent_count = length(parent_ids)
  )
}

apply_pedigree_render_overrides <- function(specs, render_overrides = NULL) {
  if (is.null(render_overrides) || length(render_overrides) == 0) {
    return(specs)
  }

  width_in <- suppressWarnings(as.numeric(render_overrides$width_in %||% specs$width_in))
  height_in <- suppressWarnings(as.numeric(render_overrides$height_in %||% specs$height_in))

  if (is.na(width_in) || width_in <= 0) {
    width_in <- specs$width_in
  }
  if (is.na(height_in) || height_in <= 0) {
    height_in <- specs$height_in
  }

  specs$width_in <- width_in
  specs$height_in <- height_in
  specs$plot_width_px <- as.integer(round(width_in * 110))
  specs$plot_height_px <- as.integer(round(height_in * 90))
  specs
}

blend_hex_with_background <- function(color_hex, alpha = 1, background_hex = "#FFFFFF") {
  color_hex <- dplyr::coalesce(trim_na(as.character(color_hex)), "#2A6F63")
  background_hex <- dplyr::coalesce(trim_na(as.character(background_hex)), "#FFFFFF")
  alpha <- suppressWarnings(as.numeric(alpha %||% 1))
  alpha <- min(max(alpha, 0), 1)

  foreground_rgb <- grDevices::col2rgb(color_hex) / 255
  background_rgb <- grDevices::col2rgb(background_hex) / 255
  blended_rgb <- alpha * foreground_rgb + (1 - alpha) * background_rgb
  grDevices::rgb(blended_rgb[[1]], blended_rgb[[2]], blended_rgb[[3]])
}

pretty_pedigree_feature_label <- function(feature_name) {
  feature_name |>
    trim_na() |>
    stringr::str_replace("^geno_", "") |>
    stringr::str_replace_all("_", " ")
}

infer_pedigree_feature_color <- function(feature_name = NULL, feature_values = NULL) {
  feature_name <- stringr::str_to_lower(trim_na(feature_name %||% "feature"))
  feature_values <- stringr::str_to_lower(trim_na(as.character(feature_values)))

  if (grepl("cyb5r4", feature_name)) {
    if (any(grepl("\\bko\\b", feature_values), na.rm = TRUE) && !any(grepl("\\bfl\\b", feature_values), na.rm = TRUE)) {
      return("#C53D3D")
    }
    return("steelblue")
  }

  if (grepl("cre|ins1|icdh5|lysm|lyz2|cmv", feature_name)) {
    return("violet")
  }

  if (grepl("alive|breeder|experiment", feature_name)) {
    return("#2A6F63")
  }

  "#2A6F63"
}

build_pedigree_feature_style <- function(feature_name = NULL, feature_values = NULL, background_hex = "#FFFFFF") {
  if (is.null(feature_name)) {
    return(NULL)
  }

  base_color <- infer_pedigree_feature_color(feature_name = feature_name, feature_values = feature_values)
  list(
    feature_name = feature_name,
    legend_title = pretty_pedigree_feature_label(feature_name),
    base_color = base_color,
    low_color = background_hex,
    mid_color = blend_hex_with_background(base_color, alpha = 0.5, background_hex = background_hex),
    high_color = blend_hex_with_background(base_color, alpha = 1, background_hex = background_hex)
  )
}

build_pedigree_feature_map <- function(df, feature_fields = character(0)) {
  usable_feature_fields <- intersect(feature_fields, names(df))

  if (length(usable_feature_fields) == 0) {
    return(tibble::tibble(
      source_field = "feature_alive",
      numeric_field = "feature_alive",
      display_name = "alive",
      base_color = infer_pedigree_feature_color("feature_alive", !df$alive)
    ))
  }

  display_names <- make.unique(vapply(usable_feature_fields, pretty_pedigree_feature_label, character(1)), sep = " ")
  base_colors <- purrr::map_chr(
    usable_feature_fields,
    ~ infer_pedigree_feature_color(feature_name = .x, feature_values = df[[.x]])
  )

  tibble::tibble(
    source_field = usable_feature_fields,
    numeric_field = paste0(usable_feature_fields, "_num"),
    display_name = display_names,
    base_color = base_colors
  )
}

build_pedigree_plot_config <- function(pedigree_data) {
  specs <- pedigree_data$render_specs %||% compute_pedigree_render_specs(pedigree_data$data)
  dense_labels <- specs$max_label_lines >= 4 || specs$node_count >= 40
  feature_style <- pedigree_data$feature_style %||% NULL

  list(
    plot_title = "Mouse pedigree",
    label_include = TRUE,
    label_column = "label",
    label_method = "geom_text",
    label_text_size = if (dense_labels) 3.0 else 3.2,
    label_nudge_y = 0.18 + min(0.18, 0.025 * specs$max_label_lines),
    label_nudge_x = 0,
    label_text_angle = 0,
    label_scale_by_pedigree = FALSE,
    point_size = if (specs$node_count >= 80) 6.4 else 7.2,
    point_scale_by_pedigree = FALSE,
    segment_scale_by_pedigree = FALSE,
    segment_linewidth = if (specs$node_count >= 80) 0.9 else 1.0,
    generation_height = 1 + min(0.8, 0.08 * specs$max_label_lines),
    ped_width = specs$ped_width,
    sex_color_include = FALSE,
    status_include = FALSE,
    overlay_include = FALSE,
    focal_fill_include = !is.null(feature_style),
    focal_fill_method = "gradient",
    focal_fill_low_color = feature_style$low_color %||% "#FFFFFF",
    focal_fill_mid_color = feature_style$mid_color %||% "#D9E6E2",
    focal_fill_high_color = feature_style$high_color %||% "#2A6F63",
    focal_fill_scale_midpoint = 0.5,
    focal_fill_legend_title = feature_style$legend_title %||% "Feature intensity",
    focal_fill_na_value = "#D8D2C6",
    focal_fill_legend_show = TRUE
  )
}

infer_sex_from_parent_roles <- function(mouse_ids, sire_ids, dam_ids) {
  dplyr::case_when(
    mouse_ids %in% sire_ids & !mouse_ids %in% dam_ids ~ "M",
    mouse_ids %in% dam_ids & !mouse_ids %in% sire_ids ~ "F",
    TRUE ~ NA_character_
  )
}

add_placeholder_parents <- function(subset_df) {
  subset_df <- tibble::as_tibble(subset_df)
  if (nrow(subset_df) == 0) {
    return(subset_df)
  }

  empty_placeholder_rows <- function() {
    tibble::tibble(
      mouse_id = character(),
      sex = character(),
      alive = logical(),
      raw_genotype = character(),
      age_label = character(),
      mouse_line = character(),
      generation = character(),
      sire_id = character(),
      dam_id = character(),
      mate_id = character(),
      status = character(),
      source_comment = character(),
      founder = logical(),
      is_placeholder = logical()
    )
  }

  present_ids <- unique(stats::na.omit(subset_df$mouse_id))
  child_rows <- subset_df |>
    dplyr::select(mouse_id, sex, mouse_line, generation, sire_id, dam_id)

  build_placeholder_rows <- function(parent_role = c("sire", "dam")) {
    parent_role <- match.arg(parent_role)
    parent_col <- if (parent_role == "sire") "sire_id" else "dam_id"
    spouse_col <- if (parent_role == "sire") "dam_id" else "sire_id"
    parent_sex <- if (parent_role == "sire") "M" else "F"

    missing_ids <- setdiff(unique(stats::na.omit(child_rows[[parent_col]])), present_ids)
    if (length(missing_ids) == 0) {
      return(empty_placeholder_rows())
    }

    purrr::map_dfr(missing_ids, function(parent_id) {
      children <- child_rows[child_rows[[parent_col]] == parent_id, , drop = FALSE]
      inferred_line <- unique(stats::na.omit(children$mouse_line))
      inferred_generation <- unique(stats::na.omit(children$generation))
      partner_ids <- unique(stats::na.omit(children[[spouse_col]]))

      tibble::tibble(
        mouse_id = parent_id,
        sex = parent_sex,
        alive = NA,
        raw_genotype = NA_character_,
        age_label = NA_character_,
        mouse_line = if (length(inferred_line) == 1) inferred_line[[1]] else NA_character_,
        generation = if (length(inferred_generation) == 1) inferred_generation[[1]] else NA_character_,
        sire_id = NA_character_,
        dam_id = NA_character_,
        mate_id = if (length(partner_ids) == 1) partner_ids[[1]] else NA_character_,
        status = "Placeholder",
        source_comment = paste("Pedigree placeholder inferred as missing", parent_role),
        founder = TRUE,
        is_placeholder = TRUE
      )
    })
  }

  placeholder_rows <- dplyr::bind_rows(
    build_placeholder_rows("sire"),
    build_placeholder_rows("dam")
  ) |>
    dplyr::distinct(mouse_id, .keep_all = TRUE)

  subset_df <- subset_df |>
    dplyr::mutate(is_placeholder = FALSE)

  dplyr::bind_rows(subset_df, placeholder_rows) |>
    dplyr::distinct(mouse_id, .keep_all = TRUE)
}

normalize_pedigree_sex <- function(df) {
  df <- tibble::as_tibble(df)
  if (nrow(df) == 0) {
    return(df)
  }

  if (!"sex" %in% names(df)) {
    df$sex <- NA_character_
  }

  inferred <- infer_sex_from_parent_roles(
    mouse_ids = df$mouse_id,
    sire_ids = unique(stats::na.omit(df$sire_id)),
    dam_ids = unique(stats::na.omit(df$dam_id))
  )

  df$sex <- dplyr::coalesce(trim_na(df$sex), inferred, "F")
  df
}

build_pedigree_label <- function(df, label_fields) {
  labels <- build_mouse_label(df, label_fields)
  if (!"is_placeholder" %in% names(df)) {
    return(labels)
  }

  placeholder_idx <- df$is_placeholder %in% TRUE
  labels[placeholder_idx] <- paste(df$mouse_id[placeholder_idx], "(placeholder)")
  labels
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
  if (any(grepl("/", x, fixed = TRUE), na.rm = TRUE)) {
    genotype_tokens <- stringr::str_split(x, "/", simplify = FALSE)
    dosage <- purrr::map_dbl(genotype_tokens, function(tokens) {
      tokens <- stringr::str_to_lower(trim_na(tokens))
      tokens <- stats::na.omit(tokens)
      if (length(tokens) == 0) {
        return(NA_real_)
      }

      positive_tokens <- c("ko", "fl", "+", "mut", "tg", "cre")
      reference_tokens <- c("wt", "-", "0")
      known_idx <- tokens %in% c(positive_tokens, reference_tokens)
      if (!any(known_idx)) {
        return(NA_real_)
      }

      mean(tokens[known_idx] %in% positive_tokens)
    })

    if (any(!is.na(dosage))) {
      return(dosage)
    }
  }

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
                                feature_fields = character(0), render_overrides = NULL) {
  mouse_ids <- normalize_mouse_id(mouse_ids)
  mouse_ids <- unique(stats::na.omit(mouse_ids))
  subset_df <- tibble::as_tibble(df) |>
    dplyr::filter(mouse_id %in% mouse_ids) |>
    dplyr::distinct(mouse_id, .keep_all = TRUE)

  if (nrow(subset_df) == 0) {
    return(list(
      data = subset_df,
      feature_fields = character(0),
      label_fields = label_fields,
      render_specs = apply_pedigree_render_overrides(compute_pedigree_render_specs(subset_df), render_overrides),
      feature_style = NULL
    ))
  }

  subset_df <- subset_df |>
    add_placeholder_parents() |>
    normalize_pedigree_sex()

  subset_df <- subset_df |>
    dplyr::mutate(
      famID = 1L,
      personID = mouse_id,
      momID = dam_id,
      dadID = sire_id,
      spouseID = mate_id,
      status_flag = ifelse(alive %in% TRUE, 0, 1),
      label = build_pedigree_label(subset_df, label_fields)
    )

  render_specs <- compute_pedigree_render_specs(subset_df)
  feature_map <- build_pedigree_feature_map(subset_df, feature_fields = feature_fields)

  for (idx in seq_len(nrow(feature_map))) {
    source_field <- feature_map$source_field[[idx]]
    numeric_field <- feature_map$numeric_field[[idx]]

    if (identical(source_field, "feature_alive")) {
      subset_df[[numeric_field]] <- feature_to_numeric(!subset_df$alive)
    } else {
      subset_df[[numeric_field]] <- feature_to_numeric(subset_df[[source_field]])
    }
  }

  feature_style <- if (nrow(feature_map) == 0) {
    NULL
  } else {
    feature_values <- if (identical(feature_map$source_field[[1]], "feature_alive")) !subset_df$alive else subset_df[[feature_map$source_field[[1]]]]
    style <- build_pedigree_feature_style(feature_map$source_field[[1]], feature_values)
    style$legend_title <- feature_map$display_name[[1]]
    style
  }

  render_specs <- apply_pedigree_render_overrides(render_specs, render_overrides)

  list(
    data = subset_df,
    feature_fields = feature_map$numeric_field,
    label_fields = label_fields,
    render_specs = render_specs,
    feature_style = feature_style,
    feature_map = feature_map
  )
}

legacy_pedigree_dependencies_available <- function() {
  all(vapply(
    c("ggped", "kinship2", "RColorBrewer", "tidyr"),
    requireNamespace,
    logical(1),
    quietly = TRUE
  ))
}

legacy_pedigree_state <- local({
  env <- new.env(parent = emptyenv())
  env$helpers <- NULL
  env
})

find_legacy_pedigree_helper_path <- function() {
  candidates <- c(
    file.path("helper_functions", "helper_functions.R"),
    file.path("..", "helper_functions", "helper_functions.R"),
    file.path("..", "..", "helper_functions", "helper_functions.R")
  )

  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Could not locate helper_functions/helper_functions.R for the legacy pedigree renderer.")
  }

  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

get_legacy_pedigree_helpers <- function() {
  if (!is.null(legacy_pedigree_state$helpers)) {
    return(legacy_pedigree_state$helpers)
  }

  if (!legacy_pedigree_dependencies_available()) {
    stop("Legacy pedigree rendering requires ggped, kinship2, RColorBrewer, and tidyr.")
  }

  helper_env <- new.env(parent = baseenv())

  bindings <- list(
    ggproto = ggplot2::ggproto,
    Geom = ggplot2::Geom,
    aes = ggplot2::aes,
    layer = ggplot2::layer,
    discrete_scale = ggplot2::discrete_scale,
    ggplot = ggplot2::ggplot,
    geom_line = ggplot2::geom_line,
    geom_segment = ggplot2::geom_segment,
    geom_text = ggplot2::geom_text,
    scale_colour_manual = ggplot2::scale_colour_manual,
    theme_void = ggplot2::theme_void,
    theme_classic = ggplot2::theme_classic,
    theme = ggplot2::theme,
    element_blank = ggplot2::element_blank,
    element_text = ggplot2::element_text,
    element_line = ggplot2::element_line,
    `%+replace%` = ggplot2::`%+replace%`,
    brewer.pal = RColorBrewer::brewer.pal,
    pivot_longer = tidyr::pivot_longer,
    all_of = tidyselect::all_of,
    unit = grid::unit,
    unit.c = grid::unit.c,
    polygonGrob = grid::polygonGrob,
    pointsGrob = grid::pointsGrob,
    textGrob = grid::textGrob,
    gpar = grid::gpar,
    gList = grid::gList,
    col2rgb = grDevices::col2rgb,
    rgb = grDevices::rgb,
    align.pedigree = kinship2::align.pedigree,
    kinship = kinship2::kinship
  )

  list2env(bindings, envir = helper_env)
  sys.source(find_legacy_pedigree_helper_path(), envir = helper_env, keep.source = FALSE)
  legacy_pedigree_state$helpers <- helper_env
  helper_env
}

build_legacy_pedigree_df <- function(pedigree_data) {
  helpers <- get_legacy_pedigree_helpers()
  ped <- pedigree_data$data
  specs <- pedigree_data$render_specs %||% compute_pedigree_render_specs(ped)

  pedigree_obj <- with(
    ped,
    kinship2::pedigree(
      id = personID,
      dadid = dadID,
      momid = momID,
      sex = sex,
      status = ifelse(alive %in% TRUE, 0, 1)
    )
  )

  aligned <- helpers$dfalign.pedigree(pedigree_obj, width = max(10, round(specs$ped_width)))
  aligned <- aligned |>
    dplyr::rename(aligned_sex = sex, aligned_status = status)

  merged <- aligned |>
    dplyr::left_join(
      ped |>
        dplyr::select(-status_flag),
      by = dplyr::join_by(Name == personID)
    ) |>
    dplyr::mutate(
      legacy_sex_label = stringr::str_to_upper(
        dplyr::coalesce(
          trim_na(as.character(sex)),
          trim_na(as.character(aligned_sex))
        )
      ),
      sex = dplyr::case_when(
        legacy_sex_label %in% c("M", "MALE", "1", "22") ~ 22,
        legacy_sex_label %in% c("F", "FEMALE", "2", "21") ~ 21,
        TRUE ~ 0
      ),
      status = as.factor(ifelse(alive %in% TRUE, 0, 1)),
      Name = dplyr::coalesce(label, Name)
    )

  feature_map <- pedigree_data$feature_map %||% tibble::tibble()
  for (idx in seq_len(nrow(feature_map))) {
    merged[[feature_map$display_name[[idx]]]] <- merged[[feature_map$numeric_field[[idx]]]]
  }

  merged
}

draw_pedigree_ggpedigree <- function(pedigree_data, interactive = FALSE) {
  if (!requireNamespace("ggpedigree", quietly = TRUE)) {
    stop("The ggpedigree package is not installed.")
  }

  ped <- pedigree_data$data
  fill_col <- pedigree_data$feature_fields[[1]] %||% NULL
  tooltip_cols <- unique(c("personID", "label", "raw_genotype", "age_label", "mouse_line", "generation"))
  tooltip_cols <- intersect(tooltip_cols, names(ped))
  config <- build_pedigree_plot_config(pedigree_data)

  plot <- ggpedigree::ggPedigree(
    ped = ped,
    famID = "famID",
    personID = "personID",
    momID = "momID",
    dadID = "dadID",
    spouseID = "spouseID",
    focal_fill_column = fill_col,
    tooltip_columns = tooltip_cols,
    interactive = interactive,
    code_male = "M",
    config = config,
    sexVar = "sex"
  )

  attr(plot, "engine_used") <- "ggpedigree"
  attr(plot, "render_specs") <- pedigree_data$render_specs %||% NULL
  plot
}

draw_pedigree_legacy <- function(pedigree_data) {
  helpers <- get_legacy_pedigree_helpers()
  specs <- pedigree_data$render_specs %||% compute_pedigree_render_specs(pedigree_data$data)
  feature_map <- pedigree_data$feature_map %||% tibble::tibble()
  legacy_df <- build_legacy_pedigree_df(pedigree_data)

  feature_names <- feature_map$display_name
  feature_colors <- if (length(feature_names) == 0) {
    c("alive" = "#2A6F63")
  } else {
    stats::setNames(feature_map$base_color, feature_names)
  }

  text_size <- dplyr::case_when(
    specs$node_count >= 100 ~ 4.8,
    specs$node_count >= 60 ~ 5.4,
    specs$node_count >= 30 ~ 6.0,
    TRUE ~ 6.6
  )
  label_offset_size <- dplyr::case_when(
    specs$node_count >= 100 ~ 5.5,
    specs$node_count >= 60 ~ 6.0,
    specs$node_count >= 30 ~ 6.5,
    TRUE ~ 7.0
  )

  plot <- helpers$ggdraw.pedigree1(
    legacy_df,
    features = names(feature_colors),
    plot.kinship.label = FALSE,
    show.legend = TRUE,
    shape.size = label_offset_size,
    text.size = text_size
  ) +
    helpers$scale_feature.name_manual(
      values = unname(feature_colors),
      main.feature.black = FALSE,
      name = "Features"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(10, 40, 20, 20),
      legend.position = "right",
      legend.box = "vertical",
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.title = ggplot2::element_text(colour = "black"),
      legend.text = ggplot2::element_text(colour = "black")
    )

  attr(plot, "engine_used") <- "legacy"
  attr(plot, "render_specs") <- specs
  plot
}

resolve_pedigree_engine <- function(engine = c("auto", "legacy", "ggpedigree"), pedigree_data = NULL) {
  engine <- match.arg(engine)

  if (engine == "auto") {
    feature_count <- nrow(pedigree_data$feature_map %||% tibble::tibble())
    if (feature_count > 1 && legacy_pedigree_dependencies_available()) {
      return("legacy")
    }
    if (requireNamespace("ggpedigree", quietly = TRUE)) {
      return("ggpedigree")
    }
    if (legacy_pedigree_dependencies_available()) {
      return("legacy")
    }
    stop("No pedigree renderer is available. Install ggpedigree or ggped + kinship2.")
  }

  engine
}

draw_pedigree <- function(pedigree_data, interactive = FALSE, engine = c("auto", "legacy", "ggpedigree")) {
  engine <- resolve_pedigree_engine(engine = engine, pedigree_data = pedigree_data)

  switch(
    engine,
    legacy = draw_pedigree_legacy(pedigree_data),
    ggpedigree = draw_pedigree_ggpedigree(pedigree_data, interactive = interactive),
    stop("Unsupported pedigree engine.")
  )
}
