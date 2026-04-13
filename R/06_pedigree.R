pedigree_feature_columns <- function(df) {
  genotype_column_names(df)
}

empty_pedigree_feature_catalog <- function() {
  tibble::tibble(
    source_field = character(),
    display_name = character(),
    values = list(),
    base_color = character()
  )
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

  width_in <- max(14, min(84, 8 + leaf_count * 0.72 + max_label_lines * 0.9))
  height_in <- max(10, min(48, 4 + generation_span * 2.6 + max_label_lines * 0.4))
  ped_width <- max(15, min(100, leaf_count * 0.9 + 6))

  list(
    node_count = nrow(df),
    leaf_count = leaf_count,
    generation_span = generation_span,
    max_label_lines = max_label_lines,
    width_in = width_in,
    height_in = height_in,
    plot_width_px = as.integer(round(width_in * 110)),
    plot_height_px = as.integer(round(height_in * 110)),
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
  specs$plot_height_px <- as.integer(round(height_in * 110))
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

  "#2A6F63"
}

subset_pedigree_source_rows <- function(df, mouse_ids) {
  mouse_ids <- normalize_mouse_id(mouse_ids)
  mouse_ids <- unique(stats::na.omit(mouse_ids))

  if (length(mouse_ids) == 0) {
    return(tibble::as_tibble(df)[0, , drop = FALSE])
  }

  tibble::as_tibble(df) |>
    dplyr::filter(mouse_id %in% mouse_ids) |>
    dplyr::distinct(mouse_id, .keep_all = TRUE)
}

pedigree_available_features <- function(df, mouse_ids = NULL) {
  df <- tibble::as_tibble(df)

  if (!is.null(mouse_ids)) {
    df <- subset_pedigree_source_rows(df, mouse_ids)
  }

  if (nrow(df) == 0) {
    return(empty_pedigree_feature_catalog())
  }

  feature_cols <- pedigree_feature_columns(df)
  if (length(feature_cols) == 0) {
    return(empty_pedigree_feature_catalog())
  }

  available_cols <- feature_cols[vapply(feature_cols, function(col) {
    any(!is.na(trim_na(df[[col]])))
  }, logical(1))]

  if (length(available_cols) == 0) {
    return(empty_pedigree_feature_catalog())
  }

  display_names <- make.unique(vapply(available_cols, pretty_pedigree_feature_label, character(1)), sep = " ")

  tibble::tibble(
    source_field = available_cols,
    display_name = display_names,
    values = lapply(available_cols, function(col) sort(unique(stats::na.omit(trim_na(df[[col]]))))),
    base_color = vapply(
      available_cols,
      function(col) infer_pedigree_feature_color(feature_name = col, feature_values = df[[col]]),
      character(1)
    )
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

  df$sex <- dplyr::coalesce(trim_na(df$sex), inferred, "U")
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

default_pedigree_discrete_palette <- function(feature_name, values, background_hex = "#FFFFFF") {
  values <- trim_na(values)
  values <- unique(stats::na.omit(values))

  if (length(values) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  base_color <- infer_pedigree_feature_color(feature_name = feature_name, feature_values = values)
  intensities <- feature_to_numeric(values)
  if (all(is.na(intensities))) {
    intensities <- seq(0.25, 1, length.out = length(values))
  }

  colours <- vapply(intensities, function(intensity) {
    if (is.na(intensity)) {
      return("#D8D2C6")
    }
    blend_hex_with_background(base_color, alpha = intensity, background_hex = background_hex)
  }, character(1))

  stats::setNames(colours, values)
}

build_pedigree_feature_scales <- function(plot_df, feature_catalog, selected_fields, color_overrides = list()) {
  selected_fields <- intersect(selected_fields, feature_catalog$source_field)
  if (length(selected_fields) == 0) {
    stop("Choose at least one gene to color before drawing the pedigree.")
  }
  if (length(selected_fields) > 4) {
    stop("The local ggped renderer supports at most 4 genes per pedigree.")
  }

  ggped_api <- get_local_ggped()
  scale_specs <- vector("list", length(selected_fields))
  names(scale_specs) <- selected_fields

  for (field in selected_fields) {
    row_idx <- match(field, feature_catalog$source_field)
    if (is.na(row_idx) || !field %in% names(plot_df)) {
      next
    }

    values_present <- sort(unique(stats::na.omit(trim_na(plot_df[[field]]))))
    default_values <- default_pedigree_discrete_palette(field, values_present)
    override <- color_overrides[[field]] %||% list()
    override_values <- override$values %||% stats::setNames(character(0), character(0))

    if (length(override_values) > 0) {
      matched_names <- intersect(names(default_values), names(override_values))
      default_values[matched_names] <- override_values[matched_names]
    }

    legend_name <- trim_na(override$name %||% feature_catalog$display_name[[row_idx]]) %||% feature_catalog$display_name[[row_idx]]
    na_value <- trim_na(override$na.value %||% "#D8D2C6") %||% "#D8D2C6"

    scale_specs[[field]] <- ggped_api$scale_feature_discrete(
      values = default_values,
      na.value = na_value,
      name = legend_name
    )
  }

  scale_specs[!vapply(scale_specs, is.null, logical(1))]
}

local_ggped_dependencies_available <- function() {
  required <- c("ggplot2", "kinship2", "cli", "gtable")
  all(vapply(required, requireNamespace, logical(1), quietly = TRUE))
}

local_ggped_state <- local({
  env <- new.env(parent = emptyenv())
  env$api <- NULL
  env
})

find_local_ggped_path <- function() {
  candidates <- c(
    "ggped",
    file.path(".", "ggped"),
    file.path("..", "ggped"),
    file.path("..", "..", "ggped")
  )

  for (candidate in unique(candidates)) {
    if (dir.exists(candidate) &&
      file.exists(file.path(candidate, "DESCRIPTION")) &&
      dir.exists(file.path(candidate, "R"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate the local ./ggped package folder.")
}

get_local_ggped <- function() {
  if (!is.null(local_ggped_state$api)) {
    return(local_ggped_state$api)
  }

  if (!local_ggped_dependencies_available()) {
    stop("Local ggped rendering requires ggplot2, kinship2, cli, and gtable.")
  }

  ggped_dir <- find_local_ggped_path()
  api_env <- new.env(parent = baseenv())

  bindings <- list(
    ggplot = ggplot2::ggplot,
    aes = ggplot2::aes,
    geom_line = ggplot2::geom_line,
    geom_segment = ggplot2::geom_segment,
    geom_text = ggplot2::geom_text,
    geom_point = ggplot2::geom_point,
    geom_tile = ggplot2::geom_tile,
    scale_shape_manual = ggplot2::scale_shape_manual,
    scale_fill_gradientn = ggplot2::scale_fill_gradientn,
    scale_fill_manual = ggplot2::scale_fill_manual,
    guide_colorbar = ggplot2::guide_colorbar,
    guide_legend = ggplot2::guide_legend,
    theme_void = ggplot2::theme_void,
    theme = ggplot2::theme,
    element_text = ggplot2::element_text,
    ggproto = ggplot2::ggproto,
    Geom = ggplot2::Geom,
    layer = ggplot2::layer,
    setNames = stats::setNames,
    gtable = gtable::gtable,
    gtable_add_grob = gtable::gtable_add_grob,
    align.pedigree = kinship2::align.pedigree,
    kinship = kinship2::kinship
  )

  list2env(bindings, envir = api_env)

  source_files <- c(
    "segmentation.R",
    "scale_feature.R",
    "geom_pedigreepoint.R",
    "dfalign.pedigree.R",
    "legend_builder.R",
    "ggdraw.pedigree.R"
  )

  for (file_name in source_files) {
    sys.source(file.path(ggped_dir, "R", file_name), envir = api_env, keep.source = FALSE)
  }

  local_ggped_state$api <- api_env
  api_env
}

build_local_ggped_df <- function(source_df, render_specs = NULL) {
  ggped_api <- get_local_ggped()
  source_df <- tibble::as_tibble(source_df)
  render_specs <- render_specs %||% compute_pedigree_render_specs(source_df)

  if (nrow(source_df) == 0) {
    return(source_df)
  }

  sex_codes <- dplyr::case_when(
    source_df$sex %in% c("M", "male", "Male", "1") ~ 1L,
    source_df$sex %in% c("F", "female", "Female", "2") ~ 2L,
    TRUE ~ 0L
  )

  pedigree_obj <- with(
    source_df,
    kinship2::pedigree(
      id = personID,
      dadid = dadID,
      momid = momID,
      sex = sex_codes,
      status = ifelse(alive %in% TRUE, 0, 1)
    )
  )

  aligned <- ggped_api$dfalign.pedigree(
    ped = pedigree_obj,
    width = max(10, round(render_specs$ped_width)),
    packed = TRUE,
    align = TRUE
  ) |>
    dplyr::rename(
      aligned_name = Name,
      aligned_sex = sex,
      aligned_status = status
    )

  source_meta <- source_df |>
    dplyr::rename(status_text = status)

  aligned |>
    dplyr::left_join(source_meta, by = dplyr::join_by(aligned_name == personID)) |>
    dplyr::mutate(
      Name = dplyr::coalesce(label, aligned_name),
      sex = dplyr::coalesce(
        normalize_sex(sex),
        dplyr::case_when(
          aligned_sex %in% c(1, "1", "M", "male", "Male") ~ "M",
          aligned_sex %in% c(2, "2", "F", "female", "Female") ~ "F",
          TRUE ~ "U"
        )
      ),
      status = ifelse(alive %in% TRUE, 0, 1),
      adopted = FALSE,
      pregnancy = FALSE
    )
}

build_pedigree_data <- function(df, mouse_ids, label_fields = c("mouse_id", "age_label", "raw_genotype"),
                                feature_fields = character(0), render_overrides = NULL) {
  source_rows <- subset_pedigree_source_rows(df, mouse_ids)

  if (nrow(source_rows) == 0) {
    return(list(
      data = source_rows,
      source_data = source_rows,
      feature_fields = character(0),
      label_fields = label_fields,
      render_specs = apply_pedigree_render_overrides(compute_pedigree_render_specs(source_rows), render_overrides),
      feature_map = empty_pedigree_feature_catalog()
    ))
  }

  source_rows <- source_rows |>
    add_placeholder_parents() |>
    normalize_pedigree_sex() |>
    dplyr::mutate(
      famID = 1L,
      personID = mouse_id,
      momID = dam_id,
      dadID = sire_id,
      spouseID = mate_id,
      status_flag = ifelse(alive %in% TRUE, 0, 1)
    )

  source_rows$label <- build_pedigree_label(source_rows, label_fields)

  render_specs <- compute_pedigree_render_specs(source_rows)
  render_specs <- apply_pedigree_render_overrides(render_specs, render_overrides)

  available_features <- pedigree_available_features(source_rows)
  selected_features <- intersect(feature_fields, available_features$source_field)
  feature_map <- if (length(selected_features) == 0) {
    empty_pedigree_feature_catalog()
  } else {
    available_features[match(selected_features, available_features$source_field), , drop = FALSE]
  }

  plot_df <- build_local_ggped_df(source_rows, render_specs = render_specs)

  list(
    data = plot_df,
    source_data = source_rows,
    feature_fields = selected_features,
    label_fields = label_fields,
    render_specs = render_specs,
    feature_map = feature_map
  )
}

draw_pedigree <- function(pedigree_data, color_overrides = list()) {
  ggped_api <- get_local_ggped()
  plot_df <- pedigree_data$data

  if (nrow(plot_df) < 2) {
    stop("Pedigree drawing requires at least two related entries after placeholder expansion.")
  }

  feature_scales <- build_pedigree_feature_scales(
    plot_df = plot_df,
    feature_catalog = pedigree_data$feature_map %||% empty_pedigree_feature_catalog(),
    selected_fields = pedigree_data$feature_fields %||% character(0),
    color_overrides = color_overrides
  )

  specs <- pedigree_data$render_specs %||% compute_pedigree_render_specs(pedigree_data$source_data %||% plot_df)
  point_size <- dplyr::case_when(
    specs$node_count >= 180 ~ 4.8,
    specs$node_count >= 120 ~ 5.5,
    specs$node_count >= 80 ~ 6.0,
    TRUE ~ 7.0
  )

  plot_obj <- ggped_api$ggdraw.pedigree(
    dat = plot_df,
    features = feature_scales,
    plot.names = TRUE,
    plot.kinship.label = FALSE,
    allow.repeated = TRUE,
    width = max(10, round(specs$ped_width)),
    size = point_size
  )

  plot_obj$plot <- plot_obj$plot +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(12, 18, 12, 18)
    )

  attr(plot_obj, "engine_used") <- "local_ggped"
  attr(plot_obj, "render_specs") <- specs
  plot_obj
}

render_pedigree_plot <- function(plot_obj) {
  ggped_api <- get_local_ggped()
  ggped_api$.draw_ggped(plot_obj)
}

save_pedigree_plot <- function(plot_obj, filename, width, height, dpi = 300) {
  ggped_api <- get_local_ggped()
  ggped_api$save_ggped(plot_obj, filename = filename, width = width, height = height, dpi = dpi)
}