required_source_columns <- c(
  "Physical.Tag", "Sex", "Date.of.Birth", "End.Date", "End.Type", "Age",
  "State", "Genotype", "Mouseline", "Generation", "Sire.Tag", "Dam.Tag",
  "Mating.Partner's.Tag"
)

optional_source_columns <- c("Alt..ID", "Mouse.SID", "1st.Gene", "2nd.Gene", "Protocol", "Comment")

normalize_source_frame <- function(df) {
  df <- tibble::as_tibble(df)

  for (col in c(required_source_columns, optional_source_columns)) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }

  df
}

extract_genotype_keys <- function(genotype_values) {
  genotype_values |>
    trim_na() |>
    stringr::str_split(",\\s*") |>
    unlist(use.names = FALSE) |>
    trim_na() |>
    stats::na.omit() |>
    stringr::str_replace("\\(.*\\)", "") |>
    trim_na() |>
    unique() |>
    sort()
}

extract_genotype_component <- function(genotype_value, gene_name) {
  genotype_value <- trim_na(genotype_value)
  gene_name <- trim_na(gene_name)

  if (is.na(genotype_value) || is.na(gene_name)) {
    return(NA_character_)
  }

  escaped_gene <- stringr::str_replace_all(gene_name, "([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1")
  match_value <- stringr::str_extract(genotype_value, paste0("(?<=", escaped_gene, "\\().*?(?=\\))"))

  if (is.na(match_value) || match_value == "") {
    return(NA_character_)
  }

  match_value |>
    stringr::str_split("/") |>
    purrr::pluck(1) |>
    trim_na() |>
    stats::na.omit() |>
    sort() |>
    paste(collapse = "/") |>
    trim_na()
}

add_parsed_genotype_columns <- function(df) {
  genes <- extract_genotype_keys(df$raw_genotype)
  genotype_columns <- character(0)

  if (length(genes) == 0) {
    attr(df, "genotype_columns") <- genotype_columns
    return(df)
  }

  for (gene in genes) {
    column_name <- paste0("geno_", sanitize_name(gene))
    df[[column_name]] <- vapply(df$raw_genotype, extract_genotype_component, character(1), gene_name = gene)
    genotype_columns <- c(genotype_columns, column_name)
  }

  attr(df, "genotype_columns") <- genotype_columns
  df
}

compute_age_columns <- function(dob, end_date, age_source = NULL, reference_date = Sys.Date(),
                                prefer_age_source = NULL) {
  end_or_today <- dplyr::if_else(is.na(end_date), reference_date, end_date)
  age_days <- suppressWarnings(as.integer(end_or_today - dob))
  age_days[is.na(dob)] <- NA_integer_

  age_source_weeks <- parse_age_source_weeks(age_source)
  if (is.null(prefer_age_source)) {
    prefer_age_source <- rep(FALSE, length(age_days))
  }
  use_age_source <- (prefer_age_source | !is_plausible_mouse_age_days(age_days)) & !is.na(age_source_weeks)

  if (any(use_age_source)) {
    age_days[use_age_source] <- as.integer(round(age_source_weeks[use_age_source] * 7))
  }

  age_weeks <- round(age_days / 7, 1)
  age_label <- ifelse(is.na(age_weeks), NA_character_, paste0(format(age_weeks, nsmall = 1, trim = TRUE), " wk"))

  list(age_days = age_days, age_weeks = age_weeks, age_label = age_label)
}

normalize_mouse_records <- function(df, source_path, source_type, imported_at = Sys.time()) {
  df <- normalize_source_frame(df)
  dob <- coerce_date_column(df$Date.of.Birth)
  end_date <- coerce_date_column(df$End.Date)
  dob[!is_plausible_mouse_date(dob)] <- as.Date(NA)
  end_date[!is.na(end_date) & !is_plausible_mouse_date(end_date)] <- as.Date(NA)
  state <- trim_na(df$State)
  alive <- ifelse(!is.na(end_date), FALSE, !tolower(state %||% "") %in% c("ended", "dead", "euthanized"))
  age_columns <- compute_age_columns(
    dob,
    end_date,
    age_source = df$Age,
    prefer_age_source = !alive & is.na(end_date)
  )

  normalized <- tibble::tibble(
    source_type = source_type,
    source_path = normalizePath(source_path, winslash = "/", mustWork = FALSE),
    source_file = basename(source_path),
    imported_at = as.POSIXct(imported_at, tz = "UTC"),
    mouse_id = normalize_mouse_id(df$Physical.Tag),
    alt_id = normalize_mouse_id(df$Alt..ID),
    mouse_sid = normalize_mouse_id(df$Mouse.SID),
    sex = normalize_sex(df$Sex),
    dob = dob,
    end_date = end_date,
    end_type = trim_na(df$End.Type),
    age_source = trim_na(df$Age),
    age_days = age_columns$age_days,
    age_weeks = age_columns$age_weeks,
    age_label = age_columns$age_label,
    status = state,
    alive = alive,
    raw_genotype = stringr::str_to_title(trim_na(df$Genotype)),
    mouse_line = trim_na(df$Mouseline),
    generation = trim_na(df$Generation),
    first_gene = trim_na(df$`1st.Gene`),
    second_gene = trim_na(df$`2nd.Gene`),
    protocol = trim_na(df$Protocol),
    sire_id = normalize_mouse_id(df$Sire.Tag),
    dam_id = normalize_mouse_id(df$Dam.Tag),
    mate_id = normalize_mouse_id(df$`Mating.Partner's.Tag`),
    source_comment = trim_na(df$Comment),
    founder = is.na(normalize_mouse_id(df$Sire.Tag)) & is.na(normalize_mouse_id(df$Dam.Tag))
  ) |>
    dplyr::filter(!is.na(mouse_id))

  add_parsed_genotype_columns(normalized)
}

import_softmouse <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble::tibble())
  }

  df <- openxlsx::read.xlsx(path, detectDates = TRUE)
  normalize_mouse_records(df, source_path = path, source_type = "softmouse")
}

import_colony_snapshot <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble::tibble())
  }

  df <- openxlsx::read.xlsx(path, detectDates = TRUE)
  normalize_mouse_records(df, source_path = path, source_type = "local_snapshot")
}

merge_conflict_fields <- function() {
  c(
    "alt_id", "mouse_sid", "sex", "dob", "end_date", "end_type",
    "status", "raw_genotype", "mouse_line", "generation",
    "first_gene", "second_gene", "protocol", "sire_id", "dam_id",
    "mate_id", "source_comment"
  )
}

merge_record_fields <- function() {
  c("mouse_id", merge_conflict_fields(), "age_source")
}

normalize_merge_source_df <- function(df) {
  if (nrow(df) == 0) {
    return(empty_mouse_records())
  }

  row_count <- nrow(df)
  normalized <- dplyr::bind_rows(df, empty_mouse_records()) |>
    dplyr::slice_head(n = row_count)

  if ("dob" %in% names(normalized)) {
    normalized$dob <- coerce_date_column(normalized$dob)
    normalized$dob[!is_plausible_mouse_date(normalized$dob)] <- as.Date(NA)
  }

  if ("end_date" %in% names(normalized)) {
    normalized$end_date <- coerce_date_column(normalized$end_date)
    normalized$end_date[!is.na(normalized$end_date) & !is_plausible_mouse_date(normalized$end_date)] <- as.Date(NA)
  }

  keep_cols <- unique(c(
    "source_type", "source_path", "source_file", "imported_at",
    merge_record_fields()
  ))

  normalized |>
    dplyr::select(dplyr::all_of(intersect(keep_cols, names(normalized))))
}

normalize_compare_scalar <- function(x) {
  if (inherits(x, "Date")) {
    return(if (is.na(x)) NA_character_ else format(as.Date(x), "%Y-%m-%d"))
  }

  if (inherits(x, "POSIXt")) {
    return(if (is.na(x)) NA_character_ else format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }

  trim_na(as.character(x))
}

scalar_is_missing <- function(x) {
  is.na(normalize_compare_scalar(x))
}

scalar_values_equal <- function(x, y) {
  identical(normalize_compare_scalar(x), normalize_compare_scalar(y))
}

detect_import_conflicts <- function(softmouse_df, snapshot_df) {
  softmouse_df <- normalize_merge_source_df(softmouse_df)
  snapshot_df <- normalize_merge_source_df(snapshot_df)

  if (nrow(softmouse_df) == 0 || nrow(snapshot_df) == 0) {
    return(tibble::tibble(
      mouse_id = character(),
      field = character(),
      softmouse_value = character(),
      snapshot_value = character()
    ))
  }

  overlapping_ids <- intersect(softmouse_df$mouse_id, snapshot_df$mouse_id)
  if (length(overlapping_ids) == 0) {
    return(tibble::tibble(
      mouse_id = character(),
      field = character(),
      softmouse_value = character(),
      snapshot_value = character()
    ))
  }

  soft_lookup <- split(softmouse_df, softmouse_df$mouse_id)
  snap_lookup <- split(snapshot_df, snapshot_df$mouse_id)

  purrr::map_dfr(overlapping_ids, function(mouse_id) {
    soft_row <- soft_lookup[[mouse_id]][1, , drop = FALSE]
    snap_row <- snap_lookup[[mouse_id]][1, , drop = FALSE]

    purrr::map_dfr(merge_conflict_fields(), function(field) {
      soft_value <- soft_row[[field]][[1]]
      snap_value <- snap_row[[field]][[1]]

      if (scalar_is_missing(soft_value) || scalar_is_missing(snap_value) || scalar_values_equal(soft_value, snap_value)) {
        return(NULL)
      }

      tibble::tibble(
        mouse_id = mouse_id,
        field = field,
        softmouse_value = normalize_compare_scalar(soft_value),
        snapshot_value = normalize_compare_scalar(snap_value)
      )
    })
  })
}

normalize_conflict_resolutions <- function(conflict_resolutions = NULL) {
  if (is.null(conflict_resolutions) || nrow(conflict_resolutions) == 0) {
    return(tibble::tibble(
      mouse_id = character(),
      field = character(),
      chosen_source = character()
    ))
  }

  tibble::as_tibble(conflict_resolutions) |>
    dplyr::transmute(
      mouse_id = normalize_mouse_id(mouse_id),
      field = trim_na(field),
      chosen_source = trim_na(chosen_source)
    ) |>
    dplyr::filter(!is.na(mouse_id), !is.na(field), !is.na(chosen_source)) |>
    dplyr::distinct(mouse_id, field, .keep_all = TRUE)
}

default_conflict_resolutions <- function(conflicts, default_source = "softmouse") {
  conflicts <- tibble::as_tibble(conflicts)
  if (nrow(conflicts) == 0) {
    return(normalize_conflict_resolutions(NULL))
  }

  normalize_conflict_resolutions(
    conflicts |>
      dplyr::transmute(
        mouse_id = mouse_id,
        field = field,
        chosen_source = default_source
      )
  )
}

find_unresolved_import_conflicts <- function(conflicts, conflict_resolutions = NULL) {
  conflicts <- tibble::as_tibble(conflicts)
  if (nrow(conflicts) == 0) {
    return(conflicts)
  }

  resolutions <- normalize_conflict_resolutions(conflict_resolutions)
  if (nrow(resolutions) == 0) {
    return(conflicts)
  }

  unresolved <- conflicts |>
    dplyr::left_join(resolutions, by = c("mouse_id", "field")) |>
    dplyr::filter(is.na(chosen_source) | !chosen_source %in% c("softmouse", "local_snapshot"))

  unresolved |>
    dplyr::select(mouse_id, field, softmouse_value, snapshot_value)
}

resolve_field_value <- function(soft_value, snapshot_value, chosen_source = NA_character_, default_conflict_source = "softmouse") {
  if (scalar_is_missing(soft_value) && scalar_is_missing(snapshot_value)) {
    return(snapshot_value %||% soft_value)
  }

  if (scalar_is_missing(soft_value)) {
    return(snapshot_value)
  }

  if (scalar_is_missing(snapshot_value) || scalar_values_equal(soft_value, snapshot_value)) {
    return(soft_value)
  }

  chosen_source <- trim_na(chosen_source) %||% default_conflict_source
  if (identical(chosen_source, "local_snapshot")) {
    return(snapshot_value)
  }

  soft_value
}

build_merged_mouse_row <- function(mouse_id, soft_row = NULL, snapshot_row = NULL,
                                   resolutions = NULL, default_conflict_source = "softmouse") {
  soft_present <- !is.null(soft_row) && nrow(soft_row) > 0
  snapshot_present <- !is.null(snapshot_row) && nrow(snapshot_row) > 0

  soft_row <- if (soft_present) normalize_merge_source_df(soft_row)[1, , drop = FALSE] else empty_mouse_records()[0, ]
  snapshot_row <- if (snapshot_present) normalize_merge_source_df(snapshot_row)[1, , drop = FALSE] else empty_mouse_records()[0, ]

  value_for <- function(field) {
    soft_value <- if (soft_present && field %in% names(soft_row)) soft_row[[field]][[1]] else NA
    snapshot_value <- if (snapshot_present && field %in% names(snapshot_row)) snapshot_row[[field]][[1]] else NA
    chosen_match <- resolutions$chosen_source[resolutions$mouse_id == mouse_id & resolutions$field == field]
    chosen_source <- if (length(chosen_match) == 0) NA_character_ else chosen_match[[1]]
    resolve_field_value(soft_value, snapshot_value, chosen_source = chosen_source, default_conflict_source = default_conflict_source)
  }

  source_type <- dplyr::case_when(
    soft_present && snapshot_present ~ "merged_import",
    soft_present ~ "softmouse",
    snapshot_present ~ "local_snapshot",
    TRUE ~ "merged_import"
  )

  source_path <- paste(stats::na.omit(c(
    if (soft_present) soft_row$source_path[[1]] else NA_character_,
    if (snapshot_present) snapshot_row$source_path[[1]] else NA_character_
  )), collapse = " | ")
  source_path <- trim_na(source_path)

  source_file <- paste(stats::na.omit(c(
    if (soft_present) soft_row$source_file[[1]] else NA_character_,
    if (snapshot_present) snapshot_row$source_file[[1]] else NA_character_
  )), collapse = " | ")
  source_file <- trim_na(source_file)

  imported_at_candidates <- stats::na.omit(as.POSIXct(c(
    if (soft_present) soft_row$imported_at[[1]] else as.POSIXct(NA),
    if (snapshot_present) snapshot_row$imported_at[[1]] else as.POSIXct(NA)
  ), tz = "UTC"))
  imported_at <- if (length(imported_at_candidates) == 0) {
    as.POSIXct(Sys.time(), tz = "UTC")
  } else {
    max(imported_at_candidates)
  }

  dob <- value_for("dob")
  end_date <- value_for("end_date")
  status <- value_for("status")
  age_source <- value_for("age_source")
  alive <- ifelse(!is.na(end_date), FALSE, !tolower(status %||% "") %in% c("ended", "dead", "euthanized"))
  age_columns <- compute_age_columns(
    dob = as.Date(dob),
    end_date = as.Date(end_date),
    age_source = age_source,
    prefer_age_source = !alive & is.na(end_date)
  )

  tibble::tibble(
    source_type = source_type,
    source_path = source_path,
    source_file = source_file,
    imported_at = as.POSIXct(imported_at, tz = "UTC"),
    mouse_id = mouse_id,
    alt_id = value_for("alt_id"),
    mouse_sid = value_for("mouse_sid"),
    sex = value_for("sex"),
    dob = as.Date(dob),
    end_date = as.Date(end_date),
    end_type = value_for("end_type"),
    age_source = age_source,
    age_days = age_columns$age_days,
    age_weeks = age_columns$age_weeks,
    age_label = age_columns$age_label,
    status = status,
    alive = alive,
    raw_genotype = value_for("raw_genotype"),
    mouse_line = value_for("mouse_line"),
    generation = value_for("generation"),
    first_gene = value_for("first_gene"),
    second_gene = value_for("second_gene"),
    protocol = value_for("protocol"),
    sire_id = value_for("sire_id"),
    dam_id = value_for("dam_id"),
    mate_id = value_for("mate_id"),
    source_comment = value_for("source_comment"),
    founder = is.na(value_for("sire_id")) & is.na(value_for("dam_id"))
  )
}

merge_import_sources <- function(softmouse_df, snapshot_df, conflict_resolutions = NULL,
                                 require_resolved = FALSE, default_conflict_source = "softmouse") {
  softmouse_df <- normalize_merge_source_df(softmouse_df)
  snapshot_df <- normalize_merge_source_df(snapshot_df)
  combined <- dplyr::bind_rows(softmouse_df, snapshot_df)
  if (nrow(combined) == 0) {
    return(combined)
  }

  conflicts <- detect_import_conflicts(softmouse_df, snapshot_df)
  unresolved <- find_unresolved_import_conflicts(conflicts, conflict_resolutions)
  if (require_resolved && nrow(unresolved) > 0) {
    stop("Resolve all import conflicts before merging.", call. = FALSE)
  }

  resolutions <- normalize_conflict_resolutions(conflict_resolutions)
  all_ids <- sort(unique(stats::na.omit(combined$mouse_id)))
  soft_lookup <- split(softmouse_df, softmouse_df$mouse_id)
  snap_lookup <- split(snapshot_df, snapshot_df$mouse_id)

  merged <- purrr::map_dfr(all_ids, function(mouse_id) {
    build_merged_mouse_row(
      mouse_id = mouse_id,
      soft_row = soft_lookup[[mouse_id]],
      snapshot_row = snap_lookup[[mouse_id]],
      resolutions = resolutions,
      default_conflict_source = default_conflict_source
    )
  })

  add_parsed_genotype_columns(merged)
}

empty_mouse_records <- function() {
  tibble::tibble(
    source_type = character(),
    source_path = character(),
    source_file = character(),
    imported_at = as.POSIXct(character(), tz = "UTC"),
    mouse_id = character(),
    alt_id = character(),
    mouse_sid = character(),
    sex = character(),
    dob = as.Date(character()),
    end_date = as.Date(character()),
    end_type = character(),
    age_source = character(),
    age_days = numeric(),
    age_weeks = numeric(),
    age_label = character(),
    status = character(),
    alive = logical(),
    raw_genotype = character(),
    mouse_line = character(),
    generation = character(),
    first_gene = character(),
    second_gene = character(),
    protocol = character(),
    sire_id = character(),
    dam_id = character(),
    mate_id = character(),
    source_comment = character(),
    founder = logical()
  )
}
