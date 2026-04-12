genotype_column_names <- function(df) {
  grep("^geno_", names(df), value = TRUE)
}

normalize_filter_values <- function(x) {
  x <- trim_na(x)
  x <- stats::na.omit(x)
  as.character(x)
}

first_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  x[[1]]
}

text_matches <- function(values, query, fixed = FALSE, ignore_case = TRUE) {
  values <- dplyr::coalesce(as.character(values), "")

  if (fixed) {
    grepl(query, values, fixed = TRUE, ignore.case = ignore_case)
  } else {
    grepl(query, values, perl = TRUE, ignore.case = ignore_case)
  }
}

apply_mouse_filters <- function(df, filters = list()) {
  if (nrow(df) == 0) {
    return(df)
  }

  out <- tibble::as_tibble(df)

  id_query <- first_or_na(normalize_filter_values(filters$id_query))
  if (!is.na(id_query)) {
    if (identical(filters$id_mode, "exact")) {
      out <- out |> dplyr::filter(mouse_id == id_query)
    } else {
      out <- out |> dplyr::filter(text_matches(mouse_id, id_query, fixed = TRUE))
    }
  }

  genotype_query <- first_or_na(normalize_filter_values(filters$genotype_query))
  if (!is.na(genotype_query)) {
    out <- out |> dplyr::filter(text_matches(raw_genotype, genotype_query))
  }

  genotype_gene <- first_or_na(normalize_filter_values(filters$genotype_gene))
  genotype_value <- first_or_na(normalize_filter_values(filters$genotype_value))
  if (!is.na(genotype_gene) && genotype_gene %in% names(out) && !is.na(genotype_value)) {
    out <- out |> dplyr::filter(dplyr::coalesce(as.character(.data[[genotype_gene]]), "") == genotype_value)
  }

  sex_values <- normalize_filter_values(filters$sex)
  if (length(sex_values) > 0) {
    out <- out |> dplyr::filter(sex %in% sex_values)
  }

  if (!is.null(filters$dob_range) && length(filters$dob_range) == 2 && !all(is.na(filters$dob_range))) {
    dob_range <- coerce_date_column(filters$dob_range)
    out <- out |> dplyr::filter(is.na(dob) | (dob >= dob_range[[1]] & dob <= dob_range[[2]]))
  }

  if (!is.null(filters$age_range) && length(filters$age_range) == 2) {
    out <- out |> dplyr::filter(is.na(age_weeks) | (age_weeks >= filters$age_range[[1]] & age_weeks <= filters$age_range[[2]]))
  }

  mouse_lines <- normalize_filter_values(filters$mouse_line)
  if (length(mouse_lines) > 0) {
    out <- out |> dplyr::filter(mouse_line %in% mouse_lines)
  }

  generations <- normalize_filter_values(filters$generation)
  if (length(generations) > 0) {
    out <- out |> dplyr::filter(generation %in% generations)
  }

  alive_filter <- filters$alive_filter %||% "all"
  if (alive_filter == "live") {
    out <- out |> dplyr::filter(alive %in% TRUE)
  } else if (alive_filter == "ended") {
    out <- out |> dplyr::filter(!alive %in% TRUE)
  }

  breeder_filter <- filters$breeder_filter %||% "all"
  if (breeder_filter == "yes") {
    out <- out |> dplyr::filter(is_breeder %in% TRUE)
  } else if (breeder_filter == "no") {
    out <- out |> dplyr::filter(!is_breeder %in% TRUE)
  }

  ready_filter <- filters$ready_filter %||% "all"
  if (ready_filter == "yes") {
    out <- out |> dplyr::filter(experiment_ready %in% TRUE)
  } else if (ready_filter == "no") {
    out <- out |> dplyr::filter(!experiment_ready %in% TRUE)
  }

  reservation_filter <- first_or_na(normalize_filter_values(filters$reservation_filter))
  if (!is.na(reservation_filter) && reservation_filter != "all") {
    out <- out |> dplyr::filter(dplyr::coalesce(reservation_status, "available") == reservation_filter)
  }

  cohort_filter <- first_or_na(normalize_filter_values(filters$cohort_filter))
  if (!is.na(cohort_filter)) {
    out <- out |> dplyr::filter(cohort_label == cohort_filter)
  }

  flag_query <- first_or_na(normalize_filter_values(filters$flag_query))
  if (!is.na(flag_query)) {
    out <- out |> dplyr::filter(text_matches(local_flags, flag_query))
  }

  out
}
