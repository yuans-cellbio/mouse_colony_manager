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

merge_import_sources <- function(softmouse_df, snapshot_df) {
  combined <- dplyr::bind_rows(softmouse_df, snapshot_df)
  if (nrow(combined) == 0) {
    return(combined)
  }

  merged <- combined |>
    dplyr::mutate(source_priority = dplyr::case_when(
      source_type == "softmouse" ~ 1L,
      source_type == "local_snapshot" ~ 2L,
      TRUE ~ 99L
    )) |>
    dplyr::arrange(source_priority, dplyr::desc(imported_at), mouse_id) |>
    dplyr::group_by(mouse_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-source_priority)

  if (nrow(snapshot_df) > 0) {
    snapshot_dates <- snapshot_df |>
      dplyr::transmute(
        mouse_id,
        snapshot_dob = dob,
        snapshot_end_date = end_date,
        snapshot_age_source = age_source
      )

    merged <- merged |>
      dplyr::left_join(snapshot_dates, by = "mouse_id") |>
      dplyr::mutate(
        dob = dplyr::if_else(
          source_type == "softmouse" & !is_plausible_mouse_date(dob) & is_plausible_mouse_date(snapshot_dob),
          snapshot_dob,
          dob
        ),
        end_date = dplyr::if_else(
          source_type == "softmouse" & !is.na(end_date) & !is_plausible_mouse_date(end_date) & is_plausible_mouse_date(snapshot_end_date),
          snapshot_end_date,
          end_date
        ),
        age_source = dplyr::coalesce(age_source, snapshot_age_source)
      ) |>
      dplyr::select(-snapshot_dob, -snapshot_end_date, -snapshot_age_source)
  }

  age_columns <- compute_age_columns(merged$dob, merged$end_date, age_source = merged$age_source)
  merged |>
    dplyr::mutate(
      age_days = age_columns$age_days,
      age_weeks = age_columns$age_weeks,
      age_label = age_columns$age_label
    )
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
