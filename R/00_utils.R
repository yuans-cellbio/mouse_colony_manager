`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

trim_na <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "Na", "Null", "NULL")] <- NA_character_
  x
}

normalize_mouse_id <- function(x) {
  trim_na(x)
}

normalize_sex <- function(x) {
  x <- trim_na(x)
  x <- toupper(substr(x, 1, 1))
  dplyr::case_when(
    x %in% c("M", "1") ~ "M",
    x %in% c("F", "2") ~ "F",
    TRUE ~ NA_character_
  )
}

coerce_date_column <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  parse_one <- function(value) {
    if (inherits(value, "Date")) {
      return(as.Date(value))
    }

    if (inherits(value, "POSIXt")) {
      return(as.Date(value))
    }

    value_chr <- trim_na(value)
    if (is.na(value_chr)) {
      return(as.Date(NA))
    }

    if (grepl("^[0-9]+(\\.[0-9]+)?$", value_chr)) {
      return(as.Date(as.numeric(value_chr), origin = "1899-12-30"))
    }

    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", value_chr)) {
      return(suppressWarnings(as.Date(value_chr, format = "%Y-%m-%d")))
    }

    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}[ T]\\d{2}:\\d{2}:\\d{2}$", value_chr)) {
      parsed_dt <- suppressWarnings(as.POSIXct(
        value_chr,
        tz = "UTC",
        tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S")
      ))
      if (!is.na(parsed_dt)) {
        return(as.Date(parsed_dt))
      }
    }

    if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", value_chr)) {
      return(suppressWarnings(as.Date(value_chr, format = "%m/%d/%Y")))
    }

    if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", value_chr)) {
      return(suppressWarnings(as.Date(value_chr, format = "%m-%d-%Y")))
    }

    if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", value_chr)) {
      return(suppressWarnings(as.Date(value_chr, format = "%m/%d/%y")))
    }

    if (grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", value_chr)) {
      return(suppressWarnings(as.Date(value_chr, format = "%m-%d-%y")))
    }

    as.Date(NA)
  }

  as.Date(vapply(as.list(x), parse_one, as.Date(NA)), origin = "1970-01-01")
}

parse_age_source_weeks <- function(x) {
  x <- trim_na(x)
  out <- rep(NA_real_, length(x))

  week_idx <- !is.na(x) & grepl("^\\d+(\\.\\d+)?\\s*(w|wk|wks|week|weeks)$", x, ignore.case = TRUE)
  if (any(week_idx)) {
    out[week_idx] <- suppressWarnings(as.numeric(sub("^\\s*(\\d+(?:\\.\\d+)?).*$", "\\1", x[week_idx], perl = TRUE)))
  }

  day_idx <- !is.na(x) & grepl("^\\d+(\\.\\d+)?\\s*(d|day|days)$", x, ignore.case = TRUE)
  if (any(day_idx)) {
    out[day_idx] <- suppressWarnings(as.numeric(sub("^\\s*(\\d+(?:\\.\\d+)?).*$", "\\1", x[day_idx], perl = TRUE))) / 7
  }

  out
}

is_plausible_mouse_age_days <- function(x, max_days = 3650) {
  !is.na(x) & x >= 0 & x <= max_days
}

is_plausible_mouse_date <- function(x, min_date = as.Date("2000-01-01"), max_date = Sys.Date() + 365) {
  !is.na(x) & x >= min_date & x <= max_date
}

coerce_datetime_column <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  x_chr <- trim_na(x)
  suppressWarnings(as.POSIXct(x_chr, tz = "UTC", tryFormats = c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%m/%d/%Y %H:%M:%S"
  )))
}

sanitize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("(^_+|_+$)", "", x)
}

parse_timestamp_from_filename <- function(path) {
  file <- basename(path)

  soft_match <- stringr::str_match(file, "(\\d{4}-\\d{2}-\\d{2})\\s+(\\d{4})")
  if (!all(is.na(soft_match[1, ]))) {
    stamp <- paste(soft_match[1, 2], soft_match[1, 3])
    parsed <- suppressWarnings(as.POSIXct(stamp, format = "%Y-%m-%d %H%M", tz = "UTC"))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  colony_match <- stringr::str_match(file, "(\\d{8})")
  if (!all(is.na(colony_match[1, ]))) {
    parsed <- suppressWarnings(as.POSIXct(colony_match[1, 2], format = "%Y%m%d", tz = "UTC"))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  as.POSIXct(file.info(path)$mtime, tz = "UTC")
}

find_latest_data_file <- function(data_dir, pattern) {
  candidates <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(candidates) == 0) {
    return(NA_character_)
  }

  order_idx <- order(vapply(candidates, parse_timestamp_from_filename, as.POSIXct(Sys.time(), tz = "UTC")), decreasing = TRUE)
  candidates[[order_idx[[1]]]]
}

find_latest_softmouse_export <- function(data_dir = "data") {
  find_latest_data_file(data_dir, "^SoftMouse\\.NET-Mouse List-.*\\.xlsx$")
}

find_latest_colony_snapshot <- function(data_dir = "data") {
  find_latest_data_file(data_dir, "^CYB5R4_Colony_\\d{8}.*\\.xlsx$")
}

find_latest_census_file <- function(data_dir = "data") {
  find_latest_data_file(data_dir, "(?i)(colony_census|cencus).*\\.(csv|xlsx)$")
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

safe_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
}

parse_seed_ids <- function(text) {
  text |>
    paste(collapse = "\n") |>
    stringr::str_split("[,\n;\\s]+", simplify = FALSE) |>
    purrr::pluck(1) |>
    normalize_mouse_id() |>
    stats::na.omit() |>
    unique() |>
    as.character()
}

build_mouse_label <- function(df, label_fields = c("mouse_id", "sex", "age_label", "raw_genotype")) {
  if (nrow(df) == 0) {
    return(character(0))
  }

  keep_fields <- intersect(label_fields, names(df))
  if (length(keep_fields) == 0) {
    return(df$mouse_id %||% rep("", nrow(df)))
  }

  pieces <- lapply(keep_fields, function(field) {
    values <- trim_na(df[[field]])
    if (field == "alive") {
      values <- ifelse(isTRUE(values), "Live", "Ended")
    }
    values
  })

  purrr::pmap_chr(pieces, function(...) {
    vals <- c(...)
    vals <- vals[!is.na(vals) & vals != ""]
    paste(vals, collapse = "\n")
  })
}

to_flag_string <- function(x) {
  x <- trim_na(x)
  ifelse(is.na(x), "", x)
}

format_status_message <- function(message) {
  paste0(format(Sys.time(), "%H:%M:%S"), "  ", message)
}
