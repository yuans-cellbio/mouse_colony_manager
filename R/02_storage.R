connect_colony_db <- function(db_path = file.path("data", "mouse_colony_manager.sqlite")) {
  ensure_dir(dirname(db_path))
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = normalizePath(db_path, winslash = "/", mustWork = FALSE),
    synchronous = NULL
  )
}

empty_annotations_df <- function() {
  tibble::tibble(
    mouse_id = character(),
    is_breeder = logical(),
    experiment_ready = logical(),
    cohort_label = character(),
    local_flags = character(),
    notes = character(),
    updated_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_reservations_df <- function() {
  tibble::tibble(
    mouse_id = character(),
    project_name = character(),
    reserved_by = character(),
    reserved_until = as.Date(character()),
    status = character(),
    notes = character(),
    updated_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_pairings_df <- function() {
  tibble::tibble(
    pairing_key = character(),
    sire_id = character(),
    dam_id = character(),
    planned_on = as.Date(character()),
    status = character(),
    notes = character(),
    updated_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_filter_presets_df <- function() {
  tibble::tibble(
    preset_name = character(),
    preset_json = character(),
    updated_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_change_log_df <- function() {
  tibble::tibble(
    change_id = integer(),
    entity_type = character(),
    entity_id = character(),
    action = character(),
    payload_json = character(),
    changed_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_import_runs_df <- function() {
  tibble::tibble(
    import_id = integer(),
    source_path = character(),
    source_type = character(),
    source_file = character(),
    imported_at = as.POSIXct(character(), tz = "UTC"),
    row_count = integer()
  )
}

empty_colony_df <- function() {
  empty_mouse_records() |>
    dplyr::mutate(
      is_breeder = logical(),
      experiment_ready = logical(),
      cohort_label = character(),
      local_flags = character(),
      notes = character(),
      updated_at = as.POSIXct(character(), tz = "UTC"),
      reservation_project = character(),
      reservation_owner = character(),
      reservation_until = as.Date(character()),
      reservation_status = character(),
      reservation_notes = character(),
      reservation_updated_at = as.POSIXct(character(), tz = "UTC"),
      active_pairing_role = character(),
      active_pairing_with = character()
    )
}

initialize_colony_db <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS import_runs (
      import_id INTEGER PRIMARY KEY,
      source_path TEXT NOT NULL UNIQUE,
      source_type TEXT NOT NULL,
      source_file TEXT NOT NULL,
      imported_at TEXT NOT NULL,
      row_count INTEGER NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS imported_records (
      import_id INTEGER NOT NULL,
      mouse_id TEXT,
      record_json TEXT NOT NULL,
      FOREIGN KEY(import_id) REFERENCES import_runs(import_id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      mouse_id TEXT PRIMARY KEY,
      is_breeder INTEGER,
      experiment_ready INTEGER,
      cohort_label TEXT,
      local_flags TEXT,
      notes TEXT,
      updated_at TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reservations (
      mouse_id TEXT PRIMARY KEY,
      project_name TEXT,
      reserved_by TEXT,
      reserved_until TEXT,
      status TEXT,
      notes TEXT,
      updated_at TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS pairings (
      pairing_key TEXT PRIMARY KEY,
      sire_id TEXT,
      dam_id TEXT,
      planned_on TEXT,
      status TEXT,
      notes TEXT,
      updated_at TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS change_log (
      change_id INTEGER PRIMARY KEY,
      entity_type TEXT NOT NULL,
      entity_id TEXT NOT NULL,
      action TEXT NOT NULL,
      payload_json TEXT,
      changed_at TEXT NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS filter_presets (
      preset_name TEXT PRIMARY KEY,
      preset_json TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")
}

restore_mouse_types <- function(df) {
  df <- tibble::as_tibble(df)

  character_cols <- intersect(c(
    "source_type", "source_path", "source_file", "mouse_id", "alt_id",
    "mouse_sid", "sex", "end_type", "age_source", "age_label", "status",
    "raw_genotype", "mouse_line", "generation", "first_gene",
    "second_gene", "protocol", "sire_id", "dam_id", "mate_id",
    "source_comment"
  ), names(df))
  for (col in character_cols) {
    df[[col]] <- trim_na(as.character(df[[col]]))
  }

  date_cols <- intersect(c("dob", "end_date"), names(df))
  for (col in date_cols) {
    df[[col]] <- coerce_date_column(df[[col]])
  }

  if ("dob" %in% names(df)) {
    df$dob[!is_plausible_mouse_date(df$dob)] <- as.Date(NA)
  }

  if ("end_date" %in% names(df)) {
    df$end_date[!is.na(df$end_date) & !is_plausible_mouse_date(df$end_date)] <- as.Date(NA)
  }

  if ("imported_at" %in% names(df)) {
    df$imported_at <- coerce_datetime_column(df$imported_at)
  }

  logical_cols <- intersect(c("alive", "founder"), names(df))
  for (col in logical_cols) {
    df[[col]] <- as.logical(as.integer(df[[col]]))
  }

  numeric_cols <- intersect(c("age_days", "age_weeks"), names(df))
  for (col in numeric_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  if (all(c("dob", "end_date", "age_source") %in% names(df))) {
    prefer_age_source <- rep(FALSE, nrow(df))
    if ("alive" %in% names(df)) {
      prefer_age_source <- !(df$alive %in% TRUE) & is.na(df$end_date)
      prefer_age_source <- dplyr::coalesce(prefer_age_source, FALSE)
    }

    age_columns <- compute_age_columns(
      df$dob,
      df$end_date,
      age_source = df$age_source,
      prefer_age_source = prefer_age_source
    )
    df$age_days <- age_columns$age_days
    df$age_weeks <- age_columns$age_weeks
    df$age_label <- age_columns$age_label
  }

  df
}

source_import_exists <- function(con, source_path) {
  query <- DBI::dbGetQuery(
    con,
    "SELECT import_id FROM import_runs WHERE source_path = ?",
    params = list(normalizePath(source_path, winslash = "/", mustWork = FALSE))
  )

  if (nrow(query) == 0) {
    return(NA_integer_)
  }

  as.integer(query$import_id[[1]])
}

insert_or_replace_import <- function(con, df, force = FALSE) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  source_path <- normalizePath(unique(df$source_path)[[1]], winslash = "/", mustWork = FALSE)
  import_id <- source_import_exists(con, source_path)

  if (!is.na(import_id) && !force) {
    return(invisible(import_id))
  }

  imported_at <- format(max(df$imported_at, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  source_type <- unique(df$source_type)[[1]]
  source_file <- unique(df$source_file)[[1]]

  DBI::dbExecute(
    con,
    "INSERT INTO import_runs (source_path, source_type, source_file, imported_at, row_count)
     VALUES (?, ?, ?, ?, ?)
     ON CONFLICT(source_path) DO UPDATE SET
       source_type = excluded.source_type,
       source_file = excluded.source_file,
       imported_at = excluded.imported_at,
       row_count = excluded.row_count",
    params = list(source_path, source_type, source_file, imported_at, nrow(df))
  )

  import_id <- source_import_exists(con, source_path)
  DBI::dbExecute(con, "DELETE FROM imported_records WHERE import_id = ?", params = list(import_id))

  payload <- purrr::map2(
    df$mouse_id,
    split(df, seq_len(nrow(df))),
    ~ list(import_id = import_id, mouse_id = .x, record_json = as.character(safe_json(.y)))
  ) |>
    dplyr::bind_rows()

  DBI::dbWriteTable(con, "imported_records", payload, append = TRUE)
  invisible(import_id)
}

overwrite_current_mouse <- function(con, current_df) {
  current_df <- current_df |>
    dplyr::mutate(
      imported_at = format(imported_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      dob = ifelse(is.na(dob), NA_character_, format(as.Date(dob), "%Y-%m-%d")),
      end_date = ifelse(is.na(end_date), NA_character_, format(as.Date(end_date), "%Y-%m-%d")),
      alive = as.integer(alive),
      founder = as.integer(founder)
    )

  DBI::dbWriteTable(con, "current_mouse", current_df, overwrite = TRUE)
}

log_change <- function(con, entity_type, entity_id, action, payload = NULL) {
  DBI::dbExecute(
    con,
    "INSERT INTO change_log (entity_type, entity_id, action, payload_json, changed_at)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      entity_type,
      entity_id,
      action,
      if (is.null(payload)) NA_character_ else safe_json(payload),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  )
}

write_snapshot <- function(con, softmouse_df, snapshot_df, current_df, force = FALSE) {
  insert_or_replace_import(con, softmouse_df, force = force)
  insert_or_replace_import(con, snapshot_df, force = force)
  overwrite_current_mouse(con, current_df)
}

load_annotations_from_con <- function(con) {
  if (!DBI::dbExistsTable(con, "annotations")) {
    return(empty_annotations_df())
  }

  DBI::dbReadTable(con, "annotations") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      is_breeder = as.logical(as.integer(is_breeder)),
      experiment_ready = as.logical(as.integer(experiment_ready)),
      updated_at = coerce_datetime_column(updated_at)
    )
}

seed_annotations_from_census <- function(con, census_path) {
  if (is.na(census_path) || !file.exists(census_path)) {
    return(invisible(NULL))
  }

  if (grepl("\\.csv$", census_path, ignore.case = TRUE)) {
    census_df <- read.csv(census_path, stringsAsFactors = FALSE)
  } else {
    census_df <- openxlsx::read.xlsx(census_path)
  }

  census_df <- tibble::as_tibble(census_df)
  names(census_df) <- sanitize_name(names(census_df))

  if (!all(c("id", "is_breeder") %in% names(census_df))) {
    return(invisible(NULL))
  }

  existing <- load_annotations_from_con(con)
  missing_rows <- census_df |>
    dplyr::transmute(
      mouse_id = normalize_mouse_id(id),
      is_breeder = as.logical(is_breeder),
      experiment_ready = NA,
      cohort_label = NA_character_,
      local_flags = NA_character_,
      notes = NA_character_,
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) |>
    dplyr::filter(!mouse_id %in% existing$mouse_id)

  if (nrow(missing_rows) > 0) {
    missing_rows$is_breeder <- as.integer(missing_rows$is_breeder)
    DBI::dbWriteTable(con, "annotations", missing_rows, append = TRUE)
  }
}

load_reservations_from_con <- function(con) {
  if (!DBI::dbExistsTable(con, "reservations")) {
    return(empty_reservations_df())
  }

  DBI::dbReadTable(con, "reservations") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      reserved_until = coerce_date_column(reserved_until),
      updated_at = coerce_datetime_column(updated_at)
    )
}

load_pairings_from_con <- function(con) {
  if (!DBI::dbExistsTable(con, "pairings")) {
    return(empty_pairings_df())
  }

  DBI::dbReadTable(con, "pairings") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      planned_on = coerce_date_column(planned_on),
      updated_at = coerce_datetime_column(updated_at)
    )
}

load_filter_presets_from_con <- function(con) {
  if (!DBI::dbExistsTable(con, "filter_presets")) {
    return(empty_filter_presets_df())
  }

  DBI::dbReadTable(con, "filter_presets") |>
    tibble::as_tibble() |>
    dplyr::mutate(updated_at = coerce_datetime_column(updated_at))
}

load_change_log_from_con <- function(con, limit = 50) {
  if (!DBI::dbExistsTable(con, "change_log")) {
    return(empty_change_log_df())
  }

  DBI::dbGetQuery(
    con,
    "SELECT * FROM change_log ORDER BY changed_at DESC LIMIT ?",
    params = list(as.integer(limit))
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(changed_at = coerce_datetime_column(changed_at))
}

load_current_mouse_from_con <- function(con) {
  if (!DBI::dbExistsTable(con, "current_mouse")) {
    return(empty_mouse_records())
  }

  current_df <- DBI::dbReadTable(con, "current_mouse") |>
    restore_mouse_types()

  current_n <- nrow(current_df)
  dplyr::bind_rows(current_df, empty_mouse_records()) |>
    dplyr::slice_head(n = current_n)
}

load_current_colony <- function(db_path = file.path("data", "mouse_colony_manager.sqlite")) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  current <- load_current_mouse_from_con(con)
  if (nrow(current) == 0) {
    return(empty_colony_df())
  }

  annotations <- load_annotations_from_con(con)
  reservations <- load_reservations_from_con(con)
  pairings <- load_pairings_from_con(con)

  active_pairings <- pairings |>
    dplyr::filter(status %in% c("planned", "active"))

  current |>
    dplyr::left_join(annotations, by = "mouse_id") |>
    dplyr::left_join(
      reservations |>
        dplyr::rename(
          reservation_project = project_name,
          reservation_owner = reserved_by,
          reservation_until = reserved_until,
          reservation_status = status,
          reservation_notes = notes,
          reservation_updated_at = updated_at
        ),
      by = "mouse_id"
    ) |>
    dplyr::mutate(
      is_breeder = dplyr::coalesce(is_breeder, FALSE),
      experiment_ready = dplyr::coalesce(experiment_ready, FALSE),
      local_flags = to_flag_string(local_flags)
    ) |>
    dplyr::left_join(
      active_pairings |>
        dplyr::transmute(mouse_id = sire_id, active_pairing_role = "sire", active_pairing_with = dam_id) |>
        dplyr::bind_rows(
          active_pairings |>
            dplyr::transmute(mouse_id = dam_id, active_pairing_role = "dam", active_pairing_with = sire_id)
        ),
      by = "mouse_id"
    )
}

save_annotation <- function(db_path, mouse_id, is_breeder = FALSE, experiment_ready = FALSE,
                            cohort_label = NA_character_, local_flags = NA_character_,
                            notes = NA_character_) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  DBI::dbExecute(
    con,
    "INSERT INTO annotations (mouse_id, is_breeder, experiment_ready, cohort_label, local_flags, notes, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT(mouse_id) DO UPDATE SET
       is_breeder = excluded.is_breeder,
       experiment_ready = excluded.experiment_ready,
       cohort_label = excluded.cohort_label,
       local_flags = excluded.local_flags,
       notes = excluded.notes,
       updated_at = excluded.updated_at",
    params = list(
      mouse_id,
      as.integer(isTRUE(is_breeder)),
      as.integer(isTRUE(experiment_ready)),
      trim_na(cohort_label),
      trim_na(local_flags),
      trim_na(notes),
      updated_at
    )
  )

  log_change(con, "annotation", mouse_id, "upsert", list(
    is_breeder = isTRUE(is_breeder),
    experiment_ready = isTRUE(experiment_ready),
    cohort_label = cohort_label,
    local_flags = local_flags
  ))
}

save_reservation <- function(db_path, mouse_id, project_name = NA_character_,
                             reserved_by = NA_character_, reserved_until = NA,
                             status = NA_character_, notes = NA_character_) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  DBI::dbExecute(
    con,
    "INSERT INTO reservations (mouse_id, project_name, reserved_by, reserved_until, status, notes, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT(mouse_id) DO UPDATE SET
       project_name = excluded.project_name,
       reserved_by = excluded.reserved_by,
       reserved_until = excluded.reserved_until,
       status = excluded.status,
       notes = excluded.notes,
       updated_at = excluded.updated_at",
    params = list(
      mouse_id,
      trim_na(project_name),
      trim_na(reserved_by),
      if (is.na(reserved_until)) NA_character_ else format(as.Date(reserved_until), "%Y-%m-%d"),
      trim_na(status),
      trim_na(notes),
      updated_at
    )
  )

  log_change(con, "reservation", mouse_id, "upsert", list(
    project_name = project_name,
    reserved_by = reserved_by,
    reserved_until = reserved_until,
    status = status
  ))
}

save_pairing <- function(db_path, sire_id, dam_id, planned_on = Sys.Date(),
                         status = "planned", notes = NA_character_) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  pairing_key <- paste(normalize_mouse_id(sire_id), normalize_mouse_id(dam_id), sep = "::")
  updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  DBI::dbExecute(
    con,
    "INSERT INTO pairings (pairing_key, sire_id, dam_id, planned_on, status, notes, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT(pairing_key) DO UPDATE SET
       planned_on = excluded.planned_on,
       status = excluded.status,
       notes = excluded.notes,
       updated_at = excluded.updated_at",
    params = list(
      pairing_key,
      normalize_mouse_id(sire_id),
      normalize_mouse_id(dam_id),
      if (is.na(planned_on)) NA_character_ else format(as.Date(planned_on), "%Y-%m-%d"),
      trim_na(status),
      trim_na(notes),
      updated_at
    )
  )

  log_change(con, "pairing", pairing_key, "upsert", list(
    sire_id = sire_id,
    dam_id = dam_id,
    planned_on = planned_on,
    status = status
  ))
}

save_filter_preset <- function(db_path, preset_name, preset_definition) {
  preset_name <- trim_na(preset_name)
  if (is.na(preset_name)) {
    return(invisible(FALSE))
  }

  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  DBI::dbExecute(
    con,
    "INSERT INTO filter_presets (preset_name, preset_json, updated_at)
     VALUES (?, ?, ?)
     ON CONFLICT(preset_name) DO UPDATE SET
       preset_json = excluded.preset_json,
       updated_at = excluded.updated_at",
    params = list(
      preset_name,
      safe_json(preset_definition),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  )

  log_change(con, "filter_preset", preset_name, "save", preset_definition)
  invisible(TRUE)
}

load_filter_preset <- function(db_path, preset_name) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  presets <- load_filter_presets_from_con(con)
  if (!preset_name %in% presets$preset_name) {
    return(NULL)
  }

  jsonlite::fromJSON(presets$preset_json[presets$preset_name == preset_name][[1]], simplifyVector = FALSE)
}

get_colony_metadata <- function(db_path = file.path("data", "mouse_colony_manager.sqlite")) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  imports <- if (DBI::dbExistsTable(con, "import_runs")) {
    DBI::dbReadTable(con, "import_runs") |>
      tibble::as_tibble() |>
      dplyr::mutate(imported_at = coerce_datetime_column(imported_at)) |>
      dplyr::arrange(dplyr::desc(imported_at))
  } else {
    empty_import_runs_df()
  }

  list(
    imports = imports,
    annotations = load_annotations_from_con(con),
    reservations = load_reservations_from_con(con),
    pairings = load_pairings_from_con(con),
    presets = load_filter_presets_from_con(con),
    changes = load_change_log_from_con(con)
  )
}

ensure_colony_store <- function(db_path = file.path("data", "mouse_colony_manager.sqlite")) {
  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)
  invisible(db_path)
}

archive_uploaded_file <- function(upload_info, source_label,
                                  archive_dir = file.path("data", "import_archive")) {
  if (is.null(upload_info) || nrow(upload_info) == 0) {
    return(NA_character_)
  }

  ensure_dir(archive_dir)
  ext <- tools::file_ext(upload_info$name[[1]])
  stem <- tools::file_path_sans_ext(upload_info$name[[1]])
  dest_name <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    sanitize_name(source_label),
    "_",
    sanitize_name(stem),
    if (nzchar(ext)) paste0(".", ext) else ""
  )
  dest_path <- file.path(archive_dir, dest_name)
  file.copy(upload_info$datapath[[1]], dest_path, overwrite = TRUE)
  normalizePath(dest_path, winslash = "/", mustWork = FALSE)
}

detect_duplicate_ids <- function(df, source_label) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df |>
    dplyr::count(mouse_id, name = "duplicate_count") |>
    dplyr::filter(duplicate_count > 1) |>
    dplyr::mutate(source = source_label) |>
    dplyr::select(source, mouse_id, duplicate_count)
}

find_missing_parent_records <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      mouse_id = character(),
      missing_parent_role = character(),
      missing_parent_id = character()
    ))
  }

  mouse_ids <- unique(stats::na.omit(df$mouse_id))

  dplyr::bind_rows(
    df |>
      dplyr::transmute(
        mouse_id,
        missing_parent_role = "sire",
        missing_parent_id = sire_id
      ),
    df |>
      dplyr::transmute(
        mouse_id,
        missing_parent_role = "dam",
        missing_parent_id = dam_id
      )
  ) |>
    dplyr::filter(!is.na(missing_parent_id), !missing_parent_id %in% mouse_ids) |>
    dplyr::distinct()
}

build_import_summary <- function(previous_df, softmouse_df, snapshot_df, merged_df,
                                 conflict_resolutions = NULL) {
  conflicts <- detect_import_conflicts(softmouse_df, snapshot_df)
  unresolved_conflicts <- find_unresolved_import_conflicts(conflicts, conflict_resolutions)
  duplicates <- dplyr::bind_rows(
    detect_duplicate_ids(softmouse_df, "SoftMouse export"),
    detect_duplicate_ids(snapshot_df, "Local snapshot")
  )
  missing_parents <- find_missing_parent_records(merged_df)

  previous_ids <- if ("mouse_id" %in% names(previous_df)) previous_df$mouse_id else character(0)
  merged_ids <- if ("mouse_id" %in% names(merged_df)) merged_df$mouse_id else character(0)

  metrics <- tibble::tibble(
    metric = c(
      "SoftMouse rows",
      "Snapshot rows",
      "Merged working rows",
      "New mice versus current DB",
      "Rows preserved from previous DB",
      "Source conflicts",
      "Resolved conflicts",
      "Unresolved conflicts",
      "Duplicate IDs in uploaded files"
      ,
      "Missing parent links"
    ),
    value = c(
      nrow(softmouse_df),
      nrow(snapshot_df),
      nrow(merged_df),
      sum(!merged_ids %in% previous_ids),
      sum(previous_ids %in% merged_ids),
      nrow(conflicts),
      nrow(conflicts) - nrow(unresolved_conflicts),
      nrow(unresolved_conflicts),
      nrow(duplicates),
      nrow(missing_parents)
    )
  )

  list(
    metrics = metrics,
    conflicts = conflicts,
    unresolved_conflicts = unresolved_conflicts,
    duplicates = duplicates
    ,
    missing_parents = missing_parents
  )
}

log_import_conflict_resolutions <- function(con, resolutions, conflicts) {
  resolutions <- normalize_conflict_resolutions(resolutions)
  if (nrow(resolutions) == 0 || nrow(conflicts) == 0) {
    return(invisible(NULL))
  }

  resolved_rows <- conflicts |>
    dplyr::inner_join(resolutions, by = c("mouse_id", "field"))

  purrr::pwalk(resolved_rows, function(mouse_id, field, softmouse_value, snapshot_value, chosen_source) {
    log_change(
      con,
      entity_type = "import_conflict",
      entity_id = paste(mouse_id, field, sep = "::"),
      action = "resolved",
      payload = list(
        mouse_id = mouse_id,
        field = field,
        chosen_source = chosen_source,
        softmouse_value = softmouse_value,
        snapshot_value = snapshot_value
      )
    )
  })
}

preview_manual_import <- function(db_path, softmouse_path, snapshot_path = NA_character_) {
  ensure_colony_store(db_path)
  previous_df <- tryCatch(load_current_colony(db_path), error = function(...) empty_colony_df())

  softmouse_df <- import_softmouse(softmouse_path)
  snapshot_df <- import_colony_snapshot(snapshot_path)
  conflict_resolutions <- default_conflict_resolutions(detect_import_conflicts(softmouse_df, snapshot_df))
  merged_df <- merge_import_sources(
    softmouse_df,
    snapshot_df,
    conflict_resolutions = conflict_resolutions,
    require_resolved = FALSE
  )
  summary <- build_import_summary(
    previous_df,
    softmouse_df,
    snapshot_df,
    merged_df,
    conflict_resolutions = conflict_resolutions
  )

  list(
    current_df = merged_df,
    metrics = summary$metrics,
    conflicts = summary$conflicts,
    unresolved_conflicts = summary$unresolved_conflicts,
    duplicates = summary$duplicates,
    missing_parents = summary$missing_parents,
    conflict_resolutions = conflict_resolutions,
    softmouse_df = softmouse_df,
    snapshot_df = snapshot_df
  )
}

run_manual_import <- function(db_path, softmouse_path, snapshot_path = NA_character_,
                              census_path = NA_character_, conflict_resolutions = NULL) {
  ensure_colony_store(db_path)
  previous_df <- tryCatch(load_current_colony(db_path), error = function(...) empty_colony_df())

  softmouse_df <- import_softmouse(softmouse_path)
  snapshot_df <- import_colony_snapshot(snapshot_path)
  conflicts <- detect_import_conflicts(softmouse_df, snapshot_df)
  unresolved_conflicts <- find_unresolved_import_conflicts(conflicts, conflict_resolutions)

  if (nrow(unresolved_conflicts) > 0) {
    stop("Resolve all import conflicts before completing the merge.", call. = FALSE)
  }

  merged_df <- merge_import_sources(
    softmouse_df,
    snapshot_df,
    conflict_resolutions = conflict_resolutions,
    require_resolved = TRUE
  )
  summary <- build_import_summary(previous_df, softmouse_df, snapshot_df, merged_df, conflict_resolutions = conflict_resolutions)

  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)
  write_snapshot(con, softmouse_df, snapshot_df, merged_df, force = TRUE)

  if (!is.na(census_path) && file.exists(census_path)) {
    seed_annotations_from_census(con, census_path)
  }

  log_import_conflict_resolutions(con, conflict_resolutions, conflicts)
  log_change(con, "import", basename(softmouse_path), "manual_import", list(
    softmouse_rows = nrow(softmouse_df),
    snapshot_rows = nrow(snapshot_df),
    merged_rows = nrow(merged_df),
    conflicts = nrow(summary$conflicts),
    resolved_conflicts = nrow(summary$conflicts) - nrow(summary$unresolved_conflicts),
    unresolved_conflicts = nrow(summary$unresolved_conflicts),
    duplicates = nrow(summary$duplicates)
    ,
    missing_parent_links = nrow(summary$missing_parents)
  ))

  list(
    current_df = merged_df,
    metrics = summary$metrics,
    conflicts = summary$conflicts,
    unresolved_conflicts = summary$unresolved_conflicts,
    duplicates = summary$duplicates,
    missing_parents = summary$missing_parents,
    conflict_resolutions = normalize_conflict_resolutions(conflict_resolutions),
    softmouse_df = softmouse_df,
    snapshot_df = snapshot_df
  )
}

bootstrap_colony_data <- function(data_dir = "data",
                                  db_path = file.path(data_dir, "mouse_colony_manager.sqlite"),
                                  force = FALSE) {
  softmouse_path <- find_latest_softmouse_export(data_dir)
  snapshot_path <- find_latest_colony_snapshot(data_dir)
  census_path <- find_latest_census_file(data_dir)

  con <- connect_colony_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initialize_colony_db(con)

  current_exists <- if (isTRUE(force)) {
    FALSE
  } else {
    DBI::dbExistsTable(con, "current_mouse") && nrow(load_current_mouse_from_con(con)) > 0
  }
  needs_refresh <- force || !current_exists

  if (!is.na(softmouse_path) && is.na(source_import_exists(con, softmouse_path))) {
    needs_refresh <- TRUE
  }

  if (!is.na(snapshot_path) && is.na(source_import_exists(con, snapshot_path))) {
    needs_refresh <- TRUE
  }

  if (needs_refresh) {
    softmouse_df <- import_softmouse(softmouse_path)
    snapshot_df <- import_colony_snapshot(snapshot_path)
    current_df <- merge_import_sources(softmouse_df, snapshot_df)
    write_snapshot(con, softmouse_df, snapshot_df, current_df, force = force)
    seed_annotations_from_census(con, census_path)
  }

  list(
    db_path = db_path,
    softmouse_path = softmouse_path,
    snapshot_path = snapshot_path,
    census_path = census_path
  )
}
