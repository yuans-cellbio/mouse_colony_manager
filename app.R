required_packages <- c(
  "shiny", "bslib", "DT", "DBI", "RSQLite", "dplyr", "ggplot2",
  "jsonlite", "openxlsx", "plotly", "purrr", "readr", "stringr",
  "tibble", "tidyr", "cli", "gtable", "kinship2"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    "\nRun source('setup_project.R') to install the app dependencies.",
    call. = FALSE
  )
}

app_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
invisible(lapply(app_files, source, chdir = TRUE))

local_sass_cache <- normalizePath(file.path("data", ".sass-cache"), winslash = "/", mustWork = FALSE)
if (!dir.exists(local_sass_cache)) {
  dir.create(local_sass_cache, recursive = TRUE, showWarnings = FALSE)
}
options(sass.cache = local_sass_cache, bslib.cache = FALSE)

create_mouse_colony_app()
