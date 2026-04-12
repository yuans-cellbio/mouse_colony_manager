required_packages <- c(
  "shiny", "bslib", "DT", "DBI", "RSQLite", "dplyr", "ggplot2",
  "jsonlite", "openxlsx", "plotly", "purrr", "readr", "stringr",
  "tibble", "tidyr", "testthat", "ggped"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

if (length(missing_packages) > 0) {
  renv::install(missing_packages)
}

renv::snapshot(prompt = FALSE)

message("Project dependencies are ready. Launch the app with shiny::runApp('.') or source('run_app.R').")
