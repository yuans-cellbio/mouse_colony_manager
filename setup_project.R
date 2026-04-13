required_packages <- c(
  "shiny", "bslib", "DT", "DBI", "RSQLite", "dplyr", "ggplot2",
  "jsonlite", "openxlsx", "plotly", "purrr", "readr", "stringr",
  "tibble", "tidyr", "cli", "gtable", "kinship2", "testthat", "shinytest2"
)

repos <- getOption("repos")
cran_repo <- ""
if (!is.null(repos) && "CRAN" %in% names(repos) && !is.null(repos[["CRAN"]])) {
  cran_repo <- repos[["CRAN"]]
}
if (!nzchar(cran_repo) || identical(cran_repo, "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

installed_ggped_matches_target <- function() {
  if (!requireNamespace("ggped", quietly = TRUE)) {
    return(FALSE)
  }

  desc <- utils::packageDescription("ggped")
  metadata <- paste(
    stats::na.omit(c(
      desc[["RemoteUsername"]],
      desc[["RemoteRepo"]],
      desc[["URL"]],
      desc[["BugReports"]]
    )),
    collapse = " "
  )

  grepl("yuans-cellbio", metadata, fixed = TRUE) &&
    grepl("ggped_multi_feature", metadata, fixed = TRUE)
}

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

if (length(missing_packages) > 0) {
  renv::install(missing_packages)
}

if (!installed_ggped_matches_target()) {
  remotes::install_github(
    "yuans-cellbio/ggped_multi_feature",
    build_vignettes = FALSE,
    upgrade = "never"
  )
}

renv::snapshot(prompt = FALSE)

message(
  paste(
    "Project dependencies are ready.",
    "Pedigree rendering uses the GitHub-hosted ggped fork at yuans-cellbio/ggped_multi_feature.",
    "Launch the app with shiny::runApp('.') or source('run_app.R')."
  )
)
