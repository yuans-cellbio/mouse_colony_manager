required_packages <- c(
  "shiny", "bslib", "DT", "DBI", "RSQLite", "dplyr", "ggplot2",
  "jsonlite", "openxlsx", "plotly", "purrr", "readr", "stringr",
  "tibble", "tidyr", "cli", "gtable", "kinship2", "ggped"
)

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
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    "\nRun source('setup_project.R') to install the app dependencies.",
    call. = FALSE
  )
}

if (!installed_ggped_matches_target()) {
  stop(
    paste(
      "The installed ggped package is not the expected GitHub fork.",
      "Run source('setup_project.R') to install yuans-cellbio/ggped_multi_feature."
    ),
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
