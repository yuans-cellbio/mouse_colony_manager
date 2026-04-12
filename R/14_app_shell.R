colony_app_ui <- function() {
  shiny::tagList(
    bslib::page_navbar(
      title = "Mouse Colony Manager",
      theme = mouse_colony_theme(),
      window_title = "Mouse Colony Manager",
      fillable = FALSE,
      bslib::nav_panel("Import", mod_import_ui("import")),
      bslib::nav_panel("Dashboard", mod_dashboard_ui("dashboard")),
      bslib::nav_panel("Colony Browser", mod_browser_ui("browser")),
      bslib::nav_panel("Lineage Trace", mod_lineage_ui("lineage")),
      bslib::nav_panel("Pedigree", mod_pedigree_ui("pedigree")),
      bslib::nav_panel("Operations", mod_operations_ui("operations"))
    ),
    status_bar_ui()
  )
}

colony_app_server <- function(input, output, session) {
  db_path <- file.path("data", "mouse_colony_manager.sqlite")
  ensure_colony_store(db_path)

  refresh_nonce <- shiny::reactiveVal(Sys.time())
  selected_ids_rv <- shiny::reactiveVal(character(0))
  lineage_result_rv <- shiny::reactiveVal(empty_lineage_result())
  status_state <- shiny::reactiveValues(
    text = format_status_message("Ready. Import files from the Import page to refresh the colony."),
    kind = "info"
  )

  set_status <- function(message, kind = "info") {
    status_state$text <- format_status_message(message)
    status_state$kind <- kind
  }

  refresh_data <- function() {
    refresh_nonce(Sys.time())
  }

  colony_data <- shiny::reactive({
    refresh_nonce()
    load_current_colony(db_path)
  })

  colony_metadata <- shiny::reactive({
    refresh_nonce()
    get_colony_metadata(db_path)
  })

  output$app_status_bar <- shiny::renderUI({
    shiny::tags$div(
      class = paste("app-status-bar", status_state$kind),
      shiny::tags$span(class = "app-status-pill", status_state$kind),
      shiny::tags$span(class = "app-status-text", status_state$text)
    )
  })

  shiny::observe({
    df <- colony_data()
    if (length(selected_ids_rv()) == 0 && nrow(df) > 0) {
      selected_ids_rv(df$mouse_id[[1]])
    }
  })

  shiny::observe({
    df <- colony_data()
    meta <- colony_metadata()

    if (nrow(df) == 0 && nrow(meta$imports) == 0) {
      set_status(
        "Working database is empty. Import files from the Import page to start a new colony database.",
        "info"
      )
    }
  })

  mod_import_server("import", db_path = db_path, metadata = colony_metadata, refresh_data = refresh_data, set_status = set_status)
  mod_dashboard_server("dashboard", data = colony_data, metadata = colony_metadata)
  browser_state <- mod_browser_server("browser", data = colony_data, db_path = db_path, selected_ids_rv = selected_ids_rv, set_status = set_status)
  mod_lineage_server("lineage", data = colony_data, selected_ids_rv = selected_ids_rv, lineage_result_rv = lineage_result_rv, set_status = set_status)
  mod_pedigree_server("pedigree", data = colony_data, selected_ids_rv = selected_ids_rv, browser_filtered = browser_state$filtered_rows, lineage_result_r = shiny::reactive(lineage_result_rv()), set_status = set_status)
  mod_operations_server("operations", data = colony_data, metadata = colony_metadata, db_path = db_path, selected_ids_rv = selected_ids_rv, refresh_data = refresh_data, set_status = set_status)
}
