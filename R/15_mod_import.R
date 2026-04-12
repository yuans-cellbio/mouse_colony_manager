mod_import_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "app-hero",
      shiny::tags$h1("Import"),
      shiny::tags$p("Select the source files you want to load into the working database. Imports are manual and will preserve app-owned local annotations such as notes, reservations, and pairing plans.")
    ),
    instruction_card(
      "Recommended workflow",
      "Upload a current SoftMouse export each time you refresh the colony. Add the latest local colony snapshot if you want to preserve historical mice that may no longer appear in the upstream export. The breeder census file is optional."
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Select Files"),
        bslib::card_body(
          shiny::fileInput(
            ns("softmouse_file"),
            "SoftMouse export (.xlsx)",
            accept = c(".xlsx")
          ),
          shiny::helpText("Required. This should be the current export downloaded from SoftMouse.NET."),
          shiny::fileInput(
            ns("snapshot_file"),
            "Local colony snapshot (.xlsx)",
            accept = c(".xlsx")
          ),
          shiny::helpText("Optional. Use the latest local colony workbook if you want to preserve historical-only mice during consolidation."),
          shiny::fileInput(
            ns("census_file"),
            "Breeder census (.csv or .xlsx)",
            accept = c(".csv", ".xlsx")
          ),
          shiny::helpText("Optional. Use this to seed breeder flags for a new database or refresh them from a census file."),
          shiny::tags$br(),
          shiny::actionButton(ns("run_import"), "Import selected files", class = "btn-primary")
        )
      ),
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Import Summary"),
        bslib::card_body(
          shiny::uiOutput(ns("summary_metrics")),
          shiny::tags$br(),
          DT::DTOutput(ns("summary_table"))
        )
      )
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Conflicts Across Uploaded Sources"),
        bslib::card_body(
          shiny::tags$p(
            class = "filter-summary",
            "These rows appear in both uploaded sources but disagree on one or more key fields."
          ),
          DT::DTOutput(ns("conflicts"))
        )
      ),
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Duplicate IDs In Uploaded Files"),
        bslib::card_body(
          DT::DTOutput(ns("duplicates"))
        )
      )
    ),
    shiny::tags$br(),
    bslib::card(
      class = "dashboard-table-card",
      full_screen = TRUE,
      bslib::card_header("Import History"),
      bslib::card_body(
        DT::DTOutput(ns("imports"))
      )
    )
  )
}

mod_import_server <- function(id, db_path, metadata, refresh_data, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    import_result <- shiny::reactiveVal(NULL)

    output$summary_metrics <- shiny::renderUI({
      result <- import_result()
      metrics <- result$metrics %||% tibble::tibble(metric = character(0), value = numeric(0))
      if (nrow(metrics) == 0) {
        return(instruction_card("No import run yet", "Choose source files and click Import selected files to load or update the working database."))
      }

      card_values <- stats::setNames(metrics$value, metrics$metric)
      bslib::layout_column_wrap(
        width = 1 / 3,
        metric_card("Merged working rows", card_values[["Merged working rows"]] %||% 0),
        metric_card("New mice", card_values[["New mice versus current DB"]] %||% 0),
        metric_card("Source conflicts", card_values[["Source conflicts"]] %||% 0)
      )
    })

    output$summary_table <- DT::renderDT({
      result <- import_result()
      DT::datatable(
        result$metrics %||% tibble::tibble(metric = character(0), value = numeric(0)),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE)
      )
    })

    output$conflicts <- DT::renderDT({
      result <- import_result()
      DT::datatable(
        result$conflicts %||% tibble::tibble(mouse_id = character(0), differing_fields = character(0)),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 280)
      )
    })

    output$duplicates <- DT::renderDT({
      result <- import_result()
      DT::datatable(
        result$duplicates %||% tibble::tibble(source = character(0), mouse_id = character(0), duplicate_count = integer(0)),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 280)
      )
    })

    output$imports <- DT::renderDT({
      import_df <- metadata()$imports |>
        dplyr::mutate(imported_at = format(imported_at, "%Y-%m-%d %H:%M"))

      DT::datatable(
        import_df,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 280)
      )
    })

    shiny::observeEvent(input$run_import, {
      shiny::req(input$softmouse_file)

      set_status("Starting manual import.", "running")

      shiny::withProgress(message = "Importing files", value = 0, {
        tryCatch({
          softmouse_path <- archive_uploaded_file(input$softmouse_file, "softmouse")
          shiny::incProgress(0.25, detail = "SoftMouse export archived")

          snapshot_path <- archive_uploaded_file(input$snapshot_file, "snapshot")
          shiny::incProgress(0.25, detail = "Snapshot archived")

          census_path <- archive_uploaded_file(input$census_file, "census")
          shiny::incProgress(0.15, detail = "Optional census archived")

          result <- run_manual_import(
            db_path = db_path,
            softmouse_path = softmouse_path,
            snapshot_path = snapshot_path,
            census_path = census_path
          )

          import_result(result)
          shiny::incProgress(0.35, detail = "Database updated")
          refresh_data()
          set_status(
            paste(
              "Import complete.",
              nrow(result$current_df),
              "mice are now available in the working database."
            ),
            "success"
          )
        }, error = function(e) {
          set_status(paste("Import failed:", conditionMessage(e)), "error")
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      })
    })
  })
}
