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
      "Upload a current SoftMouse export each time you refresh the colony. Add the latest local colony snapshot whenever you want to preserve mice or field values that are not present in the SoftMouse export. The app now preserves non-conflicting snapshot values by default, but conflicting values must be resolved explicitly before import can finish."
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
          shiny::helpText("Optional but recommended when you want to preserve colony records or field values that are absent from the current SoftMouse export."),
          shiny::fileInput(
            ns("census_file"),
            "Breeder census (.csv or .xlsx)",
            accept = c(".csv", ".xlsx")
          ),
          shiny::helpText("Optional. Use this to seed breeder flags for a new database or refresh them from a census file."),
          shiny::tags$br(),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton(ns("preview_import"), "Preview merge", class = "btn-primary")),
            shiny::column(6, shiny::actionButton(ns("complete_import"), "Complete import"))
          )
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
            "These rows appear in both uploaded sources and disagree on a specific field. Every conflict must be resolved before the import can be committed."
          ),
          DT::DTOutput(ns("conflicts"))
        )
      ),
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Conflict Overrides"),
        bslib::card_body(
          shiny::uiOutput(ns("conflict_resolution_ui"))
        )
      )
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Duplicate IDs In Uploaded Files"),
        bslib::card_body(
          DT::DTOutput(ns("duplicates"))
        )
      ),
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Missing Parents In Merged Database"),
        bslib::card_body(
          shiny::tags$p(
            class = "filter-summary",
            "These sire or dam IDs are referenced by mice in the merged result but do not exist as primary mouse entries in the merged database."
          ),
          DT::DTOutput(ns("missing_parents"))
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
    import_state <- shiny::reactiveVal(NULL)

    current_result <- shiny::reactive(import_state())

    collect_conflict_resolutions <- function(conflicts, saved_resolutions = NULL) {
      if (nrow(conflicts) == 0) {
        return(normalize_conflict_resolutions(NULL))
      }

      saved_resolutions <- normalize_conflict_resolutions(saved_resolutions)

      resolutions <- purrr::map_dfr(seq_len(nrow(conflicts)), function(i) {
        saved_choice <- saved_resolutions$chosen_source[
          saved_resolutions$mouse_id == conflicts$mouse_id[[i]] &
            saved_resolutions$field == conflicts$field[[i]]
        ]

        tibble::tibble(
          mouse_id = conflicts$mouse_id[[i]],
          field = conflicts$field[[i]],
          chosen_source = input[[paste0("conflict_resolution_", i)]] %||%
            if (length(saved_choice) == 0) "softmouse" else saved_choice[[1]]
        )
      })

      normalize_conflict_resolutions(resolutions)
    }

    output$summary_metrics <- shiny::renderUI({
      result <- current_result()
      metrics <- result$metrics %||% tibble::tibble(metric = character(0), value = numeric(0))
      if (nrow(metrics) == 0) {
        return(instruction_card("No import preview yet", "Choose source files and click Preview merge to inspect conflicts, duplicates, and missing parents before completing the import."))
      }

      card_values <- stats::setNames(metrics$value, metrics$metric)
      bslib::layout_column_wrap(
        width = 1 / 3,
        metric_card("Merged working rows", card_values[["Merged working rows"]] %||% 0),
        metric_card("New mice", card_values[["New mice versus current DB"]] %||% 0),
        metric_card("Conflict fields", card_values[["Source conflicts"]] %||% 0)
      )
    })

    output$summary_table <- DT::renderDT({
      result <- current_result()
      DT::datatable(
        result$metrics %||% tibble::tibble(metric = character(0), value = numeric(0)),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE)
      )
    })

    output$conflicts <- DT::renderDT({
      result <- current_result()
      DT::datatable(
        result$conflicts %||% tibble::tibble(
          mouse_id = character(0),
          field = character(0),
          softmouse_value = character(0),
          snapshot_value = character(0)
        ),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 280)
      )
    })

    output$conflict_resolution_ui <- shiny::renderUI({
      result <- current_result()
      conflicts <- result$conflicts %||% tibble::tibble()
      saved_resolutions <- normalize_conflict_resolutions(result$conflict_resolutions %||% NULL)

      if (nrow(conflicts) == 0) {
        return(instruction_card("No conflicts detected", "The uploaded sources do not disagree on any overlapping mouse fields, so the import can be completed without additional decisions."))
      }

      grouped_conflicts <- split(seq_len(nrow(conflicts)), conflicts$mouse_id)

      shiny::tagList(
        instruction_card(
          "Review exceptions",
          paste(
            "Each conflict defaults to SoftMouse export.",
            "Open only the mice you want to override with the local snapshot."
          )
        ),
        shiny::tags$br(),
        bslib::accordion(
          id = session$ns("conflict_resolution_accordion"),
          multiple = TRUE,
          open = FALSE,
          !!!purrr::imap(grouped_conflicts, function(row_ids, mouse_id) {
            bslib::accordion_panel(
              title = paste(mouse_id, "(", length(row_ids), "conflict", if (length(row_ids) == 1) "" else "s", ")", sep = ""),
              purrr::map(row_ids, function(i) {
                conflict <- conflicts[i, ]
                existing <- saved_resolutions$chosen_source[
                  saved_resolutions$mouse_id == conflict$mouse_id[[1]] &
                    saved_resolutions$field == conflict$field[[1]]
                ]
                selected <- if (length(existing) == 0) "softmouse" else existing[[1]]

                shiny::tags$div(
                  style = "padding: 0.75rem 0; border-bottom: 1px solid rgba(24,33,31,0.08);",
                  shiny::tags$div(
                    style = "font-weight: 600; margin-bottom: 0.35rem;",
                    conflict$field[[1]]
                  ),
                  shiny::tags$div(class = "filter-summary", paste("SoftMouse:", conflict$softmouse_value[[1]] %||% "<missing>")),
                  shiny::tags$div(class = "filter-summary", paste("Local snapshot:", conflict$snapshot_value[[1]] %||% "<missing>")),
                  shiny::selectInput(
                    session$ns(paste0("conflict_resolution_", i)),
                    "Keep value from",
                    choices = c(
                      "SoftMouse export" = "softmouse",
                      "Local snapshot" = "local_snapshot"
                    ),
                    selected = selected
                  )
                )
              })
            )
          })
        )
      )
    })

    output$duplicates <- DT::renderDT({
      result <- current_result()
      DT::datatable(
        result$duplicates %||% tibble::tibble(source = character(0), mouse_id = character(0), duplicate_count = integer(0)),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 280)
      )
    })

    output$missing_parents <- DT::renderDT({
      result <- current_result()
      DT::datatable(
        result$missing_parents %||% tibble::tibble(
          mouse_id = character(0),
          missing_parent_role = character(0),
          missing_parent_id = character(0)
        ),
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

    shiny::observeEvent(input$preview_import, {
      shiny::req(input$softmouse_file)

      set_status("Preparing import preview.", "running")

      shiny::withProgress(message = "Previewing import", value = 0, {
        tryCatch({
          result <- preview_manual_import(
            db_path = db_path,
            softmouse_path = input$softmouse_file$datapath[[1]],
            snapshot_path = input$snapshot_file$datapath[[1]] %||% NA_character_
          )

          import_state(result)
          shiny::incProgress(1, detail = "Preview ready")

          if (nrow(result$unresolved_conflicts) > 0) {
            set_status(
              paste(
                "Import preview ready.",
                nrow(result$unresolved_conflicts),
                "conflicts must be resolved before you can complete the import."
              ),
              "info"
            )
          } else if (nrow(result$conflicts) > 0) {
            set_status(
              paste(
                "Import preview ready.",
                nrow(result$conflicts),
                "conflict fields default to SoftMouse export; review only the exceptions you want to override."
              ),
              "info"
            )
          } else {
            set_status(
              paste(
                "Import preview ready.",
                nrow(result$current_df),
                "mice will be available after you complete the import."
              ),
              "success"
            )
          }
        }, error = function(e) {
          set_status(paste("Import preview failed:", conditionMessage(e)), "error")
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      })
    })

    shiny::observeEvent(input$complete_import, {
      shiny::req(input$softmouse_file)

      preview <- current_result()
      if (is.null(preview)) {
        set_status("Preview the merge before completing the import.", "error")
        return()
      }

      resolutions <- collect_conflict_resolutions(
        preview$conflicts %||% tibble::tibble(),
        saved_resolutions = preview$conflict_resolutions %||% NULL
      )
      unresolved <- find_unresolved_import_conflicts(preview$conflicts %||% tibble::tibble(), resolutions)
      if (nrow(unresolved) > 0) {
        import_state(utils::modifyList(preview, list(
          unresolved_conflicts = unresolved,
          conflict_resolutions = resolutions
        )))
        set_status(
          paste(
            "Import blocked.",
            nrow(unresolved),
            "conflicts still need a resolution."
          ),
          "error"
        )
        return()
      }

      set_status("Completing import and writing the database.", "running")

      shiny::withProgress(message = "Completing import", value = 0, {
        tryCatch({
          softmouse_path <- archive_uploaded_file(input$softmouse_file, "softmouse")
          shiny::incProgress(0.25, detail = "SoftMouse export archived")

          snapshot_path <- archive_uploaded_file(input$snapshot_file, "snapshot")
          shiny::incProgress(0.25, detail = "Snapshot archived")

          census_path <- archive_uploaded_file(input$census_file, "census")
          shiny::incProgress(0.10, detail = "Optional census archived")

          result <- run_manual_import(
            db_path = db_path,
            softmouse_path = softmouse_path,
            snapshot_path = snapshot_path,
            census_path = census_path,
            conflict_resolutions = resolutions
          )

          import_state(utils::modifyList(result, list(stage = "complete")))
          shiny::incProgress(0.40, detail = "Database updated")
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
