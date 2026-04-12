mod_operations_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "app-hero",
      shiny::tags$h1("Operations"),
      shiny::tags$p("Curator-owned local state lives here: breeder status, experiment readiness, reservations, notes, cohort labels, and pairing plans.")
    ),
    instruction_card(
      "Editing behavior",
      "Select a mouse to edit its local operational state. Saved changes update the SQLite working database and remain separate from the upstream SoftMouse source data."
    ),
    shiny::tags$br(),
    shiny::selectizeInput(ns("mouse_id"), "Mouse ID", choices = NULL),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        bslib::card_header("Local Annotation"),
        shiny::checkboxInput(ns("is_breeder"), "Breeder", value = FALSE),
        shiny::checkboxInput(ns("experiment_ready"), "Experiment ready", value = FALSE),
        shiny::textInput(ns("cohort_label"), "Cohort label"),
        shiny::textInput(ns("local_flags"), "Flags"),
        shiny::textAreaInput(ns("notes"), "Notes", rows = 6),
        shiny::actionButton(ns("save_annotation"), "Save annotation", class = "btn-primary")
      ),
      bslib::card(
      bslib::card_header("Reservation"),
      shiny::textInput(ns("project_name"), "Project"),
      shiny::textInput(ns("reserved_by"), "Reserved by"),
      shiny::dateInput(ns("reserved_until"), "Reserved until"),
      shiny::selectInput(
        ns("reservation_status"),
        "Status",
        choices = stats::setNames(c("", "available", "reserved", "hold", "released", "used"), c("", "available", "reserved", "hold", "released", "used"))
      ),
        shiny::textAreaInput(ns("reservation_notes"), "Reservation notes", rows = 4),
        shiny::actionButton(ns("save_reservation"), "Save reservation")
      ),
      bslib::card(
        bslib::card_header("Pairing Plan"),
        shiny::selectizeInput(ns("sire_id"), "Sire", choices = NULL),
        shiny::selectizeInput(ns("dam_id"), "Dam", choices = NULL),
        shiny::dateInput(ns("planned_on"), "Planned on", value = Sys.Date()),
        shiny::selectInput(ns("pair_status"), "Status", choices = c("planned", "active", "completed", "cancelled")),
        shiny::textAreaInput(ns("pair_notes"), "Pairing notes", rows = 4),
        shiny::actionButton(ns("save_pairing"), "Save pairing")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Recent Activity"),
        DT::DTOutput(ns("change_log"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Active Reservations"),
        DT::DTOutput(ns("reservations"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Pairings"),
        DT::DTOutput(ns("pairings"))
      )
    )
  )
}

mod_operations_server <- function(id, data, metadata, db_path, selected_ids_rv, refresh_data, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      df <- data()
      selected_mouse <- NULL
      if (nrow(df) > 0) {
        selected_ids <- selected_ids_rv()
        selected_mouse <- input$mouse_id %||% if (length(selected_ids) > 0) selected_ids[[1]] else NULL
        selected_mouse <- selected_mouse %||% df$mouse_id[[1]]
      }

      shiny::updateSelectizeInput(
        session,
        "mouse_id",
        choices = df$mouse_id,
        selected = selected_mouse,
        server = TRUE
      )
      shiny::updateSelectizeInput(session, "sire_id", choices = df$mouse_id[df$sex == "M"], server = TRUE)
      shiny::updateSelectizeInput(session, "dam_id", choices = df$mouse_id[df$sex == "F"], server = TRUE)
    })

    shiny::observeEvent(selected_ids_rv(), {
      selected_ids <- selected_ids_rv()
      selected <- if (length(selected_ids) > 0) selected_ids[[1]] else NULL
      if (!is.null(selected) && !is.na(selected)) {
        shiny::updateSelectizeInput(session, "mouse_id", selected = selected)
      }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$mouse_id, {
      df <- data()
      row <- df[df$mouse_id == input$mouse_id, , drop = FALSE]
      if (nrow(row) == 0) {
        return()
      }

      shiny::updateCheckboxInput(session, "is_breeder", value = isTRUE(row$is_breeder[[1]]))
      shiny::updateCheckboxInput(session, "experiment_ready", value = isTRUE(row$experiment_ready[[1]]))
      shiny::updateTextInput(session, "cohort_label", value = row$cohort_label[[1]] %||% "")
      shiny::updateTextInput(session, "local_flags", value = row$local_flags[[1]] %||% "")
      shiny::updateTextAreaInput(session, "notes", value = row$notes[[1]] %||% "")
      shiny::updateTextInput(session, "project_name", value = row$reservation_project[[1]] %||% "")
      shiny::updateTextInput(session, "reserved_by", value = row$reservation_owner[[1]] %||% "")
      shiny::updateDateInput(session, "reserved_until", value = row$reservation_until[[1]] %||% Sys.Date())
      shiny::updateSelectInput(session, "reservation_status", selected = row$reservation_status[[1]] %||% "")
      shiny::updateTextAreaInput(session, "reservation_notes", value = row$reservation_notes[[1]] %||% "")
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$save_annotation, {
      shiny::req(nzchar(input$mouse_id %||% ""))
      set_status(paste("Saving local annotation for", input$mouse_id), "running")
      save_annotation(
        db_path = db_path,
        mouse_id = input$mouse_id,
        is_breeder = input$is_breeder,
        experiment_ready = input$experiment_ready,
        cohort_label = input$cohort_label,
        local_flags = input$local_flags,
        notes = input$notes
      )
      refresh_data()
      set_status(paste("Annotation saved for", input$mouse_id), "success")
    })

    shiny::observeEvent(input$save_reservation, {
      shiny::req(nzchar(input$mouse_id %||% ""))
      set_status(paste("Saving reservation for", input$mouse_id), "running")
      save_reservation(
        db_path = db_path,
        mouse_id = input$mouse_id,
        project_name = input$project_name,
        reserved_by = input$reserved_by,
        reserved_until = input$reserved_until,
        status = input$reservation_status,
        notes = input$reservation_notes
      )
      refresh_data()
      set_status(paste("Reservation saved for", input$mouse_id), "success")
    })

    shiny::observeEvent(input$save_pairing, {
      shiny::req(nzchar(input$sire_id %||% ""), nzchar(input$dam_id %||% ""))
      set_status("Saving pairing plan.", "running")
      save_pairing(
        db_path = db_path,
        sire_id = input$sire_id,
        dam_id = input$dam_id,
        planned_on = input$planned_on,
        status = input$pair_status,
        notes = input$pair_notes
      )
      refresh_data()
      set_status("Pairing plan saved.", "success")
    })

    output$change_log <- DT::renderDT({
      DT::datatable(
        metadata()$changes |>
          dplyr::mutate(changed_at = format(changed_at, "%Y-%m-%d %H:%M")) |>
          dplyr::select(changed_at, entity_type, entity_id, action),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$reservations <- DT::renderDT({
      DT::datatable(
        metadata()$reservations,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$pairings <- DT::renderDT({
      DT::datatable(
        metadata()$pairings,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
  })
}
