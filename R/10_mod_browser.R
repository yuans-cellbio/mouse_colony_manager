default_browser_filters <- function() {
  list(
    id_query = "",
    id_mode = "contains",
    genotype_query = "",
    genotype_gene = "",
    genotype_value = "",
    sex = character(0),
    dob_range = c(NA, NA),
    age_range = c(0, 150),
    mouse_line = character(0),
    generation = character(0),
    alive_filter = "all",
    breeder_filter = "all",
    ready_filter = "all",
    reservation_filter = "all",
    cohort_filter = "",
    flag_query = ""
  )
}

mod_browser_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("filters"),
      title = "Filter Mice",
      width = 360,
      open = "desktop",
      instruction_card(
        "Filtering behavior",
        "Adjust the controls below, then click Apply Filters. All active controls combine with AND logic, and blank or untouched fields are ignored."
      ),
      shiny::tags$br(),
      shiny::textInput(ns("id_query"), "Mouse ID"),
      shiny::radioButtons(ns("id_mode"), "ID match", choices = c("Contains" = "contains", "Exact" = "exact"), inline = TRUE),
      shiny::textInput(ns("genotype_query"), "Genotype text regex"),
      shiny::selectizeInput(ns("genotype_gene"), "Parsed gene", choices = NULL),
      shiny::selectizeInput(ns("genotype_value"), "Gene value", choices = NULL, options = list(create = TRUE)),
      shiny::checkboxGroupInput(ns("sex"), "Sex", choices = c("M", "F"), inline = TRUE),
      shiny::dateRangeInput(ns("dob_range"), "Date of birth"),
      shiny::sliderInput(ns("age_range"), "Age (weeks)", min = 0, max = 150, value = c(0, 150), step = 1),
      shiny::selectizeInput(ns("mouse_line"), "Mouse line", choices = NULL, multiple = TRUE),
      shiny::selectizeInput(ns("generation"), "Generation", choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("alive_filter"), "Live status", choices = c("All" = "all", "Live only" = "live", "Ended only" = "ended")),
      shiny::selectInput(ns("breeder_filter"), "Breeder", choices = c("All" = "all", "Yes" = "yes", "No" = "no")),
      shiny::selectInput(ns("ready_filter"), "Experiment ready", choices = c("All" = "all", "Yes" = "yes", "No" = "no")),
      shiny::selectInput(ns("reservation_filter"), "Reservation status", choices = c("All" = "all", "available", "reserved", "hold", "released", "used")),
      shiny::selectizeInput(ns("cohort_filter"), "Cohort label", choices = NULL),
      shiny::textInput(ns("flag_query"), "Flags contain"),
      shiny::tags$hr(),
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton(ns("apply_filters"), "Apply filters", class = "btn-primary")),
        shiny::column(6, shiny::actionButton(ns("clear_filters"), "Clear inputs"))
      ),
      shiny::tags$br(),
      shiny::textInput(ns("preset_name"), "Preset name"),
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton(ns("save_preset"), "Save preset", class = "btn-secondary")),
        shiny::column(6, shiny::actionButton(ns("apply_preset"), "Load preset"))
      ),
      shiny::tags$br(),
      shiny::selectInput(ns("load_preset"), "Saved presets", choices = NULL)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Colony Browser"),
      shiny::tags$p(class = "filter-summary", shiny::textOutput(ns("summary"), inline = TRUE)),
      shiny::downloadButton(ns("download_csv"), "Export filtered CSV"),
      shiny::tags$br(),
      shiny::tags$br(),
      DT::DTOutput(ns("table"))
    )
  )
}

mod_browser_server <- function(id, data, db_path, selected_ids_rv, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    applied_filters <- shiny::reactiveVal(default_browser_filters())
    applied_at <- shiny::reactiveVal(NULL)

    current_inputs <- shiny::reactive({
      list(
        id_query = input$id_query,
        id_mode = input$id_mode,
        genotype_query = input$genotype_query,
        genotype_gene = input$genotype_gene,
        genotype_value = input$genotype_value,
        sex = input$sex,
        dob_range = input$dob_range,
        age_range = input$age_range,
        mouse_line = input$mouse_line,
        generation = input$generation,
        alive_filter = input$alive_filter,
        breeder_filter = input$breeder_filter,
        ready_filter = input$ready_filter,
        reservation_filter = input$reservation_filter,
        cohort_filter = input$cohort_filter,
        flag_query = input$flag_query
      )
    })

    filtered_rows <- shiny::reactive({
      apply_mouse_filters(data(), applied_filters())
    })

    output$summary <- shiny::renderText({
      stamp <- applied_at()
      stamp_text <- if (is.null(stamp)) {
        "No filter run yet."
      } else {
        paste("Last applied at", format(stamp, "%H:%M:%S"))
      }

      paste(nrow(filtered_rows()), "mice match the applied filters using AND logic.", stamp_text)
    })

    output$table <- DT::renderDT({
      display_df <- filtered_rows() |>
        dplyr::select(
          mouse_id, sex, dob, age_label, alive, is_breeder, experiment_ready,
          mouse_line, generation, raw_genotype, cohort_label, local_flags,
          reservation_status
        )

      DT::datatable(
        display_df,
        selection = "multiple",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, scrollY = 480)
      )
    })

    shiny::observe({
      df <- data()
      if (nrow(df) == 0) {
        return()
      }

      shiny::updateSelectizeInput(
        session, "mouse_line",
        choices = sort(unique(stats::na.omit(df$mouse_line))),
        selected = isolate(normalize_filter_values(input$mouse_line)),
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session, "generation",
        choices = sort(unique(stats::na.omit(df$generation))),
        selected = isolate(normalize_filter_values(input$generation)),
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session, "cohort_filter",
        choices = stats::setNames(c("", sort(unique(stats::na.omit(df$cohort_label)))), c("", sort(unique(stats::na.omit(df$cohort_label))))),
        selected = isolate(input$cohort_filter) %||% "",
        server = TRUE
      )

      geno_cols <- genotype_column_names(df)
      shiny::updateSelectizeInput(
        session, "genotype_gene",
        choices = stats::setNames(c("", geno_cols), c("Any parsed gene", stringr::str_replace(geno_cols, "^geno_", ""))),
        selected = isolate(input$genotype_gene) %||% ""
      )

      dob_values <- stats::na.omit(df$dob)
      if (length(dob_values) > 0) {
        shiny::updateDateRangeInput(
          session, "dob_range",
          start = min(dob_values),
          end = max(dob_values),
          min = min(dob_values),
          max = max(dob_values)
        )
      }

      age_values <- stats::na.omit(df$age_weeks)
      if (length(age_values) > 0) {
        shiny::updateSliderInput(
          session, "age_range",
          min = floor(min(age_values)),
          max = ceiling(max(age_values)),
          value = c(floor(min(age_values)), ceiling(max(age_values)))
        )
      }

      meta <- get_colony_metadata(db_path)
      shiny::updateSelectInput(
        session,
        "load_preset",
        choices = stats::setNames(c("", meta$presets$preset_name), c("", meta$presets$preset_name))
      )
    })

    shiny::observeEvent(input$genotype_gene, {
      df <- data()
      gene_name <- trim_na(input$genotype_gene)

      if (is.na(gene_name) || !gene_name %in% names(df)) {
        shiny::updateSelectizeInput(session, "genotype_value", choices = character(0), selected = "")
        return()
      }

      values <- sort(unique(stats::na.omit(df[[gene_name]])))
      shiny::updateSelectizeInput(
        session,
        "genotype_value",
        choices = stats::setNames(c("", values), c("", values)),
        selected = ""
      )
    })

    shiny::observeEvent(input$apply_filters, {
      filters <- current_inputs()

      tryCatch({
        filtered <- apply_mouse_filters(data(), filters)
        applied_filters(filters)
        applied_at(Sys.time())
        set_status(paste("Applied colony filters.", nrow(filtered), "mice match the current subset."), "success")
      }, error = function(e) {
        set_status(paste("Filter failed:", conditionMessage(e)), "error")
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$table_rows_selected, {
      rows <- filtered_rows()
      selected <- rows$mouse_id[input$table_rows_selected]
      selected_ids_rv(unique(stats::na.omit(selected)))
    })

    shiny::observeEvent(input$save_preset, {
      ok <- save_filter_preset(db_path, input$preset_name, current_inputs())
      if (isTRUE(ok)) {
        set_status(paste("Saved preset", input$preset_name), "success")
        meta <- get_colony_metadata(db_path)
        shiny::updateSelectInput(
          session,
          "load_preset",
          choices = stats::setNames(c("", meta$presets$preset_name), c("", meta$presets$preset_name)),
          selected = input$preset_name
        )
      }
    })

    shiny::observeEvent(input$apply_preset, {
      preset <- load_filter_preset(db_path, input$load_preset)
      if (is.null(preset)) {
        return()
      }

      shiny::updateTextInput(session, "id_query", value = preset$id_query %||% "")
      shiny::updateRadioButtons(session, "id_mode", selected = preset$id_mode %||% "contains")
      shiny::updateTextInput(session, "genotype_query", value = preset$genotype_query %||% "")
      shiny::updateSelectizeInput(session, "genotype_gene", selected = preset$genotype_gene %||% "")
      shiny::updateSelectizeInput(session, "genotype_value", selected = preset$genotype_value %||% "")
      shiny::updateCheckboxGroupInput(session, "sex", selected = preset$sex %||% character(0))
      shiny::updateDateRangeInput(session, "dob_range", start = preset$dob_range[[1]] %||% NULL, end = preset$dob_range[[2]] %||% NULL)
      if (!is.null(preset$age_range)) {
        shiny::updateSliderInput(session, "age_range", value = unlist(preset$age_range))
      }
      shiny::updateSelectizeInput(session, "mouse_line", selected = preset$mouse_line %||% character(0))
      shiny::updateSelectizeInput(session, "generation", selected = preset$generation %||% character(0))
      shiny::updateSelectInput(session, "alive_filter", selected = preset$alive_filter %||% "all")
      shiny::updateSelectInput(session, "breeder_filter", selected = preset$breeder_filter %||% "all")
      shiny::updateSelectInput(session, "ready_filter", selected = preset$ready_filter %||% "all")
      shiny::updateSelectInput(session, "reservation_filter", selected = preset$reservation_filter %||% "all")
      shiny::updateSelectizeInput(session, "cohort_filter", selected = preset$cohort_filter %||% "")
      shiny::updateTextInput(session, "flag_query", value = preset$flag_query %||% "")
      set_status("Preset loaded. Click Apply Filters to update the table.", "info")
    })

    shiny::observeEvent(input$clear_filters, {
      defaults <- default_browser_filters()
      shiny::updateTextInput(session, "id_query", value = defaults$id_query)
      shiny::updateRadioButtons(session, "id_mode", selected = defaults$id_mode)
      shiny::updateTextInput(session, "genotype_query", value = defaults$genotype_query)
      shiny::updateSelectizeInput(session, "genotype_gene", selected = defaults$genotype_gene)
      shiny::updateSelectizeInput(session, "genotype_value", selected = defaults$genotype_value)
      shiny::updateCheckboxGroupInput(session, "sex", selected = defaults$sex)
      shiny::updateSelectizeInput(session, "mouse_line", selected = defaults$mouse_line)
      shiny::updateSelectizeInput(session, "generation", selected = defaults$generation)
      shiny::updateSelectInput(session, "alive_filter", selected = defaults$alive_filter)
      shiny::updateSelectInput(session, "breeder_filter", selected = defaults$breeder_filter)
      shiny::updateSelectInput(session, "ready_filter", selected = defaults$ready_filter)
      shiny::updateSelectInput(session, "reservation_filter", selected = defaults$reservation_filter)
      shiny::updateSelectizeInput(session, "cohort_filter", selected = defaults$cohort_filter)
      shiny::updateTextInput(session, "flag_query", value = defaults$flag_query)
      df <- data()
      dob_values <- stats::na.omit(df$dob)
      if (length(dob_values) > 0) {
        shiny::updateDateRangeInput(
          session, "dob_range",
          start = min(dob_values),
          end = max(dob_values)
        )
      }
      age_values <- stats::na.omit(df$age_weeks)
      if (length(age_values) > 0) {
        shiny::updateSliderInput(
          session, "age_range",
          value = c(floor(min(age_values)), ceiling(max(age_values)))
        )
      }
      set_status("Filter inputs cleared. Click Apply Filters to refresh the subset.", "info")
    })

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0("filtered_colony_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(filtered_rows(), file)
    )

    list(filtered_rows = filtered_rows)
  })
}
