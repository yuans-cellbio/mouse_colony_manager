mod_lineage_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Trace Controls",
      width = 340,
      open = "desktop",
      instruction_card(
        "Tracing behavior",
        "Choose one or more seed mice, decide whether to search upward, downward, or both, then click Trace lineage. The result can be sent directly to the Pedigree page."
      ),
      shiny::tags$br(),
      shiny::textAreaInput(ns("seed_ids"), "Seed mouse IDs", placeholder = "One or more mouse IDs, separated by comma, space, or newline", rows = 5),
      shiny::selectInput(ns("direction"), "Direction", choices = c("Both" = "both", "Ancestors only" = "up", "Descendants only" = "down")),
      shiny::numericInput(ns("generations"), "Generations", value = 3, min = 1, max = 20),
      shiny::checkboxInput(ns("exhaustive"), "Exhaustive search", value = FALSE),
      shiny::checkboxInput(ns("focus_siblings"), "Include seed mouse siblings", value = TRUE),
      shiny::checkboxInput(ns("ancestor_siblings"), "Include ancestor siblings", value = FALSE),
      shiny::checkboxInput(ns("descendant_siblings"), "Include descendant siblings", value = FALSE),
      shiny::actionButton(ns("run_lineage"), "Trace lineage", class = "btn-primary")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Lineage Trace"),
      shiny::tags$p(class = "filter-summary", shiny::textOutput(ns("summary"), inline = TRUE)),
      shiny::downloadButton(ns("download_csv"), "Export lineage CSV"),
      shiny::tags$br(),
      shiny::tags$br(),
      DT::DTOutput(ns("table"))
    )
  )
}

mod_lineage_server <- function(id, data, selected_ids_rv, lineage_result_rv, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    lineage_result <- shiny::reactiveVal(empty_lineage_result())

    shiny::observeEvent(input$run_lineage, {
      set_status("Tracing lineage graph.", "running")
      result <- trace_lineage(
        seed_ids = parse_seed_ids(input$seed_ids),
        data = data(),
        direction = input$direction,
        max_generations = input$generations,
        exhaustive = isTRUE(input$exhaustive),
        include_focus_siblings = isTRUE(input$focus_siblings),
        include_ancestor_siblings = isTRUE(input$ancestor_siblings),
        include_descendant_siblings = isTRUE(input$descendant_siblings)
      )
      lineage_result(result)
      lineage_result_rv(result)
      set_status(paste("Lineage trace complete.", length(result$ids), "mice in the result."), "success")
    }, ignoreInit = TRUE)

    output$summary <- shiny::renderText({
      result <- lineage_result()
      paste(length(result$ids), "mice in the lineage result.")
    })

    output$table <- DT::renderDT({
      result <- lineage_result()
      display_rows <- result$rows

      if (nrow(display_rows) == 0) {
        display_rows <- tibble::tibble(
          mouse_id = character(), lineage_roles = character(), lineage_depth = integer(),
          sex = character(), age_label = character(), alive = logical(),
          raw_genotype = character(), mouse_line = character(), generation = character()
        )
      }

      DT::datatable(
        display_rows |>
          dplyr::select(mouse_id, lineage_roles, lineage_depth, sex, age_label, alive, raw_genotype, mouse_line, generation),
        selection = "multiple",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$table_rows_selected, {
      result <- lineage_result()
      selected <- result$rows$mouse_id[input$table_rows_selected]
      selected_ids_rv(unique(stats::na.omit(selected)))
    })

    output$download_csv <- shiny::downloadHandler(
      filename = function() paste0("lineage_trace_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) readr::write_csv(lineage_result()$rows, file)
    )

    list(result = lineage_result)
  })
}