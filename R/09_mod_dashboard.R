metric_output_card <- function(id, title, subtitle = NULL) {
  bslib::card(
    class = "h-100 metric-card",
    bslib::card_body(
      shiny::tags$div(class = "metric-label", title),
      shiny::textOutput(id, container = function(...) shiny::tags$div(class = "metric-value", ...)),
      if (!is.null(subtitle)) shiny::tags$p(class = "filter-summary", subtitle)
    )
  )
}

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "app-hero",
      shiny::tags$h1("Colony Overview"),
      shiny::tags$p("A startup dashboard for colony status, import health, experiment readiness, and operational follow-up.")
    ),
    instruction_card(
      "How to use this page",
      "Use Dashboard for a quick census and recent activity. Import new files from the Import page, then return here to review counts, trends, and follow-up items."
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 3,
      metric_output_card(ns("metric_active"), "Active mice", "Currently live in the working database"),
      metric_output_card(ns("metric_breeders"), "Active breeders", "Local breeder flags and live status"),
      metric_output_card(ns("metric_ready"), "Experiment ready", "Local experiment-ready flags"),
      metric_output_card(ns("metric_reservations"), "Reserved or on hold", "Current operational reservations"),
      metric_output_card(ns("metric_births"), "Births in last 30 days"),
      metric_output_card(ns("metric_ended"), "Ended in last 30 days")
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "dashboard-plot-card",
        full_screen = TRUE,
        bslib::card_header("Births And Ended Mice"),
        bslib::card_body(
          shiny::plotOutput(ns("timeline_plot"), height = 430)
        )
      ),
      bslib::card(
        class = "dashboard-plot-card",
        full_screen = TRUE,
        bslib::card_header("Genotype Distribution By Line"),
        bslib::card_body(
          shiny::plotOutput(ns("genotype_plot"), height = 430)
        )
      )
    ),
    shiny::tags$br(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Action Queue"),
        bslib::card_body(
          DT::DTOutput(ns("action_queue"))
        )
      ),
      bslib::card(
        class = "dashboard-table-card",
        full_screen = TRUE,
        bslib::card_header("Latest Imports"),
        bslib::card_body(
          DT::DTOutput(ns("imports"))
        )
      )
    )
  )
}

mod_dashboard_server <- function(id, data, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    metrics <- shiny::reactive(summarize_colony_dashboard(data()))

    output$metric_active <- shiny::renderText(metrics()$active)
    output$metric_breeders <- shiny::renderText(metrics()$breeders)
    output$metric_ready <- shiny::renderText(metrics()$experiment_ready)
    output$metric_reservations <- shiny::renderText(metrics()$reservations)
    output$metric_births <- shiny::renderText(metrics()$recent_births)
    output$metric_ended <- shiny::renderText(metrics()$recent_ended)

    output$timeline_plot <- shiny::renderPlot({
      plot_birth_death_timeline(data())
    }, res = 110)

    output$genotype_plot <- shiny::renderPlot({
      plot_genotype_distribution(data())
    }, res = 110)

    output$action_queue <- DT::renderDT({
      DT::datatable(
        build_action_queue(data()),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = 280,
          dom = "tip"
        )
      )
    })

    output$imports <- DT::renderDT({
      import_df <- metadata()$imports |>
        dplyr::mutate(imported_at = format(imported_at, "%Y-%m-%d %H:%M"))

      DT::datatable(
        import_df,
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          scrollY = 280
        )
      )
    })
  })
}
