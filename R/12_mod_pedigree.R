mod_pedigree_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Pedigree Controls",
      width = 360,
      open = "desktop",
      instruction_card(
        "Drawing behavior",
        "Choose the source subset, adjust the labels and overlays, and click Draw pedigree. This page now uses ggpedigree directly."
      ),
      shiny::tags$br(),
      shiny::selectInput(ns("source"), "Source subset", choices = c(
        "Selected mice" = "selected",
        "Browser filtered set" = "browser",
        "Lineage result" = "lineage",
        "Manual IDs" = "manual"
      )),
      shiny::textAreaInput(ns("manual_ids"), "Manual IDs", rows = 4, placeholder = "Used only when source is Manual IDs"),
      shiny::checkboxGroupInput(
        ns("label_fields"),
        "Label fields",
        choices = c(
          "Mouse ID" = "mouse_id",
          "Sex" = "sex",
          "Age" = "age_label",
          "Genotype" = "raw_genotype",
          "Generation" = "generation",
          "Line" = "mouse_line",
          "Cohort" = "cohort_label"
        ),
        selected = c("mouse_id", "age_label", "raw_genotype")
      ),
      shiny::selectizeInput(ns("feature_fields"), "Feature overlays", choices = NULL, multiple = TRUE),
      shiny::actionButton(ns("draw"), "Draw pedigree", class = "btn-primary")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Pedigree"),
      shiny::tags$p(class = "filter-summary", shiny::textOutput(ns("summary"), inline = TRUE)),
      shiny::downloadButton(ns("download_png"), "Export PNG"),
      shiny::downloadButton(ns("download_pdf"), "Export PDF"),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::plotOutput(ns("plot"), height = 760),
      shiny::tags$br(),
      DT::DTOutput(ns("subset_table"))
    )
  )
}

mod_pedigree_server <- function(id, data, selected_ids_rv, browser_filtered, lineage_result_r, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    pedigree_result <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::updateSelectizeInput(
        session,
        "feature_fields",
        choices = pedigree_feature_columns(data()),
        server = TRUE
      )
    })

    selected_source_ids <- shiny::reactive({
      switch(
        input$source,
        "selected" = selected_ids_rv(),
        "browser" = browser_filtered()$mouse_id,
        "lineage" = lineage_result_r()$ids %||% character(0),
        "manual" = parse_seed_ids(input$manual_ids),
        character(0)
      )
    })

    output$summary <- shiny::renderText({
      paste(length(selected_source_ids()), "mice are currently staged for pedigree plotting.")
    })

    shiny::observeEvent(input$draw, {
      ids <- selected_source_ids()
      if (length(ids) == 0) {
        set_status("Choose at least one mouse before drawing a pedigree.", "error")
        return()
      }

      if (length(ids) > 150) {
        set_status("Choose fewer than 150 mice for pedigree plotting.", "error")
        return()
      }

      set_status("Drawing pedigree with ggpedigree.", "running")

      tryCatch({
        payload <- build_pedigree_data(
          data(),
          mouse_ids = ids,
          label_fields = input$label_fields,
          feature_fields = input$feature_fields
        )
        plot_obj <- draw_pedigree(payload)
        pedigree_result(list(payload = payload, plot = plot_obj))
        set_status(paste("Pedigree ready.", nrow(payload$data), "mice were plotted."), "success")
      }, error = function(e) {
        set_status(paste("Pedigree drawing failed:", conditionMessage(e)), "error")
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    output$plot <- shiny::renderPlot({
      result <- pedigree_result()
      shiny::req(result)
      print(result$plot)
    }, res = 110)

    output$subset_table <- DT::renderDT({
      result <- pedigree_result()
      shiny::req(result)

      DT::datatable(
        result$payload$data |>
          dplyr::select(mouse_id = personID, sex, age_label, alive, raw_genotype, mouse_line, generation),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = 240)
      )
    })

    output$download_png <- shiny::downloadHandler(
      filename = function() paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content = function(file) {
        result <- pedigree_result()
        shiny::req(result)
        ggplot2::ggsave(file, plot = result$plot, width = 16, height = 10, dpi = 300)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content = function(file) {
        result <- pedigree_result()
        shiny::req(result)
        ggplot2::ggsave(file, plot = result$plot, width = 16, height = 10)
      }
    )
  })
}
