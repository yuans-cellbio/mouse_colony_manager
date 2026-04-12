mod_pedigree_ui <- function(id) {
  ns <- shiny::NS(id)
  default_engine <- if (legacy_pedigree_dependencies_available()) "legacy" else "ggpedigree"

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Pedigree Controls",
      width = 360,
      open = "desktop",
      instruction_card(
        "Drawing behavior",
        "Choose the source subset, adjust the labels and overlays, and click Draw pedigree. The custom ggped engine splits each node into equal feature slices and restores breeder links and feature legends. The ggpedigree engine keeps a simpler single-fill style."
      ),
      shiny::tags$br(),
      shiny::selectInput(ns("source"), "Source subset", choices = c(
        "Selected mice" = "selected",
        "Browser filtered set" = "browser",
        "Lineage result" = "lineage",
        "Manual IDs" = "manual"
      )),
      shiny::textAreaInput(ns("manual_ids"), "Manual IDs", rows = 4, placeholder = "Used only when source is Manual IDs"),
      shiny::selectInput(
        ns("engine"),
        "Pedigree engine",
        choices = c(
          "Custom split-node (ggped)" = "legacy",
          "Simple fill (ggpedigree)" = "ggpedigree"
        ),
        selected = default_engine
      ),
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
      shiny::selectizeInput(ns("feature_fields"), "Node features / genes", choices = NULL, multiple = TRUE),
      shiny::radioButtons(
        ns("canvas_mode"),
        "Canvas sizing",
        choices = c("Auto" = "auto", "Manual" = "manual"),
        selected = "auto",
        inline = TRUE
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(ns("canvas_width_in"), "Canvas width (in)", value = 30, min = 8, max = 100, step = 1),
        shiny::numericInput(ns("canvas_height_in"), "Canvas height (in)", value = 18, min = 6, max = 80, step = 1)
      ),
      shiny::helpText("Manual width and height are used only when Canvas sizing is set to Manual. Auto sizing will still populate these boxes after each draw so you can fine-tune from there."),
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
      shiny::uiOutput(ns("plot_container")),
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
      result <- pedigree_result()
      if (is.null(result)) {
        return(paste(length(selected_source_ids()), "mice are currently staged for pedigree plotting."))
      }

      specs <- result$payload$render_specs
      paste(
        nrow(result$payload$data),
        "mice plotted on a",
        paste0(format(round(specs$width_in, 1), nsmall = 1), " x ", format(round(specs$height_in, 1), nsmall = 1), " in"),
        "canvas."
      )
    })

    shiny::observeEvent(input$draw, {
      ids <- selected_source_ids()
        if (length(ids) == 0) {
          set_status("Choose at least one mouse before drawing a pedigree.", "error")
          return()
        }

      engine_label <- if (identical(input$engine, "legacy")) "custom ggped" else "ggpedigree"
      set_status(paste("Drawing pedigree with", engine_label, "."), "running")

      tryCatch({
        render_overrides <- if (identical(input$canvas_mode, "manual")) {
          list(
            width_in = input$canvas_width_in,
            height_in = input$canvas_height_in
          )
        } else {
          NULL
        }

        payload <- build_pedigree_data(
          data(),
          mouse_ids = ids,
          label_fields = input$label_fields,
          feature_fields = input$feature_fields,
          render_overrides = render_overrides
        )
        plot_obj <- draw_pedigree(payload, engine = input$engine)
        shiny::updateNumericInput(session, "canvas_width_in", value = round(payload$render_specs$width_in, 1))
        shiny::updateNumericInput(session, "canvas_height_in", value = round(payload$render_specs$height_in, 1))
        pedigree_result(list(payload = payload, plot = plot_obj))
        rendered_engine <- attr(plot_obj, "engine_used") %||% input$engine
        set_status(paste("Pedigree ready with", rendered_engine, ".", nrow(payload$data), "mice were plotted."), "success")
      }, error = function(e) {
        set_status(paste("Pedigree drawing failed:", conditionMessage(e)), "error")
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    output$plot_container <- shiny::renderUI({
      result <- pedigree_result()
      if (is.null(result)) {
        return(instruction_card("No pedigree yet", "Draw a pedigree to open the scrollable canvas and export controls."))
      }

      specs <- result$payload$render_specs
      shiny::tags$div(
        style = "overflow: auto; max-width: 100%; max-height: 78vh; background: white; border: 1px solid rgba(24,33,31,0.08); border-radius: 14px; padding: 0.75rem;",
        shiny::plotOutput(
          session$ns("plot"),
          width = paste0(specs$plot_width_px, "px"),
          height = paste0(specs$plot_height_px, "px")
        )
      )
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
        specs <- result$payload$render_specs
        ggplot2::ggsave(file, plot = result$plot, width = specs$width_in, height = specs$height_in, dpi = 300, limitsize = FALSE, bg = "white")
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content = function(file) {
        result <- pedigree_result()
        shiny::req(result)
        specs <- result$payload$render_specs
        ggplot2::ggsave(file, plot = result$plot, width = specs$width_in, height = specs$height_in, limitsize = FALSE, bg = "white")
      }
    )
  })
}
