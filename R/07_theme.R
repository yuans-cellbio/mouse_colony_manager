mouse_colony_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#f3efe6",
    fg = "#18211f",
    primary = "#215c52",
    secondary = "#a66c35",
    success = "#456c2f",
    info = "#355d82",
    warning = "#c0892d",
    danger = "#a6463c",
    base_font = bslib::font_collection(
      "Segoe UI Variable Display",
      "Bahnschrift",
      "Trebuchet MS",
      "sans-serif"
    ),
    heading_font = bslib::font_collection(
      "Palatino Linotype",
      "Book Antiqua",
      "Georgia",
      "serif"
    )
  ) |>
    bslib::bs_add_rules("
      body {
        background:
          radial-gradient(circle at top right, rgba(33, 92, 82, 0.14), transparent 28%),
          radial-gradient(circle at bottom left, rgba(166, 108, 53, 0.12), transparent 25%),
          #f3efe6;
        padding-bottom: 4.5rem;
      }
      .navbar {
        box-shadow: 0 10px 30px rgba(24, 33, 31, 0.08);
      }
      .bslib-card {
        border: 1px solid rgba(24, 33, 31, 0.08);
        box-shadow: 0 12px 35px rgba(24, 33, 31, 0.08);
        border-radius: 20px;
      }
      .metric-value {
        font-family: 'Palatino Linotype', 'Book Antiqua', serif;
        font-size: 2rem;
        line-height: 1;
        color: #18211f;
      }
      .metric-label {
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #5a665f;
        font-size: 0.78rem;
      }
      .app-hero {
        padding: 1rem 0 1.4rem 0;
      }
      .app-hero h1 {
        margin-bottom: 0.35rem;
      }
      .app-hero p {
        margin: 0;
        color: #5a665f;
        max-width: 60rem;
      }
      .filter-summary {
        color: #5a665f;
        font-size: 0.9rem;
      }
      .instruction-card {
        border-left: 5px solid #215c52;
        background: rgba(255, 255, 255, 0.7);
      }
      .instruction-card p:last-child {
        margin-bottom: 0;
      }
      .dashboard-plot-card .card-body,
      .dashboard-table-card .card-body {
        min-height: 28rem;
      }
      .metric-card .card-body {
        min-height: 8.5rem;
      }
      .app-status-bar {
        position: fixed;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1050;
        display: flex;
        align-items: center;
        gap: 0.75rem;
        padding: 0.85rem 1.25rem;
        background: rgba(24, 33, 31, 0.95);
        color: #f8f5ee;
        box-shadow: 0 -12px 30px rgba(24, 33, 31, 0.2);
      }
      .app-status-pill {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 6rem;
        padding: 0.25rem 0.7rem;
        border-radius: 999px;
        font-size: 0.74rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }
      .app-status-bar.info .app-status-pill {
        background: #355d82;
      }
      .app-status-bar.running .app-status-pill {
        background: #c0892d;
        color: #18211f;
      }
      .app-status-bar.success .app-status-pill {
        background: #456c2f;
      }
      .app-status-bar.error .app-status-pill {
        background: #a6463c;
      }
      .app-status-text {
        font-size: 0.95rem;
        line-height: 1.4;
      }
      .shiny-input-container {
        margin-bottom: 0.9rem;
      }
    ")
}

metric_card <- function(title, value, subtitle = NULL) {
  bslib::card(
    class = "h-100 metric-card",
    bslib::card_body(
      shiny::tags$div(class = "metric-label", title),
      shiny::tags$div(class = "metric-value", value),
      if (!is.null(subtitle)) shiny::tags$p(class = "filter-summary", subtitle)
    )
  )
}

instruction_card <- function(title, text) {
  bslib::card(
    class = "instruction-card",
    bslib::card_body(
      shiny::tags$strong(title),
      shiny::tags$p(class = "filter-summary", text)
    )
  )
}

status_bar_ui <- function() {
  shiny::uiOutput("app_status_bar")
}
