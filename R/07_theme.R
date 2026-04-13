mouse_colony_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg        = "#F8F9F7",
    fg        = "#1A2928",
    primary   = "#1A5C50",
    secondary = "#B07030",
    success   = "#2B6738",
    info      = "#1E4F7A",
    warning   = "#BE7A18",
    danger    = "#9C2E2E",
    base_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", wght = "300;400;500;600"),
      "system-ui",
      "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Playfair Display", wght = "400;600;700"),
      "Georgia",
      "serif"
    ),
    code_font = bslib::font_collection(
      bslib::font_google("IBM Plex Mono"),
      "monospace"
    )
  ) |>
    bslib::bs_add_rules("

      /* ── Custom properties ──────────────────────────────────── */
      :root {
        --teal-50:      #EBF4F2;
        --teal-100:     #C8E4DF;
        --teal-500:     #1A5C50;
        --teal-700:     #103D35;
        --amber-400:    #CFA050;
        --amber-500:    #B07030;
        --surface-0:    #F8F9F7;
        --surface-1:    #FFFFFF;
        --surface-2:    #F3F5F3;
        --border:       rgba(26, 41, 40, 0.09);
        --border-mid:   rgba(26, 41, 40, 0.16);
        --shadow-xs:    0 1px 2px rgba(26, 41, 40, 0.05);
        --shadow-sm:    0 1px 4px rgba(26, 41, 40, 0.06), 0 4px 14px rgba(26, 41, 40, 0.05);
        --shadow-md:    0 2px 8px rgba(26, 41, 40, 0.08), 0 8px 24px rgba(26, 41, 40, 0.07);
        --ease:         cubic-bezier(0.4, 0, 0.2, 1);
        --dur:          140ms;
      }

      /* ── Base ───────────────────────────────────────────────── */
      body {
        background-color: var(--surface-0);
        padding-bottom: 4.5rem;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        font-weight: 400;
      }

      /* ── Navbar ─────────────────────────────────────────────── */
      .navbar {
        background-color: var(--surface-1) !important;
        border-bottom: 1px solid var(--border);
        box-shadow: none;
        padding-top: 0;
        padding-bottom: 0;
      }
      .navbar-brand {
        font-family: 'Playfair Display', Georgia, serif;
        font-weight: 700;
        font-size: 1.1rem;
        letter-spacing: -0.01em;
        color: var(--teal-500) !important;
        padding: 0.9rem 0;
      }
      .navbar .nav-link {
        font-size: 0.845rem;
        font-weight: 500;
        letter-spacing: 0.015em;
        padding: 1rem 1rem !important;
        border-bottom: 2px solid transparent;
        transition: border-color var(--dur) var(--ease),
                    color var(--dur) var(--ease);
        margin-bottom: -1px;
        color: #4A5E5C !important;
      }
      .navbar .nav-link.active {
        border-bottom-color: var(--teal-500);
        color: var(--teal-500) !important;
      }
      .navbar .nav-link:hover:not(.active) {
        border-bottom-color: var(--teal-100);
        color: var(--teal-500) !important;
      }

      /* ── Cards ──────────────────────────────────────────────── */
      .bslib-card {
        background: var(--surface-1);
        border: 1px solid var(--border);
        border-radius: 8px;
        box-shadow: var(--shadow-sm);
        transition: box-shadow var(--dur) var(--ease);
      }
      .bslib-card:hover {
        box-shadow: var(--shadow-md);
      }
      .card-header {
        background: transparent;
        border-bottom: 1px solid var(--border);
        font-size: 0.82rem;
        font-weight: 700;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: #4A5E5C;
        padding: 0.75rem 1.1rem;
      }
      .card-body {
        padding: 1rem 1.1rem;
      }

      /* ── Metric Cards ───────────────────────────────────────── */
      .metric-card {
        border-top: 3px solid var(--teal-500) !important;
        border-radius: 8px !important;
        overflow: hidden;
      }
      .metric-card .card-body {
        min-height: 8rem;
        padding: 1rem 1.2rem 1.1rem;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        gap: 0.3rem;
      }
      .metric-label {
        font-size: 0.7rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: #7A9290;
        margin-bottom: 0.1rem;
      }
      .metric-value {
        font-family: 'Playfair Display', Georgia, serif;
        font-size: 2.8rem;
        font-weight: 700;
        line-height: 1;
        color: var(--teal-500);
        letter-spacing: -0.03em;
      }

      /* ── Dashboard content cards ────────────────────────────── */
      .dashboard-plot-card .card-body,
      .dashboard-table-card .card-body {
        min-height: 28rem;
      }

      /* ── Page hero header ───────────────────────────────────── */
      .app-hero {
        padding: 1.1rem 0 1.4rem 0;
        border-bottom: 1px solid var(--border);
        margin-bottom: 1.4rem;
      }
      .app-hero h1 {
        font-family: 'Playfair Display', Georgia, serif;
        font-size: 1.85rem;
        font-weight: 700;
        letter-spacing: -0.025em;
        color: #1A2928;
        margin-bottom: 0.3rem;
        line-height: 1.15;
      }
      .app-hero p {
        margin: 0;
        color: #6B8280;
        font-size: 0.9rem;
        max-width: 64rem;
        line-height: 1.65;
      }

      /* ── Instruction cards ──────────────────────────────────── */
      .instruction-card {
        border: none !important;
        border-left: 3px solid var(--teal-500) !important;
        border-radius: 0 6px 6px 0 !important;
        background: var(--teal-50) !important;
        box-shadow: none !important;
      }
      .instruction-card:hover {
        box-shadow: none !important;
      }
      .instruction-card .card-body {
        padding: 0.7rem 0.95rem;
      }
      .instruction-card strong {
        display: block;
        font-size: 0.72rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.09em;
        color: var(--teal-500);
        margin-bottom: 0.25rem;
      }
      .instruction-card p:last-child {
        margin-bottom: 0;
      }

      /* ── Filter summary / muted text ────────────────────────── */
      .filter-summary {
        color: #6B8280;
        font-size: 0.875rem;
        line-height: 1.55;
      }

      /* ── Buttons ────────────────────────────────────────────── */
      .btn {
        font-weight: 500;
        font-size: 0.845rem;
        border-radius: 5px;
        padding: 0.42rem 0.95rem;
        transition: all var(--dur) var(--ease);
        letter-spacing: 0.01em;
      }
      .btn-primary {
        background: var(--teal-500);
        border-color: var(--teal-500);
      }
      .btn-primary:hover, .btn-primary:focus {
        background: var(--teal-700);
        border-color: var(--teal-700);
        box-shadow: 0 2px 10px rgba(26, 92, 80, 0.30);
        transform: translateY(-1px);
      }
      .btn-secondary {
        background: transparent;
        border-color: var(--border-mid);
        color: #2A3D3C;
      }
      .btn-secondary:hover {
        background: var(--surface-2);
        border-color: var(--teal-500);
        color: var(--teal-500);
        transform: translateY(-1px);
      }
      .btn-outline-secondary {
        border-color: var(--border-mid);
        color: #4A5E5C;
        background: transparent;
      }
      .btn-outline-secondary:hover {
        border-color: var(--teal-500);
        color: var(--teal-500);
        background: var(--teal-50);
      }

      /* ── Form inputs ────────────────────────────────────────── */
      .shiny-input-container {
        margin-bottom: 0.8rem;
      }
      .shiny-input-container label,
      .control-label {
        font-size: 0.78rem;
        font-weight: 600;
        color: #3A4F4E;
        letter-spacing: 0.03em;
        margin-bottom: 0.28rem;
      }
      .form-control, .form-select {
        border: 1px solid rgba(26, 41, 40, 0.18);
        border-radius: 5px;
        font-size: 0.875rem;
        background-color: var(--surface-1);
        color: #1A2928;
        transition: border-color var(--dur) var(--ease),
                    box-shadow var(--dur) var(--ease);
      }
      .form-control:focus, .form-select:focus {
        border-color: var(--teal-500);
        box-shadow: 0 0 0 3px rgba(26, 92, 80, 0.13);
        background-color: var(--surface-1);
      }
      .selectize-input {
        border: 1px solid rgba(26, 41, 40, 0.18) !important;
        border-radius: 5px !important;
        font-size: 0.875rem !important;
        box-shadow: none !important;
        background-color: var(--surface-1) !important;
        transition: border-color var(--dur) var(--ease) !important;
      }
      .selectize-input.focus {
        border-color: var(--teal-500) !important;
        box-shadow: 0 0 0 3px rgba(26, 92, 80, 0.13) !important;
      }
      .selectize-dropdown {
        border: 1px solid var(--border-mid) !important;
        border-radius: 5px !important;
        box-shadow: var(--shadow-md) !important;
      }
      .selectize-dropdown .option.active {
        background-color: var(--teal-50) !important;
        color: var(--teal-700) !important;
      }
      .selectize-dropdown .option.selected {
        background-color: var(--teal-100) !important;
        color: var(--teal-700) !important;
      }

      /* ── Checkboxes & radios ────────────────────────────────── */
      .form-check-input:checked {
        background-color: var(--teal-500);
        border-color: var(--teal-500);
      }
      .form-check-input:focus {
        box-shadow: 0 0 0 3px rgba(26, 92, 80, 0.15);
        border-color: var(--teal-500);
      }
      .form-check-label {
        font-size: 0.875rem;
        color: #2A3D3C;
      }

      /* ── Range slider ───────────────────────────────────────── */
      .irs--shiny .irs-bar {
        background: var(--teal-500);
        border-top: 1px solid var(--teal-500);
        border-bottom: 1px solid var(--teal-500);
      }
      .irs--shiny .irs-handle {
        background: #fff;
        border: 2px solid var(--teal-500);
        box-shadow: 0 1px 4px rgba(26, 92, 80, 0.2);
      }
      .irs--shiny .irs-handle:hover,
      .irs--shiny .irs-handle.state_hover {
        background: var(--teal-50);
      }
      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single {
        background: var(--teal-500);
        font-size: 0.75rem;
        font-family: 'IBM Plex Mono', monospace;
      }

      /* ── Sidebar ────────────────────────────────────────────── */
      .bslib-sidebar-layout > .sidebar {
        background: #F3F6F5;
        border-right: 1px solid var(--border);
      }
      .sidebar-title {
        font-size: 0.72rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: var(--teal-500);
        padding-bottom: 0.55rem;
        border-bottom: 1px solid var(--teal-100);
        margin-bottom: 0.8rem;
      }

      /* ── DataTables ─────────────────────────────────────────── */
      table.dataTable {
        font-size: 0.855rem;
      }
      table.dataTable thead th,
      table.dataTable thead td {
        font-size: 0.72rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.07em;
        color: #5A7270;
        border-bottom: 2px solid var(--teal-100) !important;
        padding: 0.6rem 0.7rem;
        background: transparent;
        white-space: nowrap;
      }
      table.dataTable thead th:after,
      table.dataTable thead th:before {
        color: #8AADAA;
      }
      table.dataTable tbody td {
        padding: 0.5rem 0.7rem;
        border-bottom: 1px solid var(--border);
        color: #1A2928;
        vertical-align: middle;
      }
      table.dataTable tbody tr:hover > td {
        background-color: var(--teal-50) !important;
      }
      table.dataTable tbody tr.selected > td {
        background-color: var(--teal-100) !important;
        color: var(--teal-700);
        box-shadow: inset 3px 0 0 var(--teal-500);
      }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border: 1px solid var(--border-mid);
        border-radius: 5px;
        font-size: 0.845rem;
        padding: 0.3rem 0.55rem;
        color: #1A2928;
      }
      .dataTables_wrapper .dataTables_filter input:focus,
      .dataTables_wrapper .dataTables_length select:focus {
        border-color: var(--teal-500);
        outline: none;
        box-shadow: 0 0 0 3px rgba(26, 92, 80, 0.12);
      }
      .dataTables_wrapper .dataTables_info {
        font-size: 0.8rem;
        color: #7A9290;
        padding-top: 0.6rem;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: var(--teal-500) !important;
        color: #fff !important;
        border-color: var(--teal-500) !important;
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: var(--teal-50) !important;
        color: var(--teal-500) !important;
        border-color: var(--teal-100) !important;
        border-radius: 4px;
      }

      /* ── Accordion ──────────────────────────────────────────── */
      .accordion-button {
        font-size: 0.875rem;
        font-weight: 600;
        color: #2A3D3C;
      }
      .accordion-button:not(.collapsed) {
        color: var(--teal-500);
        background-color: var(--teal-50);
        box-shadow: none;
      }
      .accordion-button:focus {
        box-shadow: 0 0 0 3px rgba(26, 92, 80, 0.15);
      }
      .accordion-item {
        border: 1px solid var(--border);
        border-radius: 6px !important;
        overflow: hidden;
        margin-bottom: 0.4rem;
      }

      /* ── Help text ──────────────────────────────────────────── */
      .help-block, .form-text, .shiny-input-container .help-block {
        font-size: 0.775rem;
        color: #8AADAA;
        margin-top: 0.22rem;
        line-height: 1.4;
      }

      /* ── Status bar ─────────────────────────────────────────── */
      .app-status-bar {
        position: fixed;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1050;
        display: flex;
        align-items: center;
        gap: 0.75rem;
        padding: 0.6rem 1.4rem;
        background: #111C1B;
        border-top: 1px solid rgba(255, 255, 255, 0.05);
        color: #A8C4C0;
      }
      .app-status-pill {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 5.5rem;
        padding: 0.18rem 0.6rem;
        border-radius: 3px;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 0.66rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        flex-shrink: 0;
      }
      .app-status-bar.info .app-status-pill {
        background: rgba(30, 79, 122, 0.80);
        color: #BBDAF0;
      }
      .app-status-bar.running .app-status-pill {
        background: rgba(190, 122, 24, 0.85);
        color: #FDE8C0;
        animation: pulse-pill 1.4s ease-in-out infinite;
      }
      @keyframes pulse-pill {
        0%, 100% { opacity: 1; }
        50%       { opacity: 0.55; }
      }
      .app-status-bar.success .app-status-pill {
        background: rgba(43, 103, 56, 0.85);
        color: #C0E0C8;
      }
      .app-status-bar.error .app-status-pill {
        background: rgba(156, 46, 46, 0.85);
        color: #F0CCCC;
      }
      .app-status-text {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 0.82rem;
        color: #8AADAA;
        line-height: 1;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      /* ── Pedigree viewer toolbar ────────────────────────────── */
      .pedigree-viewer-toolbar {
        background: #F3F6F5 !important;
        border-bottom: 1px solid var(--border) !important;
      }
      .pedigree-viewer-toolbar .btn {
        font-size: 0.78rem;
        padding: 0.28rem 0.6rem;
        border-radius: 4px;
      }
      .pedigree-zoom-readout {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 0.78rem;
        font-weight: 600;
        color: #3A5250;
        min-width: 3.2rem;
        text-align: center;
      }

      /* ── Misc spacing ───────────────────────────────────────── */
      hr {
        border-color: var(--border);
        opacity: 1;
      }
      .bslib-gap-spacing {
        gap: 0.9rem;
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