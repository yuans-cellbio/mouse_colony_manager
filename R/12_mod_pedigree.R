pedigree_feature_title_input_id <- function(feature_field) {
  paste0("feature_title_", sanitize_name(feature_field))
}

pedigree_feature_na_input_id <- function(feature_field) {
  paste0("feature_na_", sanitize_name(feature_field))
}

pedigree_feature_value_input_id <- function(feature_field, value_index) {
  paste0("feature_colour_", sanitize_name(feature_field), "_", value_index)
}

pedigree_viewer_ui_assets <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .pedigree-viewer-shell {
        background: white;
        border: 1px solid rgba(24,33,31,0.08);
        border-radius: 14px;
        overflow: hidden;
      }
      .pedigree-viewer-toolbar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.75rem;
        flex-wrap: wrap;
        padding: 0.75rem 0.9rem;
        border-bottom: 1px solid rgba(24,33,31,0.08);
        background: rgba(243, 239, 230, 0.78);
      }
      .pedigree-viewer-toolbar-left {
        display: flex;
        align-items: center;
        gap: 0.45rem;
        flex-wrap: wrap;
      }
      .pedigree-viewer-toolbar-right {
        color: #5a665f;
        font-size: 0.88rem;
      }
      .pedigree-viewer-toolbar .btn {
        padding: 0.3rem 0.7rem;
        border-radius: 999px;
      }
      .pedigree-zoom-readout {
        min-width: 3.5rem;
        text-align: center;
        font-weight: 600;
        color: #18211f;
      }
      .pedigree-viewer-viewport {
        position: relative;
        overflow: hidden;
        width: 100%;
        height: min(78vh, 62rem);
        min-height: 28rem;
        background: white;
        cursor: grab;
        touch-action: none;
        overscroll-behavior: contain;
      }
      .pedigree-viewer-viewport.dragging {
        cursor: grabbing;
      }
      .pedigree-viewer-stage {
        position: absolute;
        top: 0;
        left: 0;
        transform-origin: 0 0;
        will-change: transform;
        backface-visibility: hidden;
        contain: layout paint style;
      }
      .pedigree-viewer-stage img,
      .pedigree-viewer-stage canvas {
        display: block;
        user-select: none;
        -webkit-user-drag: none;
        pointer-events: none;
        max-width: none !important;
      }
    ")),
    shiny::tags$script(shiny::HTML("
      (function() {
        function clamp(value, min, max) {
          return Math.min(Math.max(value, min), max);
        }

        function findContentSize(root) {
          var plot = root.querySelector('.shiny-plot-output');
          if (!plot) {
            return { width: 0, height: 0 };
          }

          var inner = plot.querySelector('img, canvas');
          var width = 0;
          var height = 0;

          width = plot.clientWidth || plot.offsetWidth || 0;
          height = plot.clientHeight || plot.offsetHeight || 0;

          if ((!width || !height) && inner) {
            var rect = inner.getBoundingClientRect();
            width = rect.width || inner.clientWidth || inner.width || 0;
            height = rect.height || inner.clientHeight || inner.height || 0;
          }

          if ((!width || !height) && plot.style) {
            width = width || parseFloat(plot.style.width) || 0;
            height = height || parseFloat(plot.style.height) || 0;
          }

          return { width: width, height: height };
        }

        function fallbackBounds(root) {
          var size = findContentSize(root);
          return {
            left: 0,
            top: 0,
            width: size.width || 0,
            height: size.height || 0
          };
        }

        function extractVisibleBounds(pixelData, sourceWidth, sourceHeight, displayWidth, displayHeight) {
          var minX = sourceWidth;
          var minY = sourceHeight;
          var maxX = -1;
          var maxY = -1;

          for (var y = 0; y < sourceHeight; y += 1) {
            for (var x = 0; x < sourceWidth; x += 1) {
              var idx = (y * sourceWidth + x) * 4;
              var r = pixelData[idx];
              var g = pixelData[idx + 1];
              var b = pixelData[idx + 2];
              var a = pixelData[idx + 3];

              if (a <= 8) {
                continue;
              }

              if (r >= 248 && g >= 248 && b >= 248) {
                continue;
              }

              if (x < minX) minX = x;
              if (y < minY) minY = y;
              if (x > maxX) maxX = x;
              if (y > maxY) maxY = y;
            }
          }

          if (maxX < minX || maxY < minY) {
            return {
              left: 0,
              top: 0,
              width: displayWidth,
              height: displayHeight
            };
          }

          var scaleX = displayWidth / sourceWidth;
          var scaleY = displayHeight / sourceHeight;

          return {
            left: minX * scaleX,
            top: minY * scaleY,
            width: (maxX - minX + 1) * scaleX,
            height: (maxY - minY + 1) * scaleY
          };
        }

        function graphicCacheKey(inner, bounds) {
          if (!inner) {
            return null;
          }

          if (inner.tagName === 'IMG') {
            return [
              'img',
              inner.currentSrc || inner.src || '',
              inner.naturalWidth || 0,
              inner.naturalHeight || 0,
              bounds.width || 0,
              bounds.height || 0
            ].join('|');
          }

          return [
            'canvas',
            inner.width || 0,
            inner.height || 0,
            bounds.width || 0,
            bounds.height || 0
          ].join('|');
        }

        function refreshVisibleBounds(root, onReady) {
          var state = root.__pedigreeViewerState;
          var plot = root.querySelector('.shiny-plot-output');
          var inner = plot && plot.querySelector('img, canvas');
          var fallback = fallbackBounds(root);

          if (!state) {
            return;
          }

          if (!inner || !fallback.width || !fallback.height) {
            state.visibleBounds = fallback;
            if (onReady) {
              onReady(fallback);
            }
            return;
          }

          var cacheKey = graphicCacheKey(inner, fallback);
          if (state.visibleBoundsCacheKey === cacheKey && state.visibleBounds) {
            if (onReady) {
              onReady(state.visibleBounds);
            }
            return;
          }

          function store(bounds) {
            state.visibleBounds = bounds;
            state.visibleBoundsCacheKey = cacheKey;
            if (onReady) {
              onReady(bounds);
            }
          }

          if (inner.tagName === 'CANVAS') {
            try {
              var ctx = inner.getContext('2d', { willReadFrequently: true });
              var canvasData = ctx.getImageData(0, 0, inner.width, inner.height).data;
              store(extractVisibleBounds(canvasData, inner.width, inner.height, fallback.width, fallback.height));
            } catch (err) {
              store(fallback);
            }
            return;
          }

          if (!inner.complete || !inner.naturalWidth || !inner.naturalHeight) {
            inner.addEventListener('load', function handleLoad() {
              refreshVisibleBounds(root, onReady);
            }, { once: true });
            return;
          }

          try {
            var scratch = document.createElement('canvas');
            scratch.width = inner.naturalWidth;
            scratch.height = inner.naturalHeight;
            var scratchCtx = scratch.getContext('2d', { willReadFrequently: true });
            scratchCtx.drawImage(inner, 0, 0);
            var imageData = scratchCtx.getImageData(0, 0, scratch.width, scratch.height).data;
            store(extractVisibleBounds(imageData, scratch.width, scratch.height, fallback.width, fallback.height));
          } catch (err) {
            store(fallback);
          }
        }

        function renderTransform(root) {
          var state = root.__pedigreeViewerState;
          var stage = root.querySelector('.pedigree-viewer-stage');
          if (!state || !stage) {
            return;
          }

          stage.style.transform = 'translate3d(' + state.x + 'px, ' + state.y + 'px, 0) scale(' + state.scale + ')';

          var readout = root.querySelector('.pedigree-zoom-readout');
          if (readout) {
            readout.textContent = Math.round(state.scale * 100) + '%';
          }

          state.renderQueued = false;
        }

        function requestRender(root) {
          var state = root.__pedigreeViewerState;
          if (!state) {
            return;
          }

          if (state.renderQueued) {
            return;
          }

          state.renderQueued = true;
          window.requestAnimationFrame(function() {
            renderTransform(root);
          });
        }

        function centerOffsets(viewportWidth, viewportHeight, contentWidth, contentHeight, scale) {
          return {
            x: (viewportWidth - contentWidth * scale) / 2,
            y: (viewportHeight - contentHeight * scale) / 2
          };
        }

        function centerAtScale(root, scale, mode, bounds) {
          var state = root.__pedigreeViewerState;
          var viewport = root.querySelector('.pedigree-viewer-viewport');
          bounds = bounds || fallbackBounds(root);

          if (!state || !viewport || !bounds.width || !bounds.height) {
            return;
          }

          scale = clamp(scale, state.minScale, state.maxScale);
          if (!isFinite(scale) || scale <= 0) {
            scale = 1;
          }

          state.scale = scale;
          var offsets = centerOffsets(viewport.clientWidth || 1, viewport.clientHeight || 1, bounds.width, bounds.height, scale);
          state.x = offsets.x - bounds.left * scale;
          state.y = offsets.y - bounds.top * scale;
          state.mode = mode || state.mode || 'fit';
          requestRender(root);
        }

        function fitToViewport(root) {
          var state = root.__pedigreeViewerState;
          var viewport = root.querySelector('.pedigree-viewer-viewport');

          if (!state || !viewport) {
            return;
          }

          refreshVisibleBounds(root, function(bounds) {
            if (!bounds || !bounds.width || !bounds.height) {
              return;
            }

            var viewportWidth = viewport.clientWidth || 1;
            var viewportHeight = viewport.clientHeight || 1;
            var fitScale = Math.min(viewportWidth / bounds.width, viewportHeight / bounds.height);

            centerAtScale(root, fitScale, 'fit', bounds);
          });
        }

        function resetToActualSize(root) {
          centerAtScale(root, 1, 'actual', fallbackBounds(root));
        }

        function zoomAt(root, factor, clientX, clientY) {
          var state = root.__pedigreeViewerState;
          var viewport = root.querySelector('.pedigree-viewer-viewport');
          if (!state || !viewport) {
            return;
          }

          var rect = viewport.getBoundingClientRect();
          var targetX = typeof clientX === 'number' ? clientX - rect.left : rect.width / 2;
          var targetY = typeof clientY === 'number' ? clientY - rect.top : rect.height / 2;
          var nextScale = clamp(state.scale * factor, state.minScale, state.maxScale);

          if (nextScale === state.scale) {
            return;
          }

          var scaleRatio = nextScale / state.scale;
          state.x = targetX - (targetX - state.x) * scaleRatio;
          state.y = targetY - (targetY - state.y) * scaleRatio;
          state.scale = nextScale;
          state.mode = 'free';
          requestRender(root);
        }

        function attachPlotObserver(root) {
          var plot = root.querySelector('.shiny-plot-output');
          if (!plot || root.__pedigreePlotObserver) {
            return;
          }

          var rerender = function() {
            window.requestAnimationFrame(function() {
              var state = root.__pedigreeViewerState || {};
              if (state.mode === 'fit') {
                fitToViewport(root);
              } else {
                resetToActualSize(root);
              }
            });
          };

          var observer = new MutationObserver(function() {
            rerender();
          });

          observer.observe(plot, {
            childList: true,
            subtree: true,
            attributes: true,
            attributeFilter: ['src', 'style']
          });

          root.__pedigreePlotObserver = observer;
        }

        function initPedigreeViewer(rootId) {
          var root = document.getElementById(rootId);
          if (!root) {
            return;
          }

          if (!root.__pedigreeViewerState) {
            root.__pedigreeViewerState = {
              scale: 1,
              minScale: 0.1,
              maxScale: 8,
              x: 0,
              y: 0,
              mode: 'fit',
              dragging: false,
              dragOriginX: 0,
              dragOriginY: 0,
              renderQueued: false,
              visibleBounds: null,
              visibleBoundsCacheKey: null
            };
          }

          var state = root.__pedigreeViewerState;
          var viewport = root.querySelector('.pedigree-viewer-viewport');
          if (!viewport) {
            return;
          }

          if (!root.__pedigreeViewerBound) {
            viewport.addEventListener('wheel', function(event) {
              event.preventDefault();
              zoomAt(root, event.deltaY < 0 ? 1.12 : 1 / 1.12, event.clientX, event.clientY);
            }, { passive: false });

            viewport.addEventListener('pointerdown', function(event) {
              if (event.button !== 0) {
                return;
              }
              event.preventDefault();
              state.dragging = true;
              state.dragOriginX = event.clientX - state.x;
              state.dragOriginY = event.clientY - state.y;
              viewport.classList.add('dragging');
              if (viewport.setPointerCapture) {
                viewport.setPointerCapture(event.pointerId);
              }
            });

            viewport.addEventListener('pointermove', function(event) {
              if (!state.dragging) {
                return;
              }
              event.preventDefault();
              state.x = event.clientX - state.dragOriginX;
              state.y = event.clientY - state.dragOriginY;
              state.mode = 'free';
              requestRender(root);
            });

            var stopDragging = function(event) {
              state.dragging = false;
              viewport.classList.remove('dragging');
              if (event && viewport.releasePointerCapture) {
                try {
                  viewport.releasePointerCapture(event.pointerId);
                } catch (err) {}
              }
            };

            viewport.addEventListener('pointerup', stopDragging);
            viewport.addEventListener('pointercancel', stopDragging);
            viewport.addEventListener('pointerleave', function(event) {
              if (state.dragging) {
                stopDragging(event);
              }
            });

            root.addEventListener('click', function(event) {
              var button = event.target.closest('[data-pedigree-action]');
              if (!button) {
                return;
              }

              var action = button.getAttribute('data-pedigree-action');
              if (action === 'zoom-in') {
                zoomAt(root, 1.2);
              } else if (action === 'zoom-out') {
                zoomAt(root, 1 / 1.2);
              } else if (action === 'fit') {
                fitToViewport(root);
              } else if (action === 'actual') {
                resetToActualSize(root);
              }
            });

            window.addEventListener('resize', function() {
              if (state.mode === 'fit') {
                fitToViewport(root);
              } else if (state.mode === 'actual') {
                resetToActualSize(root);
              } else {
                requestRender(root);
              }
            });

            root.__pedigreeViewerBound = true;
          }

          attachPlotObserver(root);
          fitToViewport(root);
        }

        window.initPedigreeViewer = initPedigreeViewer;

        if (window.Shiny && window.Shiny.addCustomMessageHandler) {
          window.Shiny.addCustomMessageHandler('pedigree-viewer-init', function(message) {
            initPedigreeViewer(message.rootId);
          });
        }
      })();
    "))
  )
}

pedigree_feature_editor_block <- function(ns, feature_field, display_name, values, default_palette) {
  value_controls <- lapply(seq_along(values), function(idx) {
    value_label <- values[[idx]]
    input_id <- ns(pedigree_feature_value_input_id(feature_field, idx))
    default_value <- default_palette[[value_label]] %||% "#D8D2C6"

    shiny::tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; gap: 0.75rem; margin-bottom: 0.45rem;",
      shiny::tags$span(style = "font-size: 0.92rem; line-height: 1.25;", value_label),
      shiny::tags$input(
        id = input_id,
        type = "color",
        value = default_value,
        style = "width: 3.25rem; height: 2rem; padding: 0; border: none; background: transparent;"
      )
    )
  })

  shiny::tags$div(
    style = "border: 1px solid rgba(24,33,31,0.08); border-radius: 14px; padding: 0.85rem 0.9rem; background: rgba(255,255,255,0.72); margin-bottom: 0.85rem;",
    shiny::textInput(
      ns(pedigree_feature_title_input_id(feature_field)),
      label = paste(display_name, "legend title"),
      value = display_name
    ),
    shiny::tags$p(
      class = "filter-summary",
      style = "margin-top: -0.25rem; margin-bottom: 0.55rem;",
      "Recorded genotype states in the staged subset:"
    ),
    value_controls,
    shiny::tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; gap: 0.75rem; margin-top: 0.5rem;",
      shiny::tags$span(style = "font-size: 0.92rem; line-height: 1.25;", "Missing value / placeholder"),
      shiny::tags$input(
        id = ns(pedigree_feature_na_input_id(feature_field)),
        type = "color",
        value = "#D8D2C6",
        style = "width: 3.25rem; height: 2rem; padding: 0; border: none; background: transparent;"
      )
    )
  )
}

mod_pedigree_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    pedigree_viewer_ui_assets(),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Pedigree Controls",
        width = 360,
        open = "desktop",
        instruction_card(
          "Drawing behavior",
          "This page uses your local ggped renderer. Pick a subset source, choose up to 4 recorded genes from that subset, adjust the genotype colors if needed, and click Draw pedigree."
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
        shiny::selectizeInput(
          ns("feature_fields"),
          "Genes to color",
          choices = NULL,
          multiple = TRUE,
          options = list(
            maxItems = 4,
            plugins = list("remove_button"),
            placeholder = "Choose up to 4 genes recorded in this subset"
          )
        ),
        shiny::helpText("The segmented ggped symbols support at most 4 genes at once. Legends are built from the genes you choose here."),
        shiny::uiOutput(ns("feature_mapping_ui")),
        shiny::radioButtons(
          ns("canvas_mode"),
          "Canvas sizing",
          choices = c("Auto" = "auto", "Manual" = "manual"),
          selected = "auto",
          inline = TRUE
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::numericInput(ns("canvas_width_in"), "Canvas width (in)", value = 30, min = 8, max = 120, step = 1),
          shiny::numericInput(ns("canvas_height_in"), "Canvas height (in)", value = 18, min = 6, max = 90, step = 1)
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
  )
}

mod_pedigree_server <- function(id, data, selected_ids_rv, browser_filtered, lineage_result_r, set_status) {
  shiny::moduleServer(id, function(input, output, session) {
    pedigree_result <- shiny::reactiveVal(NULL)
    feature_selection <- shiny::reactiveVal(character(0))

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

    selected_source_rows <- shiny::reactive({
      subset_pedigree_source_rows(data(), selected_source_ids())
    })

    available_feature_catalog <- shiny::reactive({
      pedigree_available_features(selected_source_rows())
    })

    shiny::observeEvent(available_feature_catalog(), {
      catalog <- available_feature_catalog()
      current <- intersect(feature_selection(), catalog$source_field)

      if (length(current) == 0) {
        current <- head(catalog$source_field, min(2, length(catalog$source_field)))
      } else {
        current <- head(current, 4)
      }

      feature_selection(current)
      shiny::freezeReactiveValue(input, "feature_fields")
      shiny::updateSelectizeInput(
        session,
        "feature_fields",
        choices = stats::setNames(catalog$source_field, catalog$display_name),
        selected = current,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$feature_fields, {
      catalog <- available_feature_catalog()
      selected <- intersect(input$feature_fields %||% character(0), catalog$source_field)
      feature_selection(head(selected, 4))
    }, ignoreInit = TRUE)

    output$feature_mapping_ui <- shiny::renderUI({
      catalog <- available_feature_catalog()
      selected_fields <- intersect(feature_selection(), catalog$source_field)

      if (nrow(catalog) == 0) {
        return(instruction_card(
          "No recorded genes",
          "The staged subset does not contain any parsed gene columns with values yet. Pick a different subset or refresh the colony data."
        ))
      }

      if (length(selected_fields) == 0) {
        return(instruction_card(
          "Choose genes",
          "Select one or more genes above to edit their genotype colors and include them in the segmented pedigree symbols."
        ))
      }

      shiny::tagList(
        shiny::tags$p(
          class = "filter-summary",
          paste("Available genes in this subset:", paste(catalog$display_name, collapse = ", "))
        ),
        lapply(selected_fields, function(field) {
          row_idx <- match(field, catalog$source_field)
          values <- catalog$values[[row_idx]]
          default_palette <- default_pedigree_discrete_palette(field, values)
          pedigree_feature_editor_block(
            ns = session$ns,
            feature_field = field,
            display_name = catalog$display_name[[row_idx]],
            values = values,
            default_palette = default_palette
          )
        })
      )
    })

    collect_color_overrides <- function(catalog, selected_fields) {
      overrides <- vector("list", length(selected_fields))
      names(overrides) <- selected_fields

      for (field in selected_fields) {
        row_idx <- match(field, catalog$source_field)
        values <- catalog$values[[row_idx]]
        default_palette <- default_pedigree_discrete_palette(field, values)

        selected_values <- vapply(seq_along(values), function(idx) {
          input_id <- pedigree_feature_value_input_id(field, idx)
          trim_na(input[[input_id]] %||% default_palette[[values[[idx]]]])
        }, character(1))

        overrides[[field]] <- list(
          name = trim_na(input[[pedigree_feature_title_input_id(field)]] %||% catalog$display_name[[row_idx]]),
          na.value = trim_na(input[[pedigree_feature_na_input_id(field)]] %||% "#D8D2C6"),
          values = stats::setNames(selected_values, values)
        )
      }

      overrides
    }

    output$summary <- shiny::renderText({
      result <- pedigree_result()
      if (is.null(result)) {
        catalog <- available_feature_catalog()
        staged_count <- nrow(selected_source_rows())
        if (nrow(catalog) == 0) {
          return(paste(staged_count, "mice are currently staged. No recorded genes were detected in this subset yet."))
        }
        return(paste(
          staged_count,
          "mice are currently staged.",
          "Available genes:",
          paste(catalog$display_name, collapse = ", ")
        ))
      }

      specs <- result$payload$render_specs
      gene_labels <- result$payload$feature_map$display_name %||% character(0)
      gene_text <- if (length(gene_labels) == 0) {
        "No genes were colored."
      } else {
        paste("Genes shown:", paste(gene_labels, collapse = ", "))
      }

      paste(
        nrow(result$payload$data),
        "plot rows on a",
        paste0(format(round(specs$width_in, 1), nsmall = 1), " x ", format(round(specs$height_in, 1), nsmall = 1), " in"),
        "canvas.",
        gene_text
      )
    })

    shiny::observeEvent(input$draw, {
      ids <- selected_source_ids()
      if (length(ids) == 0) {
        set_status("Choose at least one mouse before drawing a pedigree.", "error")
        return()
      }

      catalog <- available_feature_catalog()
      selected_features <- intersect(feature_selection(), catalog$source_field)
      if (length(selected_features) == 0) {
        set_status("Choose at least one recorded gene before drawing a pedigree.", "error")
        return()
      }

      set_status("Drawing pedigree with the local ggped renderer.", "running")

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
          feature_fields = selected_features,
          render_overrides = render_overrides
        )

        plot_obj <- draw_pedigree(
          payload,
          color_overrides = collect_color_overrides(catalog, selected_features)
        )

        shiny::updateNumericInput(session, "canvas_width_in", value = round(payload$render_specs$width_in, 1))
        shiny::updateNumericInput(session, "canvas_height_in", value = round(payload$render_specs$height_in, 1))
        pedigree_result(list(payload = payload, plot = plot_obj))
        session$onFlushed(function() {
          session$sendCustomMessage("pedigree-viewer-init", list(rootId = session$ns("viewer_shell")))
        }, once = TRUE)
        set_status(
          paste(
            "Pedigree ready with the local ggped renderer.",
            nrow(payload$data),
            "plot rows were rendered across",
            nrow(payload$feature_map),
            "genes."
          ),
          "success"
        )
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
        id = session$ns("viewer_shell"),
        class = "pedigree-viewer-shell",
        shiny::tags$div(
          class = "pedigree-viewer-toolbar",
          shiny::tags$div(
            class = "pedigree-viewer-toolbar-left",
            shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", `data-pedigree-action` = "zoom-out", "-"),
            shiny::tags$span(class = "pedigree-zoom-readout", "100%"),
            shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", `data-pedigree-action` = "zoom-in", "+"),
            shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", `data-pedigree-action` = "fit", "Fit"),
            shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", `data-pedigree-action` = "actual", "100%")
          ),
          shiny::tags$div(
            class = "pedigree-viewer-toolbar-right",
            "Scroll to zoom and drag to move"
          )
        ),
        shiny::tags$div(
          class = "pedigree-viewer-viewport",
          shiny::tags$div(
            class = "pedigree-viewer-stage",
            shiny::plotOutput(
              session$ns("plot"),
              width = paste0(specs$plot_width_px, "px"),
              height = paste0(specs$plot_height_px, "px")
            )
          )
        )
      )
    })

    output$plot <- shiny::renderPlot({
      result <- pedigree_result()
      shiny::req(result)
      render_pedigree_plot(result$plot)
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
        save_pedigree_plot(result$plot, filename = file, width = specs$width_in, height = specs$height_in, dpi = 300)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
      content = function(file) {
        result <- pedigree_result()
        shiny::req(result)
        specs <- result$payload$render_specs
        save_pedigree_plot(result$plot, filename = file, width = specs$width_in, height = specs$height_in, dpi = 300)
      }
    )
  })
}
