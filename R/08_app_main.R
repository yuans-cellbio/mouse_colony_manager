create_mouse_colony_app <- function() {
  ui <- colony_app_ui()
  shiny::shinyApp(ui = ui, server = colony_app_server)
}
