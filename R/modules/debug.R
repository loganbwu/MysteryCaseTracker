#' Show a collapsible data debugging UI
debugUI <- function(id, last_updated, app_initialised) {
  ns <- NS(id)
  
  tagList(
    tags$button(type="button", class="collapsible", "Data debugging"),
    tags$div(
      class="collapsible-content",
      tags$table(
        tags$tr(tags$td(paste("Data refreshed", last_updated %>% format("%Y-%m-%d %H:%M:%S")))),
        tags$tr(tags$td(paste("App initialised", app_initialised %>% format("%Y-%m-%d %H:%M:%S"))))
      ),
      actionButton(ns("restart"), "Restart app for all users (~15s)", icon=icon("trash"), class="btn-warning", style="width: 100%")
    ),
  )
}

#' Debug overall app data
#' 
#' Provide a place to display diagnostic information and restart the app if needed.
debugServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Restart the app
      observeEvent(input$restart, {
        stopApp()
      })
    }
  )
}