# A modal for settings

settingsUI <- function(id) {
  ns = NS(id)
  actionButton(ns("settings"), "Settings", class="btn-secondary", icon=icon("cog"))
}

#' Create a settings modal
#' 
#' Shiny server module function to manage MCT settings.
#' 
#' @param id Shiny module ID
#' @param settings List of current settings
#' @param all.classifications Vector of linelist classifications
#' 
#' @return list of new setting values
settingsServer <- function(id, settings, all.classifications) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Trigger modal to appear
      observeEvent(input$settings, {
        showModal(settingsModal())
      })
      
      # Modal layout
      settingsModal <- function() {
        ns = session$ns
        modalDialog(
          title = "Settings",
          pickerInput(ns("linelistclassifications"), "Linelist classifications",
                      choices = unique(c("Confirmed", "Probable", "Not notifiable", all.classifications)),
                      selected = settings$linelistclassifications,
                      multiple = T,
                      options = pickerOptions(actionsBox = T)),
          pickerInput(ns("exposuresitetypes"), "Exposure site types",
                      choices = c("Outbreak", "Exposure site", "Complex case",
                                  "Acquisition", "Placeholder", "(Missing)"),
                      selected = settings$exposuresitetypes,
                      multiple = T,
                      options = pickerOptions(actionsBox = T)),
          checkboxInput(ns("usecasemovements"), "Use case movements", settings$usecasemovements),
          checkboxInput(ns("limitids"), "Limit the maximum number of target IDs to 10 for performance", settings$limitids),
          
          footer = tagList(
            modalButton("Dismiss"),
            actionButton(ns("settingsdone"), "Apply settings", class="btn-primary")
          ),
          easyClose = T
        )
      }
      
      # Capture button click and return settings
      new_settings = eventReactive(input$settingsdone, {
        new_s = list()
        for (s in names(settings)) {
          new_s[[s]] = input[[s]]
        }
        removeModal()
        new_s
      })
      
      return(new_settings)
    }
  )
}