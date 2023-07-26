# Displays a table of the current exposure sites

casemovementUI <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("casemovement_table"))
}

#' Table of case movements
#' 
#' Displays a datatable of case movements
#' 
#' @param id Shiny module ID
#' @param casemovement Reactive expression returning case movement node attributes
#' @param state List of the main app state for the target (selection)
casemovementServer <- function(id, casemovement, state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$casemovement_table <- renderDT(
        casemovement() %>%
          select(CoordID, CaseNumber, MovementPlace, MovementDate, MovementTime, Address, AddressType) %>% 
          mutate_if(is.character, enc2utf8) %>%
          datatable(style="bootstrap",
                    selection = "single",
                    rownames = F,
                    extensions = "Buttons",
                    options = list(lengthChange = F,
                                   scrollX = "auto",
                                   dom = "Bfrtip",
                                   buttons = c("csv", "copy"),
                                   columnDefs=list(list(visible=F,targets=0)))),
        server = FALSE)
      
      # search instead of selecting to show all relevant rows
      observe({
        req(state$selecting.element != "casemovement_table")
        ix = which(casemovement()$CoordID == state$target.id |
                     casemovement()$CaseNumber == state$target.id)
        if (length(ix) > 0) {
          dataTableProxy("casemovement_table") %>%
            updateSearch(keywords = list(global=state$target.id))
        } else {
          dataTableProxy("casemovement_table") %>%
            clearSearch()
        }
      })
    }
  )
}