# A modal for selecting an ID from a table, filtered by a dygraph

mysteryUI <- function(id) {
  ns = NS(id)
  actionButton(ns("mysterycases"), "Mystery cases", class="btn-secondary", icon=icon("question"))
}

#' Mystery case selector module
#' 
#' Shiny server module for selecting from a list of mystery cases from the data
#' model, limited to the latest 100 for now.
#' 
#' @param id Shiny server module ID
#' @param conn Database connection to `phess-dw01`
#' 
#' @return CaseNumber of the selected case
mysteryServer <- function(id, conn) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Trigger modal to appear
      observeEvent(input$mysterycases, {
        showModal(mysteryModal())
      })
      
      # Modal layout
      mysteryModal <- function() {
        ns = session$ns
        modalDialog(
          title = "Mystery cases",
          size = "l",
          easyClose = T,
          DTOutput(ns("mysterytable"))
        )
      }
      
      # Reactive data frame is loaded when the mystery menu is opened
      mystery.df = reactive({
        print("Retrieving mystery cases")
        withProgress(
          message = "Retrieving last 100 mystery cases",
          dbGetQuery(conn,
                     "
                     SELECT
                       CaseNumber, DiagnosisDate AS Dx, Symptoms, Classification, Acquired
                     FROM V_LineList
                     WHERE Classification IN ('Confirmed', 'Probable') AND
                       Acquired IN ('Acquired in Australia, unknown source', 'Under investigation')
                     ORDER BY DiagnosisDate DESC
                     LIMIT 100")
        )
      })
      
      # Filtered datatable
      output$mysterytable = renderDT(
        mystery.df() %>%
          mutate_at(vars(Symptoms, Classification, Acquired), function (x) fct_drop(fct_explicit_na(x))),
        server = F,
        style = "bootstrap",
        selection = "single",
        rownames = F,
        filter = "top",
        options = list(lengthChange = FALSE,
                       columnDefs = list(
                         list(
                           targets = which(names(data) == "Acquired")-1,
                           render = JS(
                             "function(data, type, row, meta) {",
                             "return type === 'display' && data.length > 12 ?",
                             "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                             "}")
                         )
                       )
        )
      )
      
      
      # Capture row click and return to main
      mystery_id = eventReactive(input$mysterytable_rows_selected, {
        mystery_id = mystery.df()$CaseNumber[input$mysterytable_rows_selected]
        removeModal()
        mystery_id
      })
      
      return(mystery_id)
    }
  )
}