# Displays a table of the current linelist

linelistUI <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("linelist_table"))
}

#' Table of linelist
#' 
#' Displays a datatable of the linelist
#' 
#' @param id Shiny module ID
#' @param linelist Reactive expression returning person node attributes
#' @param state List of the main app state for the target (selection)
linelistServer <- function(id, linelist, state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$linelist_table <- renderDT(
        linelist() %>%
          select(CaseNumber, Initials, Sex, AgeAtOnset, Symptoms, SymptomsOnsetDate, CalculatedOnsetDate, DiagnosisDate, Acquired,
                 Address, Occupation, Language, CountryOfBirth, Classification, RecordOwner) %>%
          mutate_if(is.character, enc2utf8) %>%
          datatable(style="bootstrap",
                    selection = "single",
                    rownames = F,
                    extensions = 'Buttons',
                    options = list(lengthChange = F,
                                   scrollX = "auto",
                                   dom = 'Bfrtip',
                                   buttons = c('csv', 'copy'))),
        server = FALSE)
      
      # highlight target row
      observe({
        req(state$selecting.element != "linelist_table")
        ix = which(linelist()$ID == state$target.id)
        if (length(ix)>0) {
          page = (ix-1) %/% 10 + 1
          dataTableProxy("linelist_table") %>%
            selectRows(ix) %>%
            selectPage(page)
        } else {
          dataTableProxy('linelist_table') %>%
            selectRows(NULL)
        }
      })
    }
  )
}