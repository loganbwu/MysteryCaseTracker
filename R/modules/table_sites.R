# Displays a table of the current exposure sites

sitesUI <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("sites_table"))
}

#' Table of current cluster sites
#' 
#' Displays a datatable of cluster sites
#' 
#' @param id Shiny module ID
#' @param clustersites Reactive expression returning cluster site node attributes
#' @param state List of the main app state for the target (selection)
sitesServer <- function(id, sites.df, state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Display datatable
      output$sites_table <- renderDT(
        sites.df() %>%
          select(CaseNumber, ClusterName, Address, SiteCategory1, SiteCategory2, ClusterType) %>%
          mutate_if(is.character, enc2utf8) %>%
          datatable(style="bootstrap",
                    selection = "single",
                    rownames = F,
                    extensions = "Buttons",
                    options = list(lengthChange = F,
                                   scrollX = "auto",
                                   dom = 'Bfrtip',
                                   buttons = c('csv', 'copy'))),
        server = FALSE)
      
      # highlight target row
      observe({
        req(state$selecting.element != "sites_table")
        ix = which(sites.df()$ID == state$target.id)
        if (length(ix)>0) {
          page = (ix-1) %/% 10 + 1
          dataTableProxy("sites_table") %>%
            selectRows(ix) %>%
            selectPage(page)
        } else {
          dataTableProxy('sites_table') %>%
            selectRows(NULL)
        }
      })
    }
  )
}