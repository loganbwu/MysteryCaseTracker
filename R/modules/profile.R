# show a table for a single person's profile or site

profileUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Case summary"),
    div(
      style = "overflow-x: auto",
      tableOutput(ns("this_case"))
    )
  )
}

#' Display the selected profile(s)
#' 
#' Creates a table of the selected case numbers depending on the type(s). Allows
#' for multiple case numbers of mixed type (person records or exposure site
#' records)
#' 
#' @param id Shiny module ID
#' @param df Dataframe of selected nodes' attributes
profileServer <- function(id, df) {
  # df: A single-row data frame
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Process the incoming node attributes according to type
      this.case = reactive({
        df = df()
        shiny::validate(
          need(nrow(df) != 0, "ID not found in nodes")
        )
        
        # Show different fields depending on what the user has selected
        if (all(df$Type == "Person")) {
          df = df %>%
            mutate(Symptoms = ifelse(is.na(SymptomsOnsetDate), Symptoms, paste(Symptoms, SymptomsOnsetDate, sep=", "))) %>%
            select(CaseNumber, Initials, Sex, AgeAtOnset, Symptoms, DiagnosisDate, Classification, Acquired, Occupation, Address, RecordOwner)
        }
        else if (all(df$Type == "Exposure site")) {
          df = df %>%
            select(CaseNumber, ClusterName, Address, SiteCategory1, SiteCategory2)
        }
        else {
          df = df %>%
            unite("Name", Initials, ClusterName, na.rm=T) %>%
            unite("Classification", Classification, ClusterType, na.rm=T) %>%
            unite("Occ/Type", Occupation, SiteCategory1, SiteCategory2, sep=" - ", na.rm=T) %>%
            mutate(Symptoms = ifelse(is.na(SymptomsOnsetDate), Symptoms, paste(Symptoms, SymptomsOnsetDate, sep=", "))) %>%
            select(CaseNumber, Name, Symptoms, Dx=DiagnosisDate, `Occ/Type`, Classification, Sex, AgeAtOnset, Acquired, Address, RecordOwner)
        }
        df
      })
      
      # Simple transposed table showing attributes of case
      output$this_case <- renderTable(
        this.case() %>% t %>% as.data.frame,
        rownames=T, colnames=F, striped=T, width="100%"
      )
    }
  )
}