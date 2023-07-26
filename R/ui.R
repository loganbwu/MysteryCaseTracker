#' Define the MCT UI
#' 
#' Uses shiny module functions to split the app up.
navbarPageWithLogo(
  title = "COVID-19 Mystery Case Tracker",
  logo = tags$img(id = "navbar-logo", src = "www/DH Logo White.svg"),
  inverse = T,
  position = "fixed-top",
  tabPanel(
    "Contacts",
    tags$head(includeCSS("styles.css")),
    # override the default visNetwork.js which has incorrectly positioned tooltips in a modal
    tags$script(src = "visNetwork.js"),
    sidebarLayout(
      sidebarPanel(
        id = "sidebar",
        debugUI("debug", last_updated, app_initialised),
        settingsUI("settings"),
        mysteryUI("mystery"),
        tags$table(
          style="margin: 10px 4px 0 0;",
          tags$tr(width = "100%",
                  tags$td(width="20%", class="control-label", "Onset date range"),
                  tags$td(style="width: 80%; padding-left: 20px;",
                          sliderInput("daterange", NULL,
                                      min = as_date("2020-01-01"), max = today()+1,
                                      value = c(today()-days(30), today()+1),
                                      ticks = FALSE))),
          tags$tr(width = "100%",
                  tags$td(width="20%", class="control-label", "Degrees of separation"),
                  tags$td(style="width: 80%; padding-left: 20px;", sliderInput("order", NULL, 2, min=1, max=4, step=1, ticks=F))),
          tags$tr(width = "100%",
                  tags$td(width="20%", class="control-label", "Distance links"),
                  tags$td(style="width: 80%; padding-left: 20px;", sliderTextInput("distancethresholdtext", NULL, choices = c("Off", paste(seq(0, 500, 50), "m"))))),
          tags$tr(width = "100%", height="100%",
                  tags$td(width="20%", class="control-label", "Case Numbers"),
                  tags$td(style="width: 80%; padding-left: 20px;",
                          textInput("ids", NULL, NULL, placeholder="Separate multiple person/site records with a space/comma")))
        ),
        profileUI("thiscase"),
        timelineUI("timeline"),
        zoomtimelineUI("zoomtimeline", NULL, icon=icon("search-plus"), class="leaflet-control-layers")
      ),
      
      # Diagrams and tables
      mainPanel(
        style = "margin-top:64px",
        fluidRow(
          column(
            6,
            class = "col-eq-height",
            tags$table(
              tags$tr(width = "100%",
                      tags$td(width="20%", class="control-label", "Color nodes by"),
                      tags$td(width="80%", div(
                        style = "margin-bottom: -15px;",
                        selectInput("colorattribute", NULL,
                                    choices = c("Classification", "DiagnosisDate", "Symptoms", "AgeGroup", "Occupation", "Language", "CountryOfBirth", "Address", "Suburb"),
                                    selected = "Classification"))
                      )
              )
            ),
            div(
              style = "position: relative;",
              networkUI("network"),
              zoomnetworkUI("zoomnetwork", NULL, icon=icon("search-plus"), class="leaflet-control-layers",
                            style="position: absolute; bottom: 10px; z-index: 1001")
            )
          ),
          column(
            6,
            class = "col-eq-height",
            mapUI("map", height="100%"),
            zoommapUI("zoommap", NULL, icon=icon("search-plus"), class="leaflet-control-layers",
                      style="position: absolute; bottom: 10px; left: 25px; z-index: 1001; background-color: #ffffffcc")
          )
        ),
        h3("Related tables"),
        tabsetPanel(type = "pills",
                    id = "relatedtables",
                    tabPanel("Persons",
                             linelistUI("linelist"),
                             value = "linelisttab"),
                    tabPanel("Exposure sites",
                             sitesUI("sites"),
                             value = "sitestab"),
                    tabPanel("Movements",
                             casemovementUI("casemovement"),
                             value = "casemovementtab")
        )
      )
    )
  ),
  aboutUI("about"),
  tags$script(src = "collapsible.js")
)