# Displays a non-interactive timeline

timelineUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Infectious periods"),
    tabsetPanel(type = "pills",
                tabPanel("Static", plotOutput(ns("timeline"))),
                tabPanel("Interactive",
                         p(style="color:grey; margin: 12px 0 0 14px",
                           icon("info-circle"),
                           "Drag to zoom. Double-click anywhere in plot to reset."),
                         plotlyOutput(ns("timeline_interactive")))
    )
  )
}

zoomtimelineUI <- function(id, ...) {
  ns = NS(id)
  actionButton(ns("zoomtimeline"), ...)
}

#' Main timeline plot
#' 
#' Creates a ggplot of the linelist.
#' 
#' @param linelist Dataframe of the linelist
#' @param components Integer vector of graph components the same length as the
#' number of rows in `linelist`. Cases will be ordered by component
#' @param state Main MCT app state for selection/target
#' 
#' @return ggplot object
baseplot = function(linelist, components, state){
  components.df = tibble(
    ID = names(components()),
    component = components()
  )
  
  # Process linelist
  df = linelist() %>%
    filter(Classification == 'Confirmed') %>%
    left_join(components.df, by = "ID") %>%
    arrange(desc(component), desc(CalculatedOnsetDate)) %>%
    mutate(ID = as.factor(ID) %>% fct_inorder,
           color.line = ifelse(ID %in% state$ids, "black", color),
           width = case_when(ID == state$target.id ~ 1,
                             T ~ 0.5),
           size = case_when(ID == state$target.id ~ 5,
                            T ~ 3),
           label = long_desc)
  
  # Calculate date range of graph
  xmin = min(df$CalculatedOnsetDate - days(2))
  xmax = max(df$CalculatedOnsetDate + days(12))
  
  # Parameters for selection stripe
  df.hline = tibble(
    ID = state$ids[state$ids %in% df$ID],
    xmin = as_date(-1e5),
    xmax = as_date(1e5),
    label = NA
  )
  
  id.factors = df$ID[df$ID %in% state$ids]
  ggplot(df, aes(text = label, color=color)) +
    geom_segment(aes(x=CalculatedOnsetDate-days(2), y=ID, xend=CalculatedOnsetDate+days(12), yend=ID, size=width)) +
    geom_point(aes(x=CalculatedOnsetDate, y=ID, size=size)) +
    geom_segment(data=df.hline, aes(x=xmin, y=ID, xend=xmax, yend=ID), color="orange", size=4, alpha=0.2) +
    geom_text(aes(x=CalculatedOnsetDate-days(2), y=ID, label=CaseNumber), hjust=1, nudge_x=-1) +
    coord_cartesian(xlim = c(xmin - (xmax-xmin)*0.15 - 1,
                             xmax + (xmax-xmin)*0.05)) +
    scale_x_date(labels = date_format("%d %b")) +
    scale_color_identity() +
    scale_size_identity() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    labs(x = NULL, y = NULL)
}

#' Shiny server module for timeline graph
#' 
#' @param id Shiny module ID
#' @param linelist Reactive expression returning the current person node attributes
#' @param state List of main MCT app state
#' @param components Integer vector of the connected graph component for each row
#' in `linelist`
timelineServer <- function(id, linelist, state, components) {
  moduleServer(
    id,
    function(input, output, session) {
      # Basic ggplot
      output$timeline = renderPlot({
        baseplot(linelist, components, state)
      }, bg="transparent")
      
      # Interactive Plotly version
      output$timeline_interactive = renderPlotly({
        margin = list(l=0, r=0, t=0, padding=0)
        baseplot(linelist, components, state) %>%
          ggplotly(tooltip = "label") %>%
          layout(font = list(family = "Vic"), margin=margin) %>%
          config(displayModeBar = F) %>%
          style(hoverlabel = list(align="left", bgcolor = "white"))
      })
    }
  )
}

#' Modal dialog version of timeline module
#'
#' @param id Shiny module ID
#' @param linelist Reactive expression returning the current person node attributes
#' @param state List of main MCT app state
#' @param components Integer vector of the connected graph component for each row
#' in `linelist`
zoomtimelineServer <- function(id, linelist, state, components) {
  moduleServer(
    id,
    function(input, output, session) {
      # Trigger modal to appear
      observeEvent(input$zoomtimeline, {
        showModal(zoomtimelineModal())
      })
      # Modal layout
      zoomtimelineModal <- function() {
        ns = session$ns
        modalDialog(
          size = "l",
          easyClose = TRUE,
          div(style = "text-align: center",
              h3("Infectious periods")),
          div(
            style = "display: flex",
            div(
              style = "flex-grow: 100",
              tabsetPanel(type = "pills",
                          tabPanel("Static", plotOutput(ns("zoomtimeline"), height="600px")),
                          tabPanel("Interactive",
                                   div(style="height:6px"),
                                   plotlyOutput(ns("zoomtimeline_interactive"), height="600px"),
                                   p(style="color:grey; margin: 0 0 0 14px",
                                     icon("info-circle"),
                                     "Drag to zoom. Double-click anywhere in plot to reset."))
              )
            ),
            div(style = "width:150px; margin-top:48px; padding-left:4px",
                uiOutput(ns("legend"), class="leaflet-control-layers"))
          )
        )
      }
      
      # Basic ggplot
      output$zoomtimeline = renderPlot({
        baseplot(linelist, components, state)
      }, bg="transparent")
      
      # Interactive Plotly version
      output$zoomtimeline_interactive = renderPlotly({
        margin = list(l=0, r=0, t=0, padding=0)
        baseplot(linelist, components, state) %>%
          ggplotly(tooltip = "label") %>%
          layout(font = list(family = "Vic"), margin=margin) %>%
          config(displayModeBar = F) %>%
          style(hoverlabel = list(align="left", bgcolor = "white"))
      })
      
      # Separate legend for modal dialog only
      output$legend <- renderUI({
        make.legend(linelist()$color, linelist()[[state$colorattribute]], title = state$colorattribute)
      })
    }
  )
}
