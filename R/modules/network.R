# Displays a network diagram

networkUI <- function(id) {
  ns <- NS(id)
  tagList(
    visNetworkOutput(ns("network"), height="500px"),
    div(
      style = "position: absolute; top: 10px",
      uiOutput(ns("legendcontents"))
    )
  )
}

zoomnetworkUI <- function(id, ...) {
  ns = NS(id)
  actionButton(ns("zoomntwk"), ...)
}

#' Create a visNetwork from a graph
#' 
#' Creates the linelist/clustersite/case movement network visualisation with
#' data provided as vertex and edge attributes. Note this should be used with
#' the custom MysteryCaseTracker `visNetwork.js`.
#' 
#' @param session R Shiny app session
#' @param subgraph igraph object
#' @param highlight_ids Add border to nodes with ID
#' 
#' @return visNetwork object
make.visnetwork <- function(session, subgraph, highlight_ids) {
  # print(tidygraph::as_tbl_graph(subgraph()))
  data = toVisNetworkData(subgraph(), idToLabel=F)
  if ("distance" %ni% names(data$edges)) {
    data$edges = data$edges %>%
      tibble::add_column(distance = NA_real_)
  }
  message("[network.R] Rendering visnetwork with ", nrow(data$nodes), " nodes")
  
  visNetwork(nodes = data$nodes %>%
               mutate(label = short_desc, # these are precomputed at data import
                      title = long_desc,
                      shape = case_when(Type == "Exposure site" ~ "square",
                                        Type == "Case movement" ~ "triangle",
                                        TRUE ~ "dot"),
                      borderWidth = 3,
                      color.background = color,
                      color.border = ifelse(id %in% highlight_ids, "black", color),
                      color.highlight = color,
                      color = NULL,
                      value = ifelse(Type == "Person", 6, 8)),
             edges = data$edges %>%
               mutate(dashes = !is.na(distance),
                      distance = round(distance),
                      title = as.character(ifelse(is.na(distance), NA, paste(distance, "m"))))) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 0) %>%
  visOptions(highlightNearest = list(enabled = F, degree = 2, hover = T)) %>%
  visEvents(selectNode = paste0("function(nodes) { Shiny.onInputChange('",
                                session$ns("current_node_id"),
                                "', nodes.nodes); }")) %>%
  visEdges(color = 'black') %>%
  visNodes(font = list(size=21,
                       background="white",
                       face="Vic, 'Helvetica Neue', Helvetica, Arial, sans-serif")) %>%
  visInteraction(tooltipDelay = 0,
                 tooltipStay = 0#,
                 # tooltipStyle now in styles.css used by modified visNetwork.js
  )
}

#' Return a discrete HTML legend
#' 
#' Returns a legend containing a table of color/value pairs.
#' 
#' @param colors Character vector of color values
#' @param values Vector of values the same length as `colors`
#' 
#' @return Discrete HTML legend
discrete.legend <- function(colors, values, title=NULL) {
  message("[network.R] Generating discrete legend")
  
  df = tibble(col=colors, val=values) %>%
    mutate(val = val %>%
             as.character %>%
             fct_infreq %>%
             fct_lump_n(6, other_level="(More)", ties.method="first") %>%
             fct_explicit_na(),
           col = if_else(val=="(More)", "white", col)) %>%
    count(val, col) %>%
    mutate(text = paste0(val, " (", n, ")"))
  
  if (nrow(df) == 0) {
    leg = tags$div(
      class = "visnetwork-legend",
      "No values"
    )
  } else {
    items = setNames(df$col, df$text)
    contents = sapply(names(items), function (n) {
      tags$tr(
        tags$td(
          tags$span(style = paste0("background:",items[[n]]))
        ),
        tags$td(n)
      )
    },
    simplify = F,
    USE.NAMES = T)
    leg = tags$div(
      class = "visnetwork-legend",
      if (!is.null(title)) p(class="control-label", title) else NULL,
      tags$table(contents)
    )
  }
  leg    
}

#' Return a continuous HTML legend
#' 
#' Returns an interpolated legend with the min and max colors/values marked.
#' Ignores intermediate color specifications because it uses `linear-gradient`
#' between the min and max; i.e., other gradient types or intermediate colors
#' will not affect the shown gradient even if present in `colors`. Exception is
#' if only one unique value is present in `values`; function will fall back to a
#' discrete legend instead.
#' 
#' @param colors Character vector of color values
#' @param values Vector of values the same length as `colors`
#' 
#' @return Continuous HTML legend ranging from `min(values)` to `max(values)`
#' and the corresponding `colors` at the same indices, or a discrete legend if
#' only one unique value is present
continuous.legend <- function(colors, values, title=NULL) {
  if (all(is.na(values))) {
    leg = tags$div(
      class = "visnetwork-legend",
      "No values"
    )
  } else {
    min.ix = which.min(values)
    max.ix = which.max(values)
    if (min.ix == max.ix) {
      # fall back to discrete legend if there's only one value
      leg = discrete.legend(colors, values)
    } else {
      message("[network.R] Generating continuous legend")
      leg = tags$div(
        class = "visnetwork-legend",
        if (!is.null(title)) p(class="control-label", title) else NULL,
        tags$span(
          style = paste0("float: left; background: linear-gradient(", colors[max.ix], " 0%, ", colors[min.ix], " 100%", "); height: 60px;")
        ),
        tags$div(
          style = "float: left",
          tags$div(
            style = "display: block; top: 0; line-height: 1rem;",
            values[max.ix]
          ),
          tags$div(
            style = "display: block; position: relative; top: 40px; line-height: 1rem; margin-bottom: 4px;",
            values[min.ix]
          )
        )
      )
    }
  }
  leg
}

#' Generate a legend with automatic type
#' 
#' Returns a discrete legend if `values` is discrete (charactor or factor) and
#' a continuous legend if `values` is numeric.
#' 
#' @param colors Character vector of color values
#' @param values Vector of values the same length as `colors`
#' 
#' @return HTML legend with type depending on `values`
make.legend <- function(colors, values, title=NULL) {
  
  leg = NULL
  # general cases
  if (is.character(values) | is.factor(values)) {
    leg = discrete.legend(colors, values, title)
  } else if (is.numeric(values) | is.Date(values)) {
    leg = continuous.legend(colors, values, title)
  }
  leg
}


# Module functions

networkServer <- function(id, subgraph, state) {
  # Displays a network diagram of vertices in the subgraph
  # - uses the state for some formatting
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # visNetwork
      output$network <- renderVisNetwork({
        make.visnetwork(session, subgraph, state$ids)
      })
      
      # legend
      output$legendcontents <- renderUI({
        person.ix = which(vertex_attr(subgraph(), "Type") == "Person")
        if (state$colorattribute == "DiagnosisDate") {
          make.legend(vertex_attr(subgraph(), "color", person.ix),
                      # vertex attribute stores date as double
                      as_date(vertex_attr(subgraph(), state$colorattribute, person.ix)),
                      title = state$colorattribute)
        } else {
          make.legend(vertex_attr(subgraph(), "color", person.ix),
                      vertex_attr(subgraph(), state$colorattribute, person.ix))
        }
      })
      
      # Capture map node and return to server
      node_click_id = eventReactive(input$current_node_id, {
        input$current_node_id
      })
      
      return(node_click_id)
    }
  )
}

zoomnetworkServer <- function(id, subgraph, state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Trigger modal to appear
      observeEvent(input$zoomntwk, {
        showModal(zoomnetworkModal())
      })
      
      # Modal layout
      zoomnetworkModal <- function() {
        ns = session$ns
        modalDialog(
          size = "l",
          easyClose = TRUE,
          visNetworkOutput(ns("networkplot"), height="calc(100vh - 60px - 95px)"),
          div(
            style = "position: absolute; top: 15px",
            uiOutput(ns("legendcontents"))
          )
        )
      }
      
      output$networkplot <- renderVisNetwork(
        make.visnetwork(session, subgraph, state$ids)
      )
      
      output$legendcontents <- renderUI({
        person.ix = which(vertex_attr(subgraph(), "Type") == "Person")
        if (state$colorattribute == "DiagnosisDate") {
          make.legend(vertex_attr(subgraph(), "color", person.ix),
                      # vertex attribute stores date as double
                      as_date(vertex_attr(subgraph(), state$colorattribute, person.ix)),
                      title = state$colorattribute)
        } else {
          make.legend(vertex_attr(subgraph(), "color", person.ix),
                      vertex_attr(subgraph(), state$colorattribute, person.ix),
                      title = state$colorattribute)
        }
      })
    }
  )
}