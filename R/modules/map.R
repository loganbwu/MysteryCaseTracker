# Displays a map of relevant locations

# Constants

# Set coordinate pairs with these to NA (should b)
na.map.latitudes = c(-999, 0, -39.0, -39, -39.2, NA)

# Character constants to assign leaflet groups so HTML can be rendered in the layer menu
legend.groups = list(
  target = "<img class='leaflet-legend-icon' src='marker-icon-green.png'>Target events",
  people = "<img class='leaflet-legend-dot' src='circlemarker_grey_8.svg'>People",
  exposure = "<img class='leaflet-legend-icon' src='circlemarker_steelblue_8.svg'>Exposure sites",
  risk = "<img class='leaflet-legend-icon' src='circlemarker_orange_8.svg'>Case movements",
  test = "<img class='leaflet-legend-icon' src='plus-square.svg'>Testing sites",
  hotels = "<img class='leaflet-legend-icon' src='quarantine_hotel.svg'>Quarantine Hotels"
)

# Custom icons styled after default pin for use in leaflet map
colorIcons <- iconList(
  blue = makeIcon("marker-icon-blue.png",
                  "marker-icon-2x-blue.png",
                  25, 41,
                  12, 41,
                  shadowUrl = "marker-shadow.png",
                  shadowRetinaUrl = NULL,
                  41, 41,
                  12, 41,
                  1, -34),
  red = makeIcon("marker-icon-red.png",
                 "marker-icon-2x-red.png",
                 25, 41,
                 12, 41,
                 shadowUrl = "marker-shadow.png",
                 shadowRetinaUrl = NULL,
                 41, 41,
                 12, 41,
                 1, -34),
  green = makeIcon("marker-icon-green.png",
                   "marker-icon-2x-green.png",
                   25, 41,
                   12, 41,
                   shadowUrl = "marker-shadow.png",
                   shadowRetinaUrl = NULL,
                   41, 41,
                   12, 41,
                   1, -34),
  black = makeIcon("plus-square.svg",
                   NULL,
                   16, 16,
                   8, 8,
                   popupAnchorX=1, popupAnchorY=0),
  Qhotel = makeIcon("quarantine_hotel.svg",
                    NULL,
                    16, 16,
                    8, 8,
                    popupAnchorX=1, popupAnchorY=0)
)

#' Base leaflet
#' 
#' Construct the basic leaflet map before any dynamic content is specified.
#' 
#' @param testsites Optional sf object of testing sites
#' @param quarantinehotels Optional sf object of quarantine hotels
#' 
#' @return Leaflet map object ready for dynamic layers
base.leaflet <- function(testsites = NULL, quarantinehotels = NULL) {
  l = leaflet(
    options = leafletOptions(zoomControl = F)
  ) %>%
    # addProviderTiles("Esri.WorldGrayCanvas") %>%
    addProviderTiles(providers$CartoDB.Positron) %>% # this is better but not working in RStudio browser
    addLayersControl(
      overlayGroups = unname(unlist(legend.groups)),
      position = "topleft",
      options = list(collapsed=F)
    ) %>%
    addScaleBar(options = scaleBarOptions(imperial=F)) %>%
    addMapPane("baselayers", zIndex=409) %>%
    addMapPane("layers", zIndex=410) 
  if (!is.null(testsites)) {
    l = l %>%
      addMarkers(
        data = testsites,
        icon = colorIcons$black,
        label = ~short_desc,
        popup = ~long_desc,
        group = legend.groups$test,
        options = pathOptions(pane="baselayers")
      ) %>%
      hideGroup(legend.groups$test)
  }
  l
  if (!is.null(quarantinehotels)) {
    l = l %>%
      addMarkers(
        data = quarantinehotels,
        icon = colorIcons$Qhotel,
        label = ~short_desc,
        popup = ~long_desc,
        group = legend.groups$hotels,
        options = pathOptions(pane="baselayers")
      ) %>%
      hideGroup(legend.groups$hotels)
  }
}

#' Add dynamic leaflet layers
#' 
#' Uses `leafletProxy` so the base map and user view remain the same if changes
#' to the dynamic layers cause layers to be removed and re-added.
#' 
#' @param l Base leaflet map
#' @param casemovement_locations Casemovement locations preferably aggregated by distinct location
#' @param clustersites
#' @param linelist
#' @param state
#' @param target_row
#' @param withLegend
#' 
#' @return Completed Leaflet map with dynamic layers added
add.layers <- function(l, casemovement_locations, clustersites, linelist, state, target_row, withlegend=F) {
  # Create relevant spatial features
  
  riskhist.sf = casemovement_locations() %>%
    distinct(ID, .keep_all = T) %>%
    filter(Latitude %ni% na.map.latitudes) %>%
    mutate(MovementPlace = ifelse(MovementPlace == "", Address, MovementPlace)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs=4326)
  sites.sf = clustersites() %>%
    distinct(ID, .keep_all=T) %>%
    filter(Latitude %ni% na.map.latitudes) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs=4326)
  people.sf = linelist() %>%
    filter(Latitude %ni% na.map.latitudes) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) 
  selected.location.sf = bind_rows(
    # input could be either cluster site or person. This one shows up as a marker
    sites.sf %>% filter(ID %in% state$ids),
    people.sf %>% filter(ID %in% state$ids)
  )
  
  all.geometry = bind_rows(
    riskhist.sf %>% select(geometry),
    sites.sf %>% select(geometry),
    people.sf %>% select(geometry)
  )
  
  # Add layers to map
  if (nrow(riskhist.sf) > 0) {
    # Add risk history locations
    l = l %>%
      addCircleMarkers(
        data = riskhist.sf,
        label = ~short_desc,
        popup = ~long_desc,
        color = ~color,
        radius = 8,
        clusterOptions = markerClusterOptions(maxClusterRadius=10),
        layerId = ~ID,
        group = legend.groups$risk,
        options = pathOptions(pane="layers")
      )
  }
  if (nrow(sites.sf %>% filter(ID %ni% state$ids)) > 0) {
    # Add exposure sites except for selected ID
    l = l %>%
      addCircleMarkers(
        data = sites.sf %>% filter(ID %ni% state$ids),
        label = ~short_desc,
        popup = ~long_desc,
        color = ~color,
        radius = 12,
        layerId = ~ID,
        group = legend.groups$exposure,
        options = pathOptions(pane="layers")
      )
  }
  if (nrow(people.sf %>% filter(ID %ni% state$ids)) > 0) {
    # Add people except for selected ID
    l = l %>%
      addCircleMarkers(
        data = people.sf %>% filter(ID %ni% state$ids),
        label = ~short_desc,
        popup = ~long_desc,
        color = ~color,
        fillOpacity = 0.75,
        stroke = F,
        radius = 4,
        layerId = ~ID,
        group = legend.groups$people,
        options = pathOptions(pane="layers")
      )
  }
  if (nrow(selected.location.sf) > 0) {
    # Add selected case or site
    l = l %>% 
      addMarkers(
        data = selected.location.sf,
        icon = colorIcons$green,
        label = ~short_desc,
        popup = ~long_desc,
        layerId = ~ID,
        group = legend.groups$target,
        options = pathOptions(pane="layers")
      )
  }
  box = as.numeric(st_bbox(all.geometry))
  l %>%
    fitBounds(lng1 = box[1], lat1 = box[2], lng2 = box[3], lat2 = box[4],
              options = list(padding = c(32, 32))) %>%
    removeControl("dynamicLegend")
  if (withlegend) {
    l = l %>%
      addControl(
        make.legend(linelist()$color, linelist()[[state$colorattribute]], title = state$colorattribute),
        position = "topleft",
        layerId = "dynamicLegend",
        className = "leaflet-control-layers"
      )
  }
  l
}

# Module functions

mapUI <- function(id, ...) {
  ns <- NS(id)
  leafletOutput(ns("map"), ...)
}

zoommapUI <- function(id, ...) {
  ns = NS(id)
  actionButton(ns("zoommap"), ...)
}

#' Shiny map server module
#'
#' Displays a map that updates with reactive data
#' - Zooms in if target_row changes
#' - Returns the clicked marker ID to server.R
mapServer <- function(id,
                      casemovement_locations,
                      clustersites,
                      linelist,
                      testsites,
                      quarantinehotels,
                      state,
                      target_row) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Leaflet
      output$map <- renderLeaflet({
        # Create base Leaflet map
        base.leaflet(testsites, quarantinehotels)
      })
      
      observe({
        # Update leaflet by clearing data and adding relevant layers
        l = leafletProxy("map") %>%
          clearGroup(legend.groups$target) %>%
          clearGroup(legend.groups$people) %>%
          clearGroup(legend.groups$exposure) %>%
          clearGroup(legend.groups$risk) %>%
          add.layers(casemovement_locations, clustersites, linelist, state, target_row)
      })
      
      # Zoom to marker if another widget changes the app state
      observe({
        req(state$selecting.element != "map")
        lng = target_row()$Longitude
        lat = target_row()$Latitude
        if (length(lng) > 0) {
          if(is.na(lng) | is.na(lat)) {
            showNotification("Can't zoom to target on map because there are no coordinates available.",
                             type = "error")
          } else {
            leafletProxy("map") %>%
              flyTo(lng, lat, 12, options=list(duration=0.5))
          }
        }
      })
      
      # Capture map click and return to server
      map_click_id = eventReactive(input$map_marker_click, {
        input$map_marker_click$id
      })
      
      return(map_click_id)
    }
  )
}

#' Shiny map server module for modal dialog only
#' 
#' Similar to the main server module but in a modal dialog and without
#' interactivity.
zoommapServer <- function(id, casemovement_locations, clustersites, linelist, testsites, quarantinehotels, state, target_row) {
  moduleServer(
    id,
    function(input, output, session){
      
      # Trigger modal to appear
      observeEvent(input$zoommap, {
        showModal(zoommapModal())
      })
      
      # Modal layout
      zoommapModal <- function() {
        ns = session$ns
        modalDialog(
          size = "l",
          easyClose = TRUE,
          leafletOutput(ns("zoommap"), height="calc(100vh - 60px - 95px)")
        )}
      
      # Leaflet
      output$zoommap <- renderLeaflet({
        base.leaflet(testsites, quarantinehotels) %>%
          add.layers(casemovement_locations, clustersites, linelist, state, target_row, withlegend=T)
      })
    }
  )
}