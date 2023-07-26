#' Server logic for MCT app
#' 
#' The MCT is initialised in `global.R` with:
#' - An igraph object of connections between people (linelist), exposure
#' sites, and case movements
#' - A database connection to `phess-dw01` for linelist and exposure site attributes
#' - A data pin of case movement attributes (much smaller and easier to store)
#' 
#' When the user selects a CaseNumber (or multiple), the MCT gets the neighborhood
#' subgraph from the in-memory igraph. This subgraph can contain invalid nodes
#' according to the current filtering settings (a superset of nodes).
#' 
#' The MCT runs parameterised SQL queries on `phess-dw01` and functions to
#' retrieve node attributes. `WHERE` clauses implement some filtering settings.
#' If node attributes are not found, it is removed from the subgraph. The
#' neighborhood is then refined (recalculated) with node attributes.
#' 
#' The subgraph object and derived data are passed to module functions for
#' visualisation. Other objects storing the app's state/settings are also passed.
shinyServer(function(input, output, session) {
  
  # MODALS ====
  # Settings
  # Add named settings here and in settings.R
  settings <- reactiveValues(
    # Specify default values here
    usecasemovements = T,
    limitids = T,
    linelistclassifications = c("Confirmed", "Probable"),
    exposuresitetypes = c("Outbreak", "Exposure site", "Complex case")
  )
  new_settings <- settingsServer("settings", settings, all_classifications)
  observeEvent(new_settings(), {
    new_s = new_settings()
    for (s in names(settings)) {
      if (!identical(settings[[s]], new_s[[s]])) {
        message("[server.R] Updating setting ", s, " to ", paste(new_s[[s]], collapse=", "))
        settings[[s]] = new_s[[s]]
      }
    }
  })
  
  # Mystery cases modal
  mystery_id <- mysteryServer("mystery", conn)
  observe(
    updateTextInput(session, "ids", value=mystery_id())
  )
  
  
  # STATE ====
  # The state of the app is stored here because multiple elements can modify the state.
  # 'target' is the item being clicked, highlighted, or zoomed to.
  # Use 'selecting.element' to prevent elements from updating themselves after modifying the state.
  state <- reactiveValues(
    ids = NA,
    target.id = NA,
    selecting.element = NA,
    colorattribute = NA
  )
  
  ids_throttle = reactive(input$ids) %>% throttle(1000)
  
  # Modifying the ID box changes the id in state
  observe({
    # Process text to allow for multiple IDs
    state$ids = ids_throttle() %>%
      str_replace_all("[^[0-9]]+", ",") %>%
      str_remove_all("^,|,$") %>%
      str_split(",") %>%
      unlist
    message("[server.R] Set state$ids to: ", paste(isolate(state$ids), collapse=", "))
    
    # Provide warning if user specifies too many
    id.limit = 10
    if (settings$limitids & length(isolate(state$ids)) > id.limit) {
      showNotification(paste("Only using the first", id.limit, "PHESS IDs. You can bypass this in Settings."),
                       duration = 50,
                       type = "error")
      state$ids = head(isolate(state$ids), id.limit)
    }
    state$target.id = NA
    state$selecting.element = NA
  })
  
  # Coloring on an attribute changes the state
  observeEvent(input$colorattribute, {
    state$colorattribute = if (input$colorattribute == "No attribute") NA else input$colorattribute
  })
  
  # Add debugging info to console on state change
  observe({
    l = unlist(reactiveValuesToList(state))
    x = paste(paste0(names(l), ": ", l), collapse="\n")
    message("[server.R] ", x)
  })
  
  
  # REACTIVE DATA ====
  
  # Generate proto subgraph by calling processing functions
  # This is separate from the colors so that changing the color attribute does
  # not invalidate and trigger expensive neighborhood and attribute functions
  subgraph.proto = reactive({
    req(state$ids, input$order, input$daterange)
    message("[server.R] Finding subgraph centered on ", state$ids, " with order ", input$order)
    
    distance.thres = debounce(reactive(input$distancethresholdtext), 1000)() %>%
      str_extract("[0-9]+") %>%
      as.numeric
    daterange = debounce(reactive(input$daterange), 1000)()
    
    # filtering out casemovements early reduces query sizes for other nodes
    if (settings$usecasemovements) {
      containing_subgraph = get_subgraph(graph_with_casemovements, state$ids, input$order, distance.thres)
    } else {
      containing_subgraph = get_subgraph(graph_without_casemovements, state$ids, input$order, distance.thres)
    }
    
    subgraph_with_attributes = add_datamodel_attributes(
      containing_subgraph,
      conn,
      where=list(Classification=settings$linelistclassifications,
                 DiagnosisDateMin=daterange[[1]],
                 DiagnosisDateMax=daterange[[2]],
                 ClusterType=settings$exposuresitetypes,
                 UseCaseMovements=settings$usecasemovements)
    )
    
    # Not all the requested nodes will be available depending on query filters,
    # therefore the neighborhood algorithm has to be re-run.
    ids_in_graph = state$ids[state$ids %in% V(subgraph_with_attributes)$name]
    shiny::validate(need(length(ids_in_graph)>0,
                         paste("IDs not found in data model:",
                               state$ids,
                               "Consider adjusting the classification settings.",
                               collapse=", ")))
    message(paste("[server.R] Refining subgraph with", gorder(subgraph_with_attributes), "vertices from queries"))
    refined_subgraph = get_subgraph(subgraph_with_attributes, ids_in_graph, input$order, distance.thres)
  })
  
  # subgraph.proto but with color attributes added. This invalidates if the
  # selected colorattribute is changed
  subgraph = reactive({
    g = subgraph.proto()
    df = igraph::as_data_frame(g, what="vertices")
    # Use a variable color scheme depending on the selected coloring attribute
    vertex_attr(g, "color", vertex_attr(g, "Type") == "Person") = subgraph_coloring(
      input$colorattribute,
      df[df[["Type"]] == "Person", input$colorattribute]
    )
    # Location node colors are hardcoded
    vertex_attr(g, "color", vertex_attr(g, "Type") == "Exposure site") = "steelblue"
    vertex_attr(g, "color", vertex_attr(g, "Type") == "Case movement") = "orange"
    g
  })
  
  # Useful dataframe version of subgraph data attributes
  subgraph.df = reactive({
    subgraph() %>%
      igraph::as_data_frame("vertices") %>%
      # mutate_if(is.character, stri_enc_toutf8) %>%
      rename(ID = name) %>%
      mutate_at(vars(CalculatedOnsetDate, DiagnosisDate, SymptomsOnsetDate), as_date)
  })
  
  # Calculated connected component
  # Used to order clusters in  timeline. Maybe could be a vertex attribute
  # Returns an integer vector length of the graph order. Vertices with the same
  # value are in the same component
  subgraph.components <- reactive({
    req(subgraph, state$ids)
    if (length(state$ids) > 1) {
      components(subgraph(), mode="weak")$membership
    } else {
      c("NULL" = 0) # with one ID all nodes must be in the same component
    }
  })
  
  # Target person profile
  this.case <- reactive({
    req(state$ids)
    subgraph.df() %>%
      filter(ID %in% state$ids)
  })
  
  # Target is the person/site/history that has been selected as the 'state'
  target.row <- reactive({
    req(state$target.id)
    req(state$selecting.element != "casemovement_table") # can't be triggered by the casemovement table
    
    subgraph.df() %>%
      filter(ID == state$target.id)
  })
  
  # Data frames for map and tables ====
  casemovement.df <- reactive({
    # Derived from original case movements with duplicates
    req(subgraph())
    cm = subgraph() %>%
      igraph::as_data_frame() %>%
      mutate_all(as.integer) %>%
      select(from, to) %>%
      inner_join(
        casemovement,
        by = c("from"="CaseNumber", "to"="CoordID")
      ) %>%
      rename(CaseNumber=from, CoordID=to)
  })
  
  casemovement_locations.df <- reactive({
    req(subgraph.df())
    subgraph.df() %>%
      filter(Type == "Case movement")
  })
  
  clustersites.df <- reactive({
    subgraph.df() %>%
      filter(Type == "Exposure site")
  })
  
  linelist.df <- reactive({
    subgraph.df() %>%
      filter(Type == "Person")
  })
  
  # VISUALISATIONS ====
  # Current ID's profile
  profileServer("thiscase", this.case)
  
  # Timeline
  timelineServer("timeline", linelist.df, state, subgraph.components)
  
  #zoom Timeline Modal
  zoomtimelineServer("zoomtimeline", linelist.df, state, subgraph.components)
  
  # Network
  node_click_id <- networkServer("network", subgraph, state)
  # Receive node clcks and update app state
  observeEvent(node_click_id(), {
    state$target.id = node_click_id()
    state$selecting.element = "network"
  })
  observe({
    req(state$selecting.element != "network")
    message(paste("[server.R] Focusing network to", state$target.id))
    visNetworkProxy("network-network") %>%
      visFocus(id = state$target.id,
               scale = 0.5,
               animation = list(duration = 250, easingFunction = "easeInOutQuad"),
               locked = F)
  })
  # Network Zoom modal
  zoomnetworkServer("zoomnetwork", subgraph, state)
  
  
  # Leaflet map
  map_click_id <- mapServer("map", casemovement_locations.df, clustersites.df, linelist.df, testsites, quarantinehotels, state, target.row)
  # Receive map clicks and update app state
  observeEvent(map_click_id(), {
    state$target.id = map_click_id()
    state$selecting.element = "map"
  })
  # Map Zoom modal
  zoommapServer("zoommap", casemovement_locations.df, clustersites.df, linelist.df, testsites, quarantinehotels, state, target.row)
  
  # Tables ====
  # Linelist
  linelistServer("linelist", linelist.df, state)
  
  # Exposure sites
  sitesServer("sites", clustersites.df, state)
  
  # Case movements
  casemovementServer("casemovement", casemovement.df, state)
  
  # Debug ====
  debugServer("debug")
})
