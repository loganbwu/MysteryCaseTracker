'%ni%' <- Negate('%in%')

#' Minimum node distance
#' 
#' Minimum number of edges for each graph node to reach one of the named nodes
#' 
#' @param graph igraph object
#' @param nodes Vertex IDs to visit
#' @param mode passed on to `igraph::distances`
#' @param algorithm passed on to `igraph::distances`
#' 
#' @return Integer vector of minimum path lengths
min_node_distance = function (graph, nodes, mode = "all",  algorithm = "automatic") {
  
  target <- seq_len(gorder(graph))
  source <- rep(nodes, length.out = length(target))
  source_unique <- unique(source)
  dist <- distances(graph, v = source_unique, to = target, 
                    mode = mode, algorithm = algorithm)
  apply(dist, 2, min)
}

#' Create a navbarPage with a logo
#' 
#' Wrapper for the `shiny::navbarPage` layout that adds an extra logo.
#' 
#' @param ... Arguments passed to `shiny::navbarPage`
#' @param logo HTML element to add to navbar. Use of CSS styling recommended
navbarPageWithLogo <- function(..., logo) {
  navbar <- navbarPage(...)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], logo
  )
  navbar
}

#' Continuous color generator
#' 
#' Takes a coercible numeric vector, multiple colors, and returns colors along
#' the continuous spectrum
#' 
#' @param x Numeric vector
#' @param colors Character vector of colors to interpolate between along the
#' range of `x`
#' @param na.col Color to assign for NA values in `x`
#' 
#' @return Character value of colors for each value in `x`
pal = function (x, colors, na.col="grey30") {
  
  p = colorRamp(colors)
  x.rs = scales::rescale(as.numeric(x))
  cols.mat = p(x.rs)
  cols.is.na = which(is.na(cols.mat[,1]))
  cols.mat[cols.is.na,] = 0
  cols.rgb = rgb(cols.mat[,1], cols.mat[,2], cols.mat[,3], maxColorValue=255)
  cols.rgb[cols.is.na] = na.col
  cols.rgb
}

#' Multiple subgraph
#'
#' This wrapper for make_ego_graph allows more than one node to be specified
#' or nodes that aren't in the graph.
#' 
#' @param graph igraph object
#' @param nodes Vertex names that may or may not be in the initial graph
#' @param order Integer order of the subgraph to explore from `ids`
#' @param max_distance Maximum edge distance. NA distances are always included
#' 
#' @return An igraph object that is the subgraph from the specified vertices
#' 
#' @examples
#' get_subgraph(graph, nodes=c("24757042", "24625698", "24352601"), order=2)
get_subgraph = function(graph, nodes, order, max_distance=NA) {
  
  # remove edges that exceed a threshold
  if (!is.na(max_distance)) {
    edges_gt_distance = which(E(graph)$distance > max_distance)
    message("[functions.R] Removing ", length(edges_gt_distance), " edges with distance > ", max_distance)
  } else {
    edges_gt_distance = which(!is.na(E(graph)$distance))
    message("[functions.R] Removing edges with non-NA distance")
  }
  g = graph %>% delete_edges(edges_gt_distance)
  
  # Augment vertices with any IDs that aren't present (not in edgelist)
  nodes_ni_graph = nodes[nodes %ni% V(g)$name]
  g = g + nodes_ni_graph
  
  # Calculate subgraphs
  if (length(nodes) == 1) {
    g = make_ego_graph(g, order=order, nodes=nodes, mode="all")[[1]]
  } else {
    # Getting the subgraphs for multiple nodes is more expensive
    g_list = make_ego_graph(g, order=order, nodes=nodes, mode="all")
    keep_nodes = sapply(g_list, function(x) {V(x)$name}) %>% unlist %>% unique
    g = induced_subgraph(g, keep_nodes) # faster than union of ego graphs
  }
}

#' Add attributes to nodes from the data model
#' 
#' - Call functions to collect vertex attributes
#' - SQL queries are artificially limited to TOP 1000
#' - Node attributes are combined then inner joined to the vertex list to
#'   remove vertices that weren't found.
#' 
#' @param graph igraph object where vertex names are the keys in the data model
#' @param conn Database connection to SQL data model
#' 
#' @return igraph object with vertex attributes from data model
#' 
#' @examples 
#' conn = dbConnect(...)
#' graph = graph.data.frame(...)
#' g = get_containing_subgraph(graph, c("24757042", "24625698", "24352601"), order=2)
#' g2 = add_datamodel_attributes(g, conn)
add_datamodel_attributes = function(graph, conn, where) {
  node_ids = V(graph)$name # node IDs beginning with "-" are case movement locations
  message("[server.R] ", length(node_ids), " vertices")
  
  withProgress(
    message = paste("Running queries for up to", gorder(graph), "vertices"),
    {
      # Query linelist nodes
      incProgress(amount=0.2, detail="Linelist")
      linelist_attributes = query_linelist(conn,
                                           node_ids[substring(node_ids, 1, 1) != "-"],
                                           classification=where$Classification,
                                           dxdatemin=where$DiagnosisDateMin,
                                           dxdatemax=where$DiagnosisDateMax)
      message("[server.R] Query returned ", nrow(linelist_attributes), " rows for linelist")
      if (nrow(linelist_attributes) == 1000) {
        showNotification("Linelist query retrieved the maximum 1000 records", type="error", duration=30)
      }
      
      # Query exposure sit enodes
      incProgress(amount=0.2, detail="Cluster sites")
      exposuresite_attributes = query_exposuresites(conn, node_ids[substring(node_ids, 1, 1) != "-"], clustertype=where$ClusterType)
      message("[server.R] Query returned ", nrow(exposuresite_attributes), " rows for exposure sites")
      if (nrow(exposuresite_attributes) == 1000) {
        showNotification("Exposure site query retrieved the maximum 1000 records", type="error", duration=30)
      }
      
      # Retrieve case movements from in-memory table
      incProgress(amount=0.2, detail="Case movements")
      if (where$UseCaseMovements) {
        casemovement_location_attributes = get_casemovement_locations(casemovement, node_ids[substring(node_ids, 1, 1) == "-"])
        message("[server.R] Function returned ", nrow(casemovement_location_attributes), " rows for case movement locations")
      } else {
        casemovement_location_attributes = get_casemovement_locations(casemovement, NA)
      }
      
      # Aggregate and add as attributes to the graph
      incProgress(amount=0.2, detail="Processing attributes")
      all_attributes = bind_rows(linelist_attributes,
                                 exposuresite_attributes,
                                 casemovement_location_attributes) %>%
        mutate(name = as.character(coalesce(CoordID, CaseNumber)))
      shiny::validate(
        need(nrow(all_attributes) > 0, "No node information retrieved. Check settings to ensure the record's classification is enabled.")
      )
      dfs = igraph::as_data_frame(graph, "both") # get vertex and edge data frames
      dfs$vertices = dfs$vertices %>%
        inner_join(all_attributes, by="name") %>%
        distinct()
      dfs$edges = dfs$edges %>%
        filter(from %in% dfs$vertices$name,
               to %in% dfs$vertices$name)
      updated_graph = graph_from_data_frame(dfs$edges, FALSE, dfs$vertices)
    })
}

#' 10-year agegroup
#'
#' @param age Numeric vector of ages
#' 
#' @return Character vector of 10-year agegroups of form '{X0}-{X9}'
agegroup_10y = function(age) {
  age.tenth.floor = floor(age / 10)
  paste(age.tenth.floor*10, age.tenth.floor*10+9, sep="-")
}

#' Color nodes by a value with hardcoded rules
#' 
#' @param colorattribute Name of vertex attribute to color by
#' @param values Vertex attribute values
#' 
#' @return Character vector of colors
subgraph_coloring = function(colorattribute, values) {
  # Node coloring based on the user-selected variable
  # Return a color for each value depending on the colorattribute
  
  if(colorattribute == "DiagnosisDate") {
    colors = pal(values, c("pink","firebrick"))
  } else if (colorattribute == "Symptoms") {
    colors = case_when(values == "Yes" ~ "goldenrod",
                       values == "No" ~ "darkseagreen",
                       values == "NA" ~ "lightblue",
                       values == "Not stated" ~ "slategray",
                       values == "Unknown" ~ "sienna",
                       TRUE ~ "grey")
  } else if (colorattribute == "Classification") {
    colors = case_when(values == "Confirmed" ~ "firebrick",
                       values == "Contact - active" ~ "darkorange",
                       values == "Probable" ~ "purple",
                       values == "Not notifiable" ~ "lightseagreen",
                       values == "Not Case/Not Close Contact" ~ "lightblue",
                       values == "Rejected - contact > 14 days" ~ "slategray",
                       values == "Rejected - no testing" ~ "peachpuff",
                       values == "Rejected after testing" ~ "thistle",
                       TRUE ~ "grey")
  } else if (colorattribute %in% c("AgeGroup", "Occupation", "Language", "CountryOfBirth", "Address", "Suburb")) {
    unique.vals = unique(values)
    unique.vals = unique.vals[!is.na(unique.vals)]
    distinct.colors = distinctColorPalette(length(unique.vals))
    names(distinct.colors) = unique.vals
    colors = recode(values, !!!distinct.colors, .default="grey", .missing="grey")
  } else {
    colors = rep("grey", length(values))
  }
  colors
}

#' drop_na method for numeric class
drop_na.numeric = function(x) x[!is.na(x)]

#' drop_na method for character class
drop_na.character = function(x) x[!is.na(x)]

# SQL query functions ----

#' Query the linelist by CaseNumber
#' 
#' @param conn SQL database connection
#' @param casenumbers Vector of casenumbers in V_LineList or V_Person
#' @param classification Vector of allowed classifications, allowed to be zero-length for none
#' 
#' @return Dataframe of processed results from the Data Model
query_linelist = function(conn, casenumbers, classification, dxdatemin=NULL, dxdatemax=NULL) {
  if (!is.character(casenumbers) & !is.character(casenumbers)) stop("casenumbers must be character or numeric")
  if (length(classification) == 0) classification = ""
  classification = paste0("'", classification, "'")
  
  query = paste0("
  SELECT
  CAST(ll.CaseNumber AS INT) AS CaseNumber,
  SUBSTRING(pp.FirstName, 1, 2) + SUBSTRING(pp.Surname, 1, 2) AS Initials,
  ll.Sex,
  ll.AgeAtOnset,
  ll.Acquired,
  ll.Symptoms,
  ll.DiagnosisDate,
  ll.CalculatedOnsetDate,
  ll.Classification,
  ll.SymptomsOnsetDate,
  ll.Language,
  ll.CountryOfBirth,
  ll.Suburb,
  ll.Occupation,
  ll.RecordOwner,
  pp.Latitude,
  pp.Longitude,
  pp.FullCleanAddress AS Address,
  ac.AddressLongitude__c AS AddressLongitude,
  ac.AddressLatitude__c AS AddressLatitude
  FROM V_LineList AS ll
  LEFT JOIN V_Person AS pp ON ll.RecordID = pp.RecordID
  LEFT JOIN Account AS ac ON pp.AccountID = ac.Id
  WHERE ll.CaseNumber IN (",
  paste(casenumbers, collapse=", "),
  ") AND ll.Classification IN (",
  paste(classification, collapse=", "),
  ")")
  if (!is.null(dxdatemin)) {
    query = paste0(query, " AND ll.CalculatedOnsetDate >= '", dxdatemin, "'")
  }
  if (!is.null(dxdatemax)) {
    query = paste0(query, " AND ll.CalculatedOnsetDate <= '", dxdatemax, "'")
  }
  query = paste(query, "LIMIT 1000")
  result = dbGetQuery(conn, query) %>%
    mutate_if(is.character, function(x) iconv(x, to="Latin1")) %>%
    mutate(AgeGroup = agegroup_10y(AgeAtOnset),
           Type = "Person",
           short_desc = as.character(CaseNumber),
           long_desc = paste(Initials,
                             CaseNumber,
                             paste0(Sex, ", ", AgeAtOnset, "y"),
                             paste("Occ:", Occupation),
                             paste("Dx:", DiagnosisDate),
                             ifelse(is.na(SymptomsOnsetDate),
                                    paste("Sx:", Symptoms),
                                    paste0("Sx: ", Symptoms, ", ", SymptomsOnsetDate)),
                             paste("CoB:", CountryOfBirth),
                             paste("Lang:", Language),
                             Address,
                             paste("RecOwner:", RecordOwner),
                             sep = "<br>"))
}

#' Query exposure sites by ExposureCaseNumber
#' 
#' @param conn SQL database connection
#' @param exposurecasenumbers Vector of exposurecasenumbers in V_LineList or V_Person
#' @param clustertype Vector of allowed clustertypes, allowed to be zero-length for none
#' 
#' @return Dataframe of processed results from the Data Model
query_exposuresites = function(conn, exposurecasenumbers, clustertype) {
  if (!is.character(exposurecasenumbers) & !is.character(exposurecasenumbers)) stop("exposurecasenumbers must be character or numeric")
  if (length(clustertype) == 0) clustertype = ""
  clustertype = paste0("'", clustertype, "'")
  
  query = paste0("
  SELECT
  CAST(ExposureCaseNumber AS INT) AS CaseNumber,
  ClusterName,
  StreetAddress AS Address,
  Latitude,
  Longitude,
  SiteCategory1,
  SiteCategory2,
  ClusterType,
  DeclaredDate
  FROM V_ClusterSites
  WHERE ExposureCaseNumber IN (",
  paste(exposurecasenumbers, collapse=", "),
  ") AND ClusterType IN (",
  paste(clustertype, collapse=", "),
  ")")
  query = paste(query, "LIMIT 1000")
  result = dbGetQuery(conn, query) %>%
    mutate_if(is.character, function(x) iconv(x, to="Latin1")) %>%
    mutate(Type = "Exposure site",
           short_desc = str_remove(ClusterName, ".*20[0-9]{3,4} ?"),
           long_desc = paste(ClusterName %>% str_remove(".*20[0-9]{3,4} ?"),
                             CaseNumber,
                             ClusterType,
                             Address,
                             sep = "<br>"))
}

#' Get distinct locations by artificial Coordinate ID
#' 
#' CoordID is pre-generated by preprocessing so this is not a SQL query. Additionally
#' CoordIDs are unique for NA coordinates and are negative to avoid clashes with
#' CaseNumbers/ExposureCaseNumbers
#' 
#' @param casemovement Dataframe of case movements
#' @param ids Character vector of location IDs
#' 
#' @return Dataframe of processed results
get_casemovement_locations = function(casemovement, ids) {
  casemovement %>%
    filter(CoordID %in% ids) %>%
    mutate_if(is.character, stri_enc_toutf8) %>%
    group_by(CoordID) %>%
    summarise(Address = Address %>% unique %>% drop_na %>% paste(collapse=", ") %>% str_trunc(64),
              MovementPlace = MovementPlace %>% unique %>% drop_na %>% paste(collapse=", ") %>% str_trunc(64),
              AddressType = AddressType %>% unique %>% drop_na %>% paste(collapse=", ") %>% str_trunc(64),
              Longitude = Longitude %>% drop_na %>% first,
              Latitude = Latitude %>% drop_na %>% first,
              .groups = "keep") %>%
    mutate_if(is.character, function (x) na_if(stri_enc_toutf8(x), "")) %>%
    mutate(Type = "Case movement",
           short_desc = MovementPlace,
           long_desc = paste(MovementPlace,
                             Address,
                             AddressType %>% fct_explicit_na("(No address type)"),
                             sep = "<br>"))
}
