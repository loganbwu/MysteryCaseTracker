# Overrides the RStudio Connect pin data used by `preprocessing.rmd` and `global.R`.
if("pins" %in% (.packages())){
  stop("Package `pins` is attached. Only run `dummy_pins.R` for testing without `pins` as it replaces the COVID-19 data model with dummy data.")
}


library(sqldf)
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(sf)
# source("dummy_data.R")
# source("dummy_sql.R")

# BEGIN preprocessing.rmd replacement ----
all_classifications = c("Confirmed", "Probable")
rejected_classifications = all_classifications %>%
  str_subset("eject")
linelist <- dbGetQuery(conn, paste0("SELECT
                       CAST(ll.CaseNumber AS INT) AS CaseNumber,
                       ll.Classification,
                       pp.Latitude,
                       pp.Longitude,
                       ac.AddressLongitude__c AS AddressLongitude,
                       ac.AddressLatitude__c AS AddressLatitude
                       FROM V_LineList AS ll
                       LEFT JOIN V_Person as pp
                         ON ll.RecordID = pp.RecordID
                       LEFT JOIN Account as ac
                         ON pp.AccountID = ac.Id
                       WHERE ll.Classification NOT IN (", paste0("'", rejected_classifications, "'", collapse=", "), ")"))
linelist = linelist %>%
  mutate(
    # Set known default values to NA
    Longitude = ifelse(Longitude==-999 & Latitude==-999 |
                         Longitude==145 & Latitude%in%c(-39,-39.2), NA, Longitude),
    Latitude = ifelse(is.na(Longitude), NA, Latitude),
    AddressLongitude = ifelse(AddressLongitude==-999 & AddressLatitude==-999 |
                                AddressLongitude==145 & AddressLatitude%in%c(-39,-39.2), NA, AddressLongitude),
    AddressLatitude = ifelse(is.na(AddressLongitude), NA, AddressLatitude),
    
    # Coalesce coordinates and remove Account fields
    Longitude = coalesce(AddressLongitude, Longitude),
    Latitude = coalesce(AddressLatitude, Latitude),
    AddressLongitude = NULL,
    AddressLatitude = NULL
  )

clustersites <- dbGetQuery(conn, 
                           "SELECT
                           CAST(ExposureCaseNumber AS INT) AS ExposureCaseNumber,
                           DeclaredDate,
                           Latitude,
                           Longitude
                           FROM V_ClusterSites")

casemovement = dbGetQuery(conn,
                          "SELECT
                          CAST(ll.CaseNumber AS INT) AS CaseNumber,
                          cm.MovementDate,
                          cm.MovementTime,
                          cm.MovementPlace,
                          cm.FullCleanAddress AS 'Address',
                          COALESCE(lo.AddressLongitude__c, cm.Longitude) AS Longitude,
                          COALESCE(lo.AddressLatitude__c, cm.Latitude) AS Latitude,
                          cm.AddressType
                          FROM V_casemovement AS cm
                          INNER JOIN V_LineList AS ll
                            ON cm.RecordID = ll.RecordID
                          LEFT JOIN ContactEncounter AS ce
                            ON cm.Id = ce.Id
                          LEFT JOIN Location AS lo
                            ON ce.Location__c = lo.Id") %>%
  mutate_if(is.character, function(x) iconv(x, to="Latin1")) %>%
  # Set known default coordinates to NA
  mutate(Longitude = ifelse(Longitude==145 & Latitude%in%c(-39,-39.2), NA, Longitude),
         Latitude = ifelse(is.na(Longitude), NA, Latitude)) %>%
  # create unique casemovement location id from lat and lon:
  arrange(Latitude) %>%
  group_by(Longitude, Latitude) %>%
  mutate(CoordID = -cur_group_id()) %>%
  ungroup() %>%
  # remove the CoordID for invalid coordinates
  mutate(CoordID = ifelse(is.na(Longitude), NA, CoordID),
         Address = Address %>% str_to_title %>% str_replace("Vic(?![a-z])", "VIC"))


# Calculate proximity links between locations
locations = bind_rows(
  clustersites %>% mutate(id=ExposureCaseNumber, Type="Exposure site"),
  casemovement %>% distinct(CoordID, .keep_all=T) %>% mutate(id=CoordID, Type="Risk history")
) %>%
  select(id, Longitude, Latitude) %>%
  drop_na(Longitude, Latitude)

# location date ranges
cooldown = 30 # a visit is active for 30 days
clustersites.daterange = clustersites %>%
  select(id = ExposureCaseNumber, start = DeclaredDate) %>%
  mutate(end = start + days(cooldown))
casemovement.daterange = casemovement %>%
  select(id = CoordID, start = MovementDate) %>%
  mutate(end = start + days(cooldown)) %>%
  drop_na(id) %>%
  group_by(id) %>%
  summarise(start = min(start), end = max(end))
locations.daterange = bind_rows(clustersites.daterange, casemovement.daterange)

# Add links with max distance
locations_sf = st_as_sf(locations, coords=c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(28355) %>% # projected coordinate system in metres for Victoria
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()
max_distance_loc = 1000
location_proximity_edges = dbGetQuery(
  NULL,
  paste0("SELECT i.id AS orig,
  j.id AS dest,
  i.X - j.X AS dX,
  i.Y - j.Y AS dY
  FROM locations_sf AS i
  JOIN locations_sf AS j
  WHERE i.id <> j.id AND
  ABS(i.X - j.X) < ", max_distance_loc, " AND ABS(i.Y - j.Y) < ", max_distance_loc)) %>%
  mutate(from = orig,
         to = dest,
         distance = sqrt((dX - dY)^2),
         .keep = "none") %>%
  filter(distance < max_distance_loc) %>%
  left_join(locations.daterange, by=c("from"="id"), relationship="many-to-many") %>%
  left_join(locations.daterange, by=c("to"="id"), relationship="many-to-many") %>%
  filter(start.x <= end.y & start.y <= end.x) %>% # req ranges to overlap
  mutate(from = as.integer(from),
         to = as.integer(to)) %>%
  select(from, to, distance)

# Proximity links between people
max_distance_person = 50
linelist_sf = st_as_sf(linelist, coords=c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(28355) %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()
  
person_proximity_edges = dbGetQuery(
  NULL,
  paste0("SELECT i.CaseNumber AS orig,
  j.CaseNumber AS dest,
  i.X - j.X AS dX,
  i.Y - j.Y AS dY
  FROM linelist_sf AS i
  JOIN linelist_sf AS j
  WHERE i.CaseNumber <> j.CaseNumber AND
  ABS(i.X - j.X) < ", max_distance_person, " AND ABS(i.Y - j.Y) < ", max_distance_person)) %>%
  mutate(from = orig,
         to = dest,
         distance = sqrt((dX - dY)^2),
         .keep = "none") %>%
  filter(distance < max_distance_person) %>%
  filter(from %in% linelist$CaseNumber[linelist$Classification=="Confirmed"] |
           to %in% linelist$CaseNumber[linelist$Classification=="Confirmed"])
proximity_edges = bind_rows(location_proximity_edges, person_proximity_edges)

# This is the data that would be pinned to RStudio Connect
data = list(casemovement = casemovement,
            proximity_edges = proximity_edges,
            all_classifications = all_classifications)

# END preprocessing.rmd replacement ----

#' Allow global.R to retrieve data as if it were pinned by `preprocessing.R`
pin_get = function(name, board=NULL) {
  if (str_detect(name, "MysteryCaseTracker-Data")) {
    return(data)
  }
  
  if (str_detect(name, "TestingSites_1")) {
    return(TestingSites_1)
  }
}

pin_versions = function(name, board=NULL) {
  tibble(created = as.character(now()))
}
