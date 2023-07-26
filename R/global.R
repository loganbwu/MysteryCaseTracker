# if (Sys.getenv("RSCONNECT_API_KEY") == "") {
#   stop("System variable 'RSCONNECT_API_KEY' missing. On RStudio Connect, go to Settings>Vars. If running locally, either add an .Renviron file or run Sys.setenv(RSCONNECT_API_KEY = ...).")
# }

library(igraph)
library(visNetwork)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(plotly)
library(leaflet)
library(forcats)
library(scales)
library(stringr)
library(stringi)
library(DT)
# library(pins) # Commented packages not used for demo
# library(bit64)
library(shinyWidgets)
library(randomcoloR)
# library(odbc)
# library(pool)

# Spoof the app with static random data
source("dummy_data.R")
source("dummy_sql.R")
source("dummy_pins.R")

app_initialised = now() %>% with_tz("Australia/Melbourne")

source("functions.R")
source("modules/settings.R")
source("modules/mystery.R")
source("modules/about.R")
source("modules/profile.R")
source("modules/timeline.R")
source("modules/network.R")
source("modules/map.R")
source("modules/debug.R")
source("modules/table_linelist.R")
source("modules/table_sites.R")
source("modules/table_casemovement.R")

# Open a database connection pool. This is shared between all users in a thread 
# conn <- dbConnect(
#   odbc(),
#   Driver = "ODBC Driver 17 for SQL Server",
#   Server = "p-ause-ps-sql-s-hpb-rpt-01.database.windows.net",
#   uid = Sys.getenv("PHESS_DW01_UID"),
#   pwd = Sys.getenv("PHESS_DW01_PWD"),
#   Database = "phess-dw01",
#   encoding = "latin1",
#   applicationintent = "readonly"
# )
conn = NA

# Connect to RStudio Connect to retrieve precalculated 'pins'
# board_register_rsconnect(key = Sys.getenv("RSCONNECT_API_KEY", "Warning: Add as env var on RSConnect"),
#                          server = "https://rstudio.covid19.dhhs.vic.gov.au")

message("[global.R] Querying case links...")
caselinks <- dbGetQuery(conn,
                        "SELECT DISTINCT
                        CAST(cl.SourceCaseNumber AS INT) AS SourceCaseNumber,
                        CAST(cl.TargetCaseNumber AS INT) AS TargetCaseNumber
                        FROM V_CaseLinks AS cl")

# Load preprocessed data pin
message("[global.R] Downloading case movements")
pinned_data = pin_get("logan.wu@health.vic.gov.au/MysteryCaseTracker-Data", board="rsconnect")
casemovement = pinned_data$casemovement %>%
  # NA CoordIDs are assigned for locations without coordinates
  # -ve numbers avoid clashes with (Exposure)CaseNumbers
  # Add a unique temporary CoordID
  mutate(CoordID = ifelse(is.na(CoordID),
                          min(CoordID, na.rm=T) - row_number(),
                          CoordID))
proximity_edges = pinned_data$proximity_edges
all_classifications = pinned_data$all_classifications
last_updated = pin_versions("logan.wu@health.vic.gov.au/MysteryCaseTracker-Data", board = "rsconnect")$created[1] %>%
  as_datetime %>% with_tz("Australia/Melbourne")
last_updated = now()

message("[global.R] Downloading testing sites")
# Retrieve Pin
testsites <- pin_get("logan.wu@health.vic.gov.au/TestingSites_1", board = "rsconnect")

# Process testing sites
testsites = testsites %>%
  mutate(short_desc = sitename,
         long_desc = paste(sitename, service, address, sep="<br>")) %>%
  #can't find 'status' column in old .csv file (JMc: 21/Oct/2021)
  #long_desc = paste(sitename, service, address, status, sep="<br>")) %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
  select(short_desc, long_desc)

# Process Quarantine Hotels
quarantinehotels = read.csv("data/Quarantine_Hotel_list.csv") %>%
  mutate_if(is.character, str_squish) %>%
  mutate(short_desc = Hotel,
         long_desc = paste(Hotel, Address, paste(City, Postcode), sep="<br>")) %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
  select(short_desc, long_desc)

message("[global.R] Generating graph...")
# Create an igraph object:
graph_with_casemovements = bind_rows(caselinks %>% rename(from=SourceCaseNumber, to=TargetCaseNumber),
                                     casemovement %>% select(from=CaseNumber, to=CoordID) %>% distinct,
                                     proximity_edges) %>%
  graph.data.frame
graph_without_casemovements = bind_rows(caselinks %>% rename(from=SourceCaseNumber, to=TargetCaseNumber),
                                        proximity_edges) %>%
  graph.data.frame
message("[global.R] Graph generated.")

