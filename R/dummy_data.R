
# Generate tables of dummy data to form a local static database for testing instead of the real COVID-19 data model

library(tibble)
library(dplyr)
library(lubridate)

# Parameters for randomly generated coordinates
# mel_cent = c(144.946457, -37.840935)
mel_cent = c(144.946457, -36.840935) # displace otherwise random people end up in the ocean
mel_sd = c(0.5, 0.5)

n_people = 100
n_houses = n_people / 4
n_clustersites = n_people / 5
n_caselinks = (n_people + n_clustersites) * 1.1
ratio_clusterlinks_to_personlinks = 0.3
n_testsites = 10
n_casemovement = 3 * n_people
n_casemovement_locations = n_people * 2



houses = tibble(
  house_ix = seq_len(n_houses),
  FullCleanAddress = paste(sample(1:999, n_houses), "Street, 3XXX"),
  Longitude = rnorm(n_houses, mel_cent[1], sd=mel_sd[1]),
  Latitude = rnorm(n_houses, mel_cent[2], sd=mel_sd[2]),
)

V_LineList = tibble(
  RecordID = 1:n_people,
  CaseNumber = as.integer(paste0(1, 1:n_people)),
  Sex = ifelse(runif(n_people) < 0.5, "M", "F"),
  AgeAtOnset = round(runif(n_people, 0, 100)),
  Acquired = sample(c("Contact with a confirmed case", "Acquired in Australia, unknown source",
                      "Under investigation", "Close contact"), n_people, replace = TRUE),
  Symptoms = ifelse(runif(n_people) < 0.8, "Yes", "No"),
  CalculatedOnsetDate = as.character(today() - days(round(runif(n_people, 0, 30)))),
  SymptomsOnsetDate = as_date(CalculatedOnsetDate) + days(round(runif(n_people, 0, 5))),
  DiagnosisDate = as_date(CalculatedOnsetDate) + days(round(runif(n_people, 2, 10))),
  Classification = "Confirmed",
  Language = ifelse(runif(n_people) < 0.8, "Language1", "Language2"),
  CountryOfBirth = "Australia",
  Suburb = "Melbourne",
  Occupation = "Worker",
  RecordOwner = "DEMO",
)

V_Person = tibble(
  AccountID = 1:n_people,
  RecordID = 1:n_people,
  FirstName = paste(sample(LETTERS, 2, replace=T), collapse=""),
  Surname = paste(sample(LETTERS, 2, replace=T), collapse=""),
  house_ix = sample(houses$house_ix, n_people, replace=T),
  Longitude = houses$Longitude[house_ix],
  Latitude = houses$Latitude[house_ix],
  FullCleanAddress = houses$FullCleanAddress[house_ix]
)

Account = tibble(
  Id = 1:n_people,
  AddressLongitude__c = NA_real_,
  AddressLatitude__c = NA_real_
)

V_ClusterSites = tibble(
  ExposureCaseNumber = as.integer(paste0(9, 1:n_clustersites)),
  ClusterName = paste("Cluster", seq_len(n_clustersites)),
  StreetAddress = paste(sample(1:999, n_clustersites), "Street, VIC 3XXX"),
  LGA = "LGA",
  Postcode = "3000",
  Longitude = rnorm(n_clustersites, mel_cent[1], sd=mel_sd[1]),
  Latitude = rnorm(n_clustersites, mel_cent[2], sd=mel_sd[2]),
  SiteCategory1 = sample(c("Commercial", "Industrial", "Residential"), n_clustersites, replace=TRUE),
  SiteCategory2 = sample(c("Office", "Home", "Recreational"), n_clustersites, replace=TRUE),
  ClusterType = sample(c("Outbreak", "Investigation site"), n_clustersites, replace=TRUE),
  DeclaredDate = today() - days(14)
)

V_CaseLinks = bind_rows(
  tibble(
    SourceCaseNumber = sample(V_LineList$CaseNumber, round(n_caselinks * (1-ratio_clusterlinks_to_personlinks)), replace=TRUE),
    TargetCaseNumber = sample(V_LineList$CaseNumber, round(n_caselinks * (1-ratio_clusterlinks_to_personlinks)), replace=TRUE)
  ),
  tibble(
    SourceCaseNumber = sample(V_LineList$CaseNumber, round(n_caselinks * ratio_clusterlinks_to_personlinks), replace=TRUE),
    TargetCaseNumber = sample(V_ClusterSites$ExposureCaseNumber, round(n_caselinks * ratio_clusterlinks_to_personlinks), replace=TRUE)
  )) %>%
  filter(SourceCaseNumber != TargetCaseNumber) %>%
  distinct()

.casemovement_locations = tibble(
  MovementPlace = paste("Place", seq_len(n_casemovement_locations)),
  Address = paste(sample(1:(10*n_casemovement_locations), n_casemovement_locations), "Street, VIC 3XXX"),
  Longitude = rnorm(n_casemovement_locations, mel_cent[1], sd=mel_sd[1]),
  Latitude = rnorm(n_casemovement_locations, mel_cent[2], sd=mel_sd[2]),
  AddressType = sample(c("Commercial", "Industrial", "Residential"), n_casemovement_locations, replace=TRUE)
)

V_casemovement = tibble(
  ID = NA_character_,
  RecordID = sample(V_LineList$RecordID, n_casemovement, replace=TRUE),
  MovementDate = today() - days(round(runif(n_casemovement, 0, 30))),
  MovementTime = "00:00",
  ix_casemovement_location = sample(seq_len(n_casemovement_locations), n_casemovement, replace=TRUE),
  MovementPlace = .casemovement_locations$MovementPlace[ix_casemovement_location],
  FullCleanAddress = .casemovement_locations$Address[ix_casemovement_location],
  Longitude = .casemovement_locations$Longitude[ix_casemovement_location],
  Latitude = .casemovement_locations$Latitude[ix_casemovement_location],
  AddressType = .casemovement_locations$AddressType[ix_casemovement_location]
) %>%
  select(-ix_casemovement_location)

# These two just required to make the existing SQL query work
ContactEncounter = tibble(
  Id = character(),
  Location__c = character()
)
Location = tibble(
  Id = character(),
  AddressLongitude__c = numeric(),
  AddressLatitude__c = numeric()
)

# Static data previously supplied as a CSV
TestingSites_1 = tibble(
  sitename = paste("Test site", seq_len(n_testsites)),
  service = sample(c("Testing", "Vaccination"), n_testsites, replace=TRUE),
  address = paste(sample(1:999, n_testsites), "Street, VIC 3XXX"),
  lon = rnorm(n_testsites, mel_cent[1], sd=mel_sd[1]),
  lat = rnorm(n_testsites, mel_cent[2], sd=mel_sd[2]),
)