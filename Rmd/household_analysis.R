library(odbc)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)

linelist = fread("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/Modelling/snapshots/latest/linelist.txt",
                 select = c("RecordID", "CaseNumber", "DiagnosisDate", "Classification", "Acquired", "PresentedTo", "Address", "AddressLine2", "Sex"),
                 na.strings = "")
person = fread("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/Modelling/snapshots/latest/Persons.txt",
               select = c("RecordID", "Surname", "Age", "FullCleanAddress"),
               na.strings = "")

conf_or_pcc = linelist %>%
  mutate(DiagnosisDate = dmy(DiagnosisDate)) %>%
  filter(Classification == "Contact - active" |
           Classification == "Confirmed" & DiagnosisDate > as_date("2021-07-01") & Acquired != "Travel overseas") %>%
  left_join(person, by="RecordID") %>%
  select(-RecordID, -Acquired)
conf_or_pcc = conf_or_pcc %>%
  mutate(Isolating = PresentedTo %in% c("Isolating in alternative accommodation", "Hospital admission") |
           str_detect(Address, "(C|c)/-"),
         Age = Age %>% replace_na(999)) %>%
  as_tibble

confirmed_addresses = conf_or_pcc %>%
  filter(Classification=="Confirmed") %>%
  mutate(Address = coalesce(FullCleanAddress, Address),
         address_filtered = Address %>% str_remove(",") %>% str_squish %>%
           str_to_lower %>% str_remove(" (road|rd|street|grove|close|avenue|drive|place|court|crescent|c/-).*$") %>%
           str_replace("st mich", "saint mich")) %>%
  group_by(address_filtered) %>%
  summarise(n = n(),
            Addresses = paste(Address, collapse=", "))

addresses = conf_or_pcc %>%
  mutate(Address = coalesce(FullCleanAddress, Address),
         address_filtered = Address %>% str_remove(",") %>% str_squish %>%
           str_to_lower %>% str_remove(" (road|rd|street|terrace|grove|close|avenue|drive|place|court|crescent|c/-).*$") %>%
           str_replace("st mich", "saint mich"),
         Surname = str_to_title(Surname),
         detail = paste0(Surname, " (", Age, str_extract(Sex, "^."), ifelse(Classification=="Confirmed", "+", "-"), ")") %>% na_if(" ()")) %>%
  drop_na(address_filtered) %>%
  group_by(address_filtered) %>%
  filter(sum(Classification=='Confirmed') > 0) %>%
  summarise(Addresses = paste(unique(Address), collapse=", "),
            n_confirmed_isolating = sum(Isolating & Classification=="Confirmed"),
            n_confirmed_present = sum(!Isolating & Classification=="Confirmed"),
            n_contact = sum(Classification=='Contact - active'),
            n_distinct_adult_surnames = n_distinct(Surname[Age>=18]),
            CaseNumbers = paste(CaseNumber, collapse=", "),
            AdultSurnames = paste(detail[Age>=18], collapse=", "),
            ChildSurnames = paste(detail[Age<18], collapse=", "),
            PresentedTo = paste(PresentedTo, collapse=", "),
            FirstDiagnosisDate = min(DiagnosisDate, na.rm=TRUE),
            LastDiagnosisDate = max(DiagnosisDate, na.rm=TRUE),
            suggest_further_isolation = ifelse(n_confirmed_present > 0 & n_contact > 0, "Yes", "No")) %>%
  filter(n_distinct_adult_surnames > 2) %>%
  select(-address_filtered) %>%
  arrange(LastDiagnosisDate)

write.csv(addresses, "~/shared_address_isolation.csv", row.names=FALSE)
write.xlsx(addresses, "~/shared_address_isolation.xlsx", asTable = TRUE)

addresses %>% filter(suggest_further_isolation=="Yes") %>% View
