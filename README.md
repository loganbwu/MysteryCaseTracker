# Mystery Case Tracker

This dashboard displays PHESS data in an epidemiologically useful format.


## Getting Started

[Download](https://rstudio.com/products/rstudio/download/) and install [RStudio](https://rstudio.com/). You can access RStudio primers from the [rstudio.cloud website](https://rstudio.cloud/learn/primers).

### Prerequisites

Install R and any required packages:

```{r}
install.packages(c("igraph", "visNetwork", "dplyr", "data.table", "ggplot2",
  "lubridate", "sf", "leaflet", "forcats", "scales", "stringr", "dygraphs",
  "DT", "xts", "RODBC", "odbc", "generator", "pins"))
```

### Installing

Open the project in RStudio. With `server.R` or `ui.R` open, launch a local version of the app with "Run app" or `shiny::runApp()`.

## Deployment

There are two deployment options. Both require refreshing of data and updating of the app.

### Virtual Machine

Requires VM and E: drive access.

#### Data

Also requires an ODBC data source connection to be set up.

| ODBC Config          | Value                                           |
|----------------------|-------------------------------------------------|
| Driver               | ODBC Driver 17 for SQL Server                   |
| Name                 | PHESS                                           |
| Server               | p-ause-ps-sql-s-hpb-rpt-01.database.windows.net |
| With AAD Interactive | first.last<span>@</span>dhhs.vic.gov.au         |
| Change default db    | phess-dw01                                      |
| Application intent   | READONLY                                        |

Confirm that the saveRDS functions at the bottom are writing to the same location as the app - this should be `E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/36 Mystery Cases/mysteryLinksApp/data-processed`. For testing it can write to `E:/EHU/COVID19/COVID_mysteryLinks/data-processed/`.

Run `preprocessing_DB.R` in RStudio. It will prompt you for your DHHS username and password.

#### App

Overwrite contents of `E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/36 Mystery Cases/mysteryLinksApp/code` with the contents of the `/R/` folder in the `main` branch. The `runApp.R` file launches the app from here. To give users a double-click executable, create a `.bat` file that executs `runApp.R`.

### RStudio Connect

Deployed to https://rstudio.covid19.dhhs.vic.gov.au/

#### Data

Publish `preprocessing.Rmd` to RStudio Connect. Add environment variables so it can authenticate with SQL Server and write to pins (RStudio will initially report a publishing error - add the variables and refresh in RStudio Connect):

- PHESS_DW01_UID: *redacted*
- PHESS_DW01_PWD: *password*
- RSCONNECT_API_KEY: *generate an API key*

Schedule this RMarkdown every day or hourly so that it updates the pinned data.

*Note: a potential issue that may come up is the owner of the pins; the full name is prepended by the username/email i.e., logan.wu<span>@</span>dhhs.vic.gov.au/LineList. The pins may also need to be shared with the correct people.*

#### App

Publish the app in `/R/` from the `main` branch in RStudio to RStudio Connect.

*Note: the same person should probably publish these because it creates a folder under `/R/rsconnect/username`* to track where the app belongs for future updates.

Add an environment variable to give the app access to pins:

- RSCONNECT_API_KEY: *generate an API key*

## Team

* **Jose Canevari** (dev)
* **Fazil Hassan** (test)
* **Ian Hepples** (BTIM)
* **Julia Hofkes** (epi)
* **Michael Lydeamore** (dev)
* **Jayaraman Madhavan** (dev)
* **Hugh Parsonage** (dev)
* **Claire Shallue** (project manager)
* **Abhik Vyas** (scrum master)
* **Logan Wu** (dev)

## Development

### Branch policy

There are two core branches: `master` and `release`.  

Mirroring the branches are the MASTER and RELEASE versions of the app on 
the RStudio Server via rsconnect.

Both the `master` and `release` branches are _protected_ branches;
any change to either of them requires approval by another team member.
The reviewer is responsible for merging and publishing.


The `release` branch is always the state of the current version in use. 
To release a new version, 
fetch the head of `master` into your local copy of the repository,
merge into the `release` branch, publish the RELEASE version of the app, 
then push the `release` branch. 
(Hence, at every release, the `master` branch is even with `release`.)
Then republish the `master` branch to the MASTER app on rsconnect.

The `master` branch is the 'default' branch. To make a change to the app, make
a pull request to the `master` branch and seek approval from another team member.
After every change to the `master` branch, the person reviewing makes the pull
request and republishes the the MASTER app on rsconnect.

To review a pull request, check out the relevant feature branch locally and test.
To suggest a change, make a pull request to that branch.