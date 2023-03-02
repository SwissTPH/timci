[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/github/last-commit/SwissTPH/timci.svg)](https://github.com/SwissTPH/timci/commits/master)

# Tools for Integrated Management of Childhood Illnesses (TIMCI)

`timci` is a R package for managing, exporting Markdown reports and visualising in a Shiny app the data collected by [ODK](https://getodk.org/) for the different studies of the TIMCI project (Tools for Integrated Management of Childhood Illnesses). 

The TIMCI project is funded by [Unitaid](https://unitaid.org/) and led by [PATH](https://www.path.org/), in partnership with the [Swiss Tropical and Public Health Institute (swiss TPH)](https://www.swisstph.ch/en/) and local research organisations.

The overall goal of the TIMCI project is to reduce morbidity and mortality in sick children attending primary care facilities, while supporting the rational and efficient use of diagnostics and medicines by healthcare providers. The project is conducted in three African countries (Kenya, Senegal and Tanzania) and two Asian countries (the Indian state of Uttar Pradesh (UP) and Myanmar).

|Country|Research partner|
|-|-|
|Kenya|University of Nairobi (UoN)|
|India|King's College Medical University (KGMU)|
|Myanmar|Burnet Institute (BI)|
|Senegal|Université Cheikh Anta Diop de Dakar (UCAD)|
|Tanzania|Ifakara Health Institute (IHI)|

## Table of contents
* [Getting started](https://github.com/SwissTPH/timci#getting-started)
* [Run a generic Shiny app](https://github.com/SwissTPH/timci#run-a-generic-shiny-app)
* [Run the TIMCI Shiny app](https://github.com/SwissTPH/timci#run-the-timci-shiny-app)
* [Generate R Markdown reports for TIMCI (manual)](https://github.com/SwissTPH/timci#generate-r-markdown-reports-for-timci-manual)
* [Generate R Markdown reports for TIMCI (automated pipeline)](https://github.com/SwissTPH/timci#generate-r-markdown-reports-for-timci-automated-pipeline)
* [Valuable resources](https://github.com/SwissTPH/timci#valuable-resources)

## Getting started
This package was developed on a Windows 10 operating system, with R version 4.0.2 (2020-06-22), RStudio version 1.4.1106 and Rtools40.
* [Link to download the installer for RTools](https://cran.r-project.org/bin/windows/Rtools/rtools40.html)
* [Link to download an R installer from CRAN mirrors](https://cran.r-project.org/mirrors.html)
* [Link to download the installer for RStudio](https://rstudio.com/products/rstudio/download/#download)

For Windows users, it is smoother to install 1) Rtools, 2) R and 3) RStudio in this order.

### Prerequisites 

#### ruODK
To install `RuODK`, which is the R client that the `timci` package uses to simply interact with the Application Programming Interface (API) of ODK Central, please use the following forked branch and not the official package (some functions needed by the `timci`package are only available in this branch and not in the official package)

```r
library(devtools)
devtools::install_github("thaliehln/ruODK", ref="test")
```

Otherwise, in general you would have to follow the instructions provided [here](https://docs.ropensci.org/ruODK/#install) to install `ruODK`.

##### ruODK installation difficulties
When trying to upgrade RuODK, if you encounter difficulties to upgrade some of the dependencies (e.g., `sf` package), you can switch `dependencies` and `build_vignettes` from `TRUE` to `FALSE`.
```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github(
    "ropensci/ruODK@main", 
    dependencies = FALSE, 
    upgrade = "always",
    build_vignettes = FALSE)
```

#### TinyTex LaTeX distribution
`TinyTeX` is a custom LaTeX distribution. It is needed to compile R Markdown documents to PDF.
To install `TinyTeX` from the `tinytex` package, please proceed as follow:
```r
install.packages("tinytex")
tinytex::install_tinytex()
```
If specific LaTeX packages (e.g., `fancyhdr`, `lastpage`, `babel`, `tocbibind`, `worldflags`) or styles are missing, they can be installed using the following commands:

```r
tinytex::tlmgr_install("fancyhdr")
```

```r
tinytex::parse_install(
    text = "! LaTeX Error: File `ulem.sty' not found."
)
```

In order to avoid the warning *No hyphenation patterns were preloaded for the language French into the format.*, please consider installing the package `hyphen-french`
```r
tinytex::tlmgr_install("hyphen-french")
```

#### emayili
This package is required to send emails using a gmail address.
The function `send_email_tls` in the `timci` package requires `emayili v0.6.5` and above to work correctly.

To know what version of the `emayili` package is installed on your computer, please proceed as follow:

```r
packageVersion("emayili")
```

If the version number is below `0.6.5`, please install the package from the master branch:
```r
remotes::install_github("datawookie/emayili")
```

From 30 May 2022 Google no longer supports signing in to Google Accounts using only usernames and passwords. Guidance on how to create and use an application password that can be used in `emayili` is available in the following post:
[{emayili} Updated Gmail Authentication](https://datawookie.dev/blog/2022/03/updated-gmail-authentication/).

#### webshot

To be able to render HTML widgets (e.g., diagrams generated using the package DiagrammeR) as screenshots in a Word or a PDF documents generated using Rmarkdown, you need to install the webshot package and download and install PhantomJS. More explanation available [here](https://bookdown.org/yihui/bookdown/html-widgets.html).
```r
install.packages("webshot")
webshot::install_phantomjs()
```

#### Other R dependencies

You need to have the following R packages installed: `hash`, `shiny`, `qrcode`, `readxl`, `ggplot2`, `dplyr`, `viridis`, `pryr`, `flexdashboard`, `magrittr`, `scales`, `tidyr`, `DT`, `data.table`, `openxlsx`, `rmarkdown`, `stringr`, `qwraps2`, `digest`, `readr`, `fs`, `kableExtra`, `dataMaid`, `skimr`, `DataExplorer`, `sf`, `spData`, `finalfit`, `webshot`, `DiagrammeR`.

All these packages can be installed using the function `install.packages`:
```r
install.packages("hash")
install.packages("shiny")
install.packages("qrcode")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("viridis")
install.packages("pryr")
install.packages("flexdashboard")
install.packages("magrittr")
install.packages("scales")
install.packages("tidyr")
install.packages("DT")
install.packages("data.table")
install.packages("openxslx")
install.packages("rmarkdown")
install.packages("kableExtra")
install.packages("finalfit")
install.packages("DiagrammeR")
```

### Installation of timci from GitHub
To install the `timci` package on your workstation, you can
* either download the `master` branch as a ZIP and build the package locally

![image](https://user-images.githubusercontent.com/71643277/111299284-0d164100-8650-11eb-8688-8bd152214d56.png)

* or install the latest released version (preferred option)  
```r
install.packages("devtools")
```
```r
library(devtools)
devtools::install_github("SwissTPH/timci")
```

An index of available documentation for the `timci` is displayed using the `help()` function
```r
help(package="timci")
```

### Setup of the connection to ODK Central
You need to setup `ruODK` by creating environment variables in a `~/.Renviron` file that will contain the OData service URL of your main project as well as your credentials (username and password) on ODK Central.

![image](https://user-images.githubusercontent.com/71643277/113840950-87d70580-9791-11eb-9433-7073079961f7.png)

```bash
ODKC_SVC="https://.../v1/projects/.../forms/....svc"
ODKC_UN=me@email.com
ODKC_PW=...
TZ="Europe/Zurich"
```
For more information about `ruODK`, the detailed documentation is available [here](https://docs.ropensci.org/ruODK/)

For more information about the `ODK Central API`, the detailed documentation is available [here](https://odkcentral.docs.apiary.io/)

### Setup specific to the TIMCI project
For the TIMCI project, you need to set the following environment variables in your `~/.Renviron` file.

![image](https://user-images.githubusercontent.com/71643277/113840980-8dcce680-9791-11eb-98ac-c95c4a0a59bf.png)

```bash
TIMCI_COUNTRY="Tanzania"
TIMCI_IS_RCT=1

TIMCI_RCTLS_PID=14
TIMCI_PII_PW=...
TIMCI_RCTLS_PP=...
TIMCI_CRF_FACILITY_FID="01-TIMCI-CRF-Facility"
TIMCI_CRF_DAY7_FID="01c-TIMCI-CRF-Day7"
TIMCI_CRF_HOSPIT_FID="01b-TIMCI-CRF-Hospital"
TIMCI_CRF_DAY28_FID="01d-TIMCI-CRF-Day28"
TIMCI_WEEKLY_FA_FID="01e-TIMCI-weekly-FA"

TIMCI_PILOT_RCTLS_PID=7
TIMCI_PILOT_RCTLS_PP=...

TIMCI_SPA_PID=2
TIMCI_SPA_CGEI_FID="02-TIMCI-SPA-CGEI"
TIMCI_SPA_FA_FID="03-TIMCI-SPA-Fassmt"
TIMCI_SPA_SCO_FID="05-TIMCI-SPA-SCO"
TIMCI_SPA_HCPI_FID="04-TIMCI-SPA-HCPi"
TIMCI_TF_FID="07-TIMCI-timeflow"

TIMCI_PILOT_SPA_PID=2

TIMCI_QUAL_PID=12
TIMCI_QUAL_PP=...
TIMCI_QUAL_CGIDI1_FID="08a-TIMCI-cg-idi-invitation"
TIMCI_QUAL_CGIDI2_FID="08b-TIMCI-cg-idi-encryption-list"
TIMCI_QUAL_CGIDI3_FID="08c-TIMCI-cg-idi-interview"

TIMCI_WD_FID="99-TIMCI-withdrawal"

EMAIL_UN=...
EMAIL_PW=...
```

* `TIMCI_COUNTRY` is an environment variable that sets the name of your country (`India`, `Kenya`, `Myanmar`, `Senegal` and `Tanzania`).

* `TIMCI_IS_RCT` is an environment variable that indicates if the main data collection is the pragmatic cluster randomised controlled trial (RCT) or the longitudinal observational study (LS). It should be set to `1` in Tanzania and India where the main data collection is the pragmatic cluster RCT, and `0` in Kenya, Myanmar, and Senegal where the main data collection is the LS.

* `TIMCI_PII_PW` is an environment variable that sets the password of the encrypted archive that is generated by the R pipeline to store confidential information (and in particular, personally identifiable information).

* `TIMCI_RCTLS_PID` is an environment variable that sets the ODK Central ID for the main data collection (either the pragmatic cluster RCT or the LS).

* `TIMCI_RCTLS_PP` is an environment variable that sets the ODK Central encryption passphrase for the main data collection (either the pragmatic cluster RCT or the LS).

* `TIMCI_CRF_FACILITY_FID` is an environment variable that sets the ODK Central ID for the facility form.

* `TIMCI_CRF_DAY7_FID` is an environment variable that sets the ODK Central ID for the Day 7 follow-up form.

* `TIMCI_CRF_HOSPIT_FID` is an environment variable that sets the ODK Central ID for the hospitalisation follow-up form.

* `TIMCI_CRF_DAY28_FID` is an environment variable that sets the ODK Central ID for the Day 28 follow-up form.

* `TIMCI_WEEKLY_FA_FID` is an environment variable that sets the ODK Central ID for the weekly facility assessment form.

* `TIMCI_SPA_PID` is an environment variable that sets the ODK Central ID for the Service Provision Assessment (SPA) project.

* `TIMCI_QUAL_PID` is an environment variable that sets the ODK Central ID for the qualitative study project.

* `TIMCI_QUAL_PP` is an environment variable that sets the ODK Central encryption passphrase for the qualitative study project.

* `TIMCI_SPA_CGEI_FID` is an environment variable that sets the ODK Central ID for the SPA caregiver exit interview form.

* `TIMCI_SPA_FA_FID` is an environment variable that sets the ODK Central ID for the SPA facility assessment form.

* `TIMCI_SPA_SCO_FID` is an environment variable that sets the ODK Central ID for the SPA sick child observation protocol form.

* `TIMCI_SPA_HCPI_FID` is an environment variable that sets the ODK Central ID for the SPA healthcare provider interview form.

* `TIMCI_TF_FID` is an environment variable that sets the ODK Central ID for the time-flow form.

* `TIMCI_WD_FID` is an environment variable that sets the ODK Central ID for the withdrawal form.

![image](https://user-images.githubusercontent.com/71643277/110971003-9d0b7080-835a-11eb-9b56-eb493d4c5e27.png)

### Setup of the automated R Markdown pipeline
`pandoc` is a document converter that can convert files from one markup format into another (e.g., .docx, .pdf etc). It is an independent piece of software, separate from R. However, it comes bundled with R Studio because the `rmarkdown` package relies on it for document conversion. Rstudio uses the `RSTUDIO_PANDOC` environment variable to find where `pandoc` is installed on your machine. To use `rmarkdown` in a batch, `RSTUDIO_PANDOC` needs to be set in your `~/.Renviron` file.

![image](https://user-images.githubusercontent.com/71643277/113841176-bf45b200-9791-11eb-8077-6d29fd0ece8b.png)

```bash
RSTUDIO_PANDOC='C:/Program Files/RStudio/bin/pandoc'
```

## Run a generic Shiny app 
`run_mini_app` should work with any ODK Central server
```R
library(timci)
timci::run_mini_app()
```
## Run the TIMCI Shiny app
`run_app` requests an access to a TIMCI ODK Central server.
```R
library(timci)
timci::run_app()
```

## Generate R Markdown reports for TIMCI (manual)

You should now have the following three files in your working directory: `timci_run.R`, `timci_research_facilities.xlsx` and `~/.Renviron`

![image](https://user-images.githubusercontent.com/71643277/113839377-ebf8ca00-978f-11eb-9b58-8c0bd23aa76c.png)

`timci_research_facilities.xlsx` maps the correspondences between the ODK Collect device identifiers and the health facilities where the research assistants are posted.

![image](https://user-images.githubusercontent.com/71643277/113838910-8573ac00-978f-11eb-8ddd-9238595b26df.png)

`timci_run.R` contains the R script to generate R Markdown reports for TIMCI. An example is provided [here](https://github.com/SwissTPH/timci/wiki/TIMCI-report-generation-R-script). /!\ The `run_rmarkdown` function requests an internet access to a TIMCI ODK Central server to work correctly.

![image](https://user-images.githubusercontent.com/71643277/113839903-69bcd580-9790-11eb-9620-2607634b2217.png)

```R
gc() # Garbage collection

library(timci)
library(readxl)

stack_size <- getOption("pandoc.stack.size", default = "2048m")
email <- 0

# Set key dates
start_date <- as.Date(Sys.getenv("TIMCI_START_DATE"))
write(start_date, stderr())
spa_start_date <- as.Date(Sys.getenv('TIMCI_SPA_START_DATE'))
lock_date <- as.Date(Sys.getenv('TIMCI_CLEANING_END_DATE'))

# Set the root directory for storing results
output_dir <- file.path(getwd(),"timci_exports")
# Import the mapping between the ODK Collect device identifiers and the health facilities where the research assistants are posted
research_facilities <- read_excel(file.path(getwd(),"timci_research_facilities.xlsx"))

###############################
# RANDOMISED CONTROLLED TRIAL #
###############################

write(timci::formats2h1("RANDOMISED CONTROLLED TRIAL"), stderr())

# Create the structure of the folder and subfolders that are created everyday to store the reports and exports
study_dirname <- paste0("02_", Sys.getenv('TIMCI_COUNTRY'), "_main_study")
dirs <- timci::generate_folder_structure(output_dir, study_dirname)

rctls_pid <- Sys.getenv("TIMCI_RCTLS_PID")
rctls_pp <- Sys.getenv("TIMCI_RCTLS_PP")
spa_pid <- Sys.getenv("TIMCI_SPA_PID")
qual_pid <- Sys.getenv("TIMCI_QUAL_PID")
qual_pp <- Sys.getenv("TIMCI_QUAL_PP")
cost_pid <- Sys.getenv("TIMCI_COST_PID")

# Run several Rmarkdown files to generate standardised automated reports for the main study
timci::run_rmarkdown_reportonly(rctls_pid = rctls_pid,
                                rctls_pp = rctls_pp,
                                spa_pid = spa_pid,
                                cost_pid = cost_pid,
                                qpid = qual_pid,
                                qual_pp = qual_pp,
                                research_facilities = research_facilities,
                                report_dir = dirs[[5]],
                                participant_zip = file.path(dirs[[2]], "participants.zip"),
                                mdb_dir = dirs[[2]],
                                fu_dir = dirs[[3]],
                                spa_db_dir = dirs[[4]],
                                qc_dir = dirs[[6]],
                                lock_dir = dirs[[7]],
                                cost_dir = dirs[[9]],
                                qualcg_dir = dirs[[10]],
                                qualhcp_dir = dirs[[11]],
                                qualkii_dir = dirs[[12]],
                                qualos_dir = dirs[[13]],
                                path_dir = dirs[[8]],
                                start_date = start_date,
                                end_date = NULL,
                                spa_start_date = spa_start_date,
                                lock_date = Sys.Date(),
                                sample_size = 110880)

gc(verbose = TRUE) # Garbage collection and prints memory usage statistics
```

## Generate R Markdown reports for TIMCI (automated pipeline)
### Setup of the Windows task scheduler

Create a .bat file that calls `timci_run.R`

![image](https://user-images.githubusercontent.com/71643277/113839973-7d683c00-9790-11eb-9f7d-50f668186f6c.png)

```bat
cd dirname
"...\Rscript.exe" ...\timci_run.R
PAUSE
```
You should now have four files in your working directory: `timci_run.R`, `timci_daily_routine.bat`, `timci_research_facilities.xlsx` and `~/.Renviron`

![image](https://user-images.githubusercontent.com/71643277/113839486-02068a80-9790-11eb-9d55-6d65f492f41f.png)

1. Open the task scheduler

![image](https://user-images.githubusercontent.com/71643277/110971229-e360cf80-835a-11eb-9e66-c9a328b8493b.png)

2. Action > Create Task

![image](https://user-images.githubusercontent.com/71643277/110971196-d8a63a80-835a-11eb-9557-a37b7ece4f6c.png)

3. ... (to be completed soon)

![image](https://user-images.githubusercontent.com/71643277/110971144-cb894b80-835a-11eb-9797-e5b0c71a911b.png)  
![image](https://user-images.githubusercontent.com/71643277/110971360-12774100-835b-11eb-8d84-a7ed9f7b02bf.png)  
![image](https://user-images.githubusercontent.com/71643277/110971385-1b681280-835b-11eb-8510-6ca0ea8f7bc3.png)

## Valuable resources

### ODK resources
[OpenDataKit (ODK)](https://getodk.org/) is a free-and open-source software that helps millions of people collect data quickly, accurately, offline, and at scale. [ODK Central](https://docs.getodk.org/central-intro/) is the latest cloud-based data clearinghouse ODK solution for digitally captured data.

1. [ODK Central setup](https://docs.getodk.org/central-setup/)

2. [ODK Central apiary](https://odkcentral.docs.apiary.io/) contains a very comprehensive documentation of the ODK Central RESTful API. Note that anything that can be done in the ODK Central's web user interface can be done directly via the API.

3. [RuODK](https://docs.ropensci.org/ruODK/) is an open-source R client to access and parse data from ODK Central, which is developed and maintained by Florian Mayer for the Western Australian Department of Biodiversity, Conservation and Attractions (DBCA).

4. [ODK forum](https://forum.getodk.org/) already contains a fantastic amount of information and the core ODK team is very reactive.

### R resources

<a href="https://shiny.rstudio.com/articles/"><img src="http://r-pkgs.org/images/cover.png" height="252" align = "right"/></a>

1. [Shiny package](https://shiny.rstudio.com/articles/) has extensive documentation on best practices for R Shiny application development.

2. (reproduced as is from the Devtools README) There are a number of fantastic blog posts on writing your first package, including
   - [Writing an R package from scratch - Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
   - [How to develop good R packages - Maëlle Salmon](http://www.masalmon.eu/2017/12/11/goodrpackages/)
   - [Making your first R package - Fong Chun Chan](http://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html)
   - [Writing an R package from scratch - Tomas Westlake](https://r-mageddon.netlify.com/post/writing-an-r-package-from-scratch/)

3. (reproduced as is from the Devtools README) [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) is
   the exhaustive, canonical reference for writing R packages, maintained by R core developers.

#### R cheatsheets / reference guides
<a href="https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf"><img src="https://raw.githubusercontent.com/batpigandme/cheatsheets/1c942c36846559b3e8efbd40d023bc351aeed6ba/pngs/thumbnails/package-development-thumbs.png" height="252"/></a>

<a href="https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf">Rmarkdown cheat sheet</a>

## Contributing
Pull requests are welcome on the `develop` branch. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[GPL-3]

## Demo purpose
