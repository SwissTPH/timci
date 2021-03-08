[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/github/last-commit/Thaliehln/timci.svg)](https://github.com/Thaliehln/timci/commits/master)

# Tools for Integrated Management of Childhood Illnesses (TIMCI)

`timci` is a R package for managing, exporting Markdown reports and visualising in a Shiny app the data collected by ODK for the different studies of the TIMCI project (Tools for Integrated Management of Childhood Illnesses). 

The overall goal of the TIMCI project is to reduce morbidity and mortality in sick children attending primary care facilities, while supporting the rational and efficient use of diagnostics and medicines by healthcare providers. The project is conducted in three African (Kenya, Senegal and Tanzania) and two Asian (India and Myanmar) countries.

This package has been developed on a Windows 10 operating system, with RStudio version 1.3.1073 using R v4.0.2.

## Installation of timci from GitHub
You can download the `master` branch of the `timci` package.

You need to have the following R packages installed: `ruODK`, `hash`, `shiny`, `qrcode` (non-exhaustive list, to be completed)

## Setup of timci 

### Setup of the ODK Central connection
You need to setup `ruODK` by creating environment variables in `~/.Renviron` containing the OData service URL of your project as well as your credentials (username and password) on ODK Central:
```bash
ODKC_SVC="https://.../v1/projects/.../forms/....svc"
ODKC_UN=me@email.com
ODKC_PW=...
TZ="Europe/Zurich"
```
For more information about `ruODK`, there are lots of great resources available at [https://docs.ropensci.org/ruODK/](https://docs.ropensci.org/ruODK/)

For more information about the `ODK Central API`, there are lots of great resources available at [https://odkcentral.docs.apiary.io/](https://odkcentral.docs.apiary.io/)

### TIMCI-specific setup
For the TIMCI project, you need to set the following environment variables in your `~/.Renviron` file:
```bash
INSTITUTION="Swiss Tropical and Public Health Institute"
PII_PW=...
```

`PII_PW` contains the password of the encrypted archive that will store participant personally identifiable information

### Setup of the automated R Markdown pipeline
`pandoc` is a document converter that can convert files from one markup format into another (e.g., .docx, .pdf etc). It is an independent piece of software, separate from R. However, it comes bundled with R Studio because the `rmarkdown` package relies on it for document conversion. Rstudio uses the `RSTUDIO_PANDOC` environment variable to find where `pandoc` is installed on your machine. To use `rmarkdown` in a batch, `RSTUDIO_PANDOC` needs to be set in your `~/.Renviron` file:
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
`run_rmarkdown` requests an access to a TIMCI ODK Central server.

```R
library(timci)
output_dir <- "directory_name"
subdir <- paste0("reports_", Sys.Date())
dir.create(file.path(output_dir, subdir), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "01_databases"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "02_followup"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "03_qualitative"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "04_reports"), showWarnings = FALSE)
timci::run_rmarkdown(file.path(output_dir,subdir, "04_reports"), file.path(output_dir, subdir, "participants.zip"), file.path(output_dir, subdir, "01_databases"), file.path(output_dir, subdir, "02_followup"), file.path(output_dir, subdir, "03_qualitative")
```
## Generate R Markdown reports for TIMCI (automated pipeline)
### Setup of the Windows task scheduler
Save the code needed to generate R Markdown reports for TIMCI (manual) in a R script, e.g. `timci_daily_routine.R`

Create a .bat file that calls `timci_daily_routine.R`
```bat
cd dirname
"...\Rscript.exe" ...\timci_daily_routine.R --cd-to-userdocs
PAUSE
```

1. Open the task scheduler

2. Action > Create Task

3. ... (to be completed soon)

## Valuable resources

### ODK
[OpenDataKit (ODK)](https://getodk.org/) is a free-and open-source software that helps millions of people collect data quickly, accurately, offline, and at scale. [ODK Central](https://docs.getodk.org/central-intro/) is the latest cloud-based data clearinghouse ODK solution for digitally captured data.

1. [ODK Central setup](https://docs.getodk.org/central-setup/)

2. [ODK Central apiary](https://odkcentral.docs.apiary.io/) contains a very comprehensive documentation of the ODK Central RESTful API. Note that anything that can be done in the ODK Central's web user interface can be done directly via the API.

3. [RuODK](https://docs.ropensci.org/ruODK/) is an open-source R client to access and parse data from ODK Central, which is developed and maintained by Florian Mayer for the Western Australian Department of Biodiversity, Conservation and Attractions (DBCA).

4. [ODK forum](https://forum.getodk.org/) already contains a fantastic amount of information and the core ODK team is very reactive.

### R

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

<a href="https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf"><img src="https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference-guide.png" height="252"/></a>

## Contributing
Pull requests are welcome on the `development` branch. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[GPL-3]
