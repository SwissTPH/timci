# TIMCI

`timci` is a R package for managing, exporting Markdown reports and visualising in a Shiny app the data collected by ODK for the different studies of the TIMCI project (Tools for Integrated Management of Childhood Illnesses).

## Installation of timci from GitHub
You can download the `master` branch of the `timci` package.

## Setup of the ODK Central connection
You need to setup `ruODK` by creating environment variables in `~/.Renviron` containing the OData service URL of your project as well as your credentials (username and password) on ODK Central:
```bash
ODKC_SVC="https://.../v1/projects/.../forms/....svc"
ODKC_UN=me@email.com
ODKC_PW=...
TZ="Europe/Zurich"
```
For more information about `ruODK`, there are lots of great resources available at [https://docs.ropensci.org/ruODK/](https://docs.ropensci.org/ruODK/)

For more information about the `ODK Central API`, there are lots of great resources available at [https://odkcentral.docs.apiary.io/](https://odkcentral.docs.apiary.io/)

## TIMCI-specific setup
Other environment variables to set in `~/.Renviron`:
```bash
COUNTRY="Switzerland"
PII_PW=...
```
## Usage

### Run a generic Shiny app 
Should work with any ODK Central server
```R
library(timci)
timci::run_mini_app()
```
### Run the TIMCI Shiny app
This requests access to the TIMCI ODK Central server.
```R
library(timci)
timci::run_app()
```

### Generate several R Markdown reports for TIMCI
This requests access to the TIMCI ODK Central server.

Create a R script `timci_daily_routine.R`

```R
library(timci)
Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/RStudio/bin/pandoc')
output_dir <- "directory_name"
subdir <- paste0("reports_", Sys.Date())
dir.create(file.path(output_dir, subdir), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "01_databases"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "02_followup"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "03_qualitative"), showWarnings = FALSE)
dir.create(file.path(output_dir, subdir, "04_reports"), showWarnings = FALSE)
timci::run_rmarkdown(file.path(output_dir,subdir, "04_reports"), file.path(output_dir, subdir, "participants.zip"), file.path(output_dir, subdir, "01_databases"), file.path(output_dir, subdir, "02_followup"), file.path(output_dir, subdir, "03_qualitative")
```
## Setup of the Windows task scheduler
Create a .bat file that calls `timci_daily_routine.R`
```bat
cd dirname
"...\Rscript.exe" ...\timci_daily_routine.R --cd-to-userdocs
PAUSE
```

Open the task scheduler

Action > Create Task

To be completed soon

## Valuable resources

### ODK Central

1. [ODK Central setup](https://docs.getodk.org/central-setup/)

2. [ODK Central apiary](https://odkcentral.docs.apiary.io/) contains a very comprehensive documentation of the ODK Central RESTful API. Note that anything that can be done in ODK Central's web user interface can be done directly via the API.

3. [RuODK](https://docs.ropensci.org/ruODK/) is an open-source R client to access and parse data from ODK Central.

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
