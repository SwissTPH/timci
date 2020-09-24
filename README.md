# TIMCI

TIMCI is a R package for managing the data collected through the different studies of the TIMCI project (Tools for Integarted Management of Childhood Illnesses).

## Installation of from GitHub:
# install.packages("devtools")
devtools::install_github("Thaliehln/timci")

```bash
install.packages('timci')
```

## Cheatsheet (reproduced as is from the Devtools README)

<a href="https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf"><img src="https://raw.githubusercontent.com/batpigandme/cheatsheets/1c942c36846559b3e8efbd40d023bc351aeed6ba/pngs/thumbnails/package-development-thumbs.png" height="252"/></a>

## Usage

```R
library(timci)

timci::run_app() # Run Shiny app
```

## Useful resources

R package development can be intimidating, however there are now a number of
valuable resources to help!

<a href="https://shiny.rstudio.com/articles/"><img src="http://r-pkgs.org/images/cover.png" height="252" align = "right"/></a>

1. [Shiny package](https://shiny.rstudio.com/articles/) has extensive documentation on best practices for R Shiny application development.

2. (reproduced as is from the Devtools README) There are a number of fantastic blog posts on writing your first package, including
   - [Writing an R package from scratch - Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
   - [How to develop good R packages - Maëlle Salmon](http://www.masalmon.eu/2017/12/11/goodrpackages/)
   - [Making your first R package - Fong Chun Chan](http://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html)
   - [Writing an R package from scratch - Tomas Westlake](https://r-mageddon.netlify.com/post/writing-an-r-package-from-scratch/)

3. (reproduced as is from the Devtools README) [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) is
   the exhaustive, canonical reference for writing R packages, maintained by the R core developers.

4. [RuODK](https://docs.ropensci.org/ruODK/) is an R client to access and parse data from ODK Central.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[GPL-3]