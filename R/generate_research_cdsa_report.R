#' Generate a report comparing research and CDSA data
#'
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param start_date Qualitative data collection start date (optional)
#' @param end_date Qualitative data collection end date (optional)
#' @export

generate_research_cdsa_report <- function(research_facilities,
                                          report_dir,
                                          start_date = NULL,
                                          end_date = NULL) {

  ###########################
  # Set up current language #
  ###########################

  timci::set_language_settings()

  ######################
  # Set up study dates #
  ######################

  if (is.null(end_date)) {
    end_date <- Sys.Date()
  } else{
    end_date <- as.Date(end_date, "%Y-%m-%d")
  }

  ###################################
  # Load TIMCI research & CDSA data #
  ###################################

  write(formats2h1("Load TIMCI research data"), stderr())


  write(formats2h1("Load TIMCI CDSA data"), stderr())


  ############################
  # Research vs. CDSA report #
  ############################

  write(formats2h1("Generate research vs. CDSA report"), stderr())

  if (!is.null(baseline_data)) {
    if (length(baseline_data) > 0) {
      params <- list(research_facilities = research_facilities,
                     baseline_data = NULL,
                     cdsa_data = cgidi_interview_data,
                     start_date = NULL,
                     end_date = NULL)
      generate_pdf_report(report_dir, "compare_rctls_cdsa_report.Rmd", "timci_research_cdsa_report", params)
    }
  }

}
