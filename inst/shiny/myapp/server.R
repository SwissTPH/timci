# Defines the server logic for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

library(ruODK)
library(hash)

server <- function(input, output) {

  # Connect to the ODK Central server using ruODK
  ruODK::ru_setup(
    svc = "https://timicodktest.smartforest.de/v1/projects/2/forms/01-TIMCI-CRF-Facility.svc",
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = "Europe/Zurich",
    verbose = FALSE # Can be switched to TRUE for demo or debugging
  )

  # Load ODK forms which have submissions
  odk_form_list <- ruODK::form_list()
  odk_form_vlist <- subset(odk_form_list, submissions > 0, select=c(fid))

  # Load ODK data
  odk_data <- hash()
  for(form in odk_form_vlist){
    current_odk_data <- ruODK::odata_submission_get(fid = form)
    # Process ODK data (this will depend on the form - here targeting RCT / LS main form)
    odk_data[[form]] <- format_odk_data(current_odk_data)
  }

  # De-identify ODK data
  research_data <- hash()
  for(form in odk_form_vlist){
    research_data[[form]] <- deidentify_data(odk_data[[form]])
  }

  # Load dummy data
  dummy_tuple <- timci::load_dummy_data()
  dummy_data <- dummy_tuple[['data']]

  # Display dummy data in a table
  timci::dummy_data_table_server("tab1", dummy_data[["Arm I CDSA POX"]])

  # Display enrolment gauge on dummy data
  timci::enrolment_tab_server("enrolment", dummy_tuple[['df']])

  # Display ODK data in a table
  timci::odk_data_table_server("tab2", odk_form_vlist$fid, odk_data)

  # Display de-identified research data in a table
  timci::odk_data_table_server("tab3", odk_form_vlist$fid, research_data)

}
