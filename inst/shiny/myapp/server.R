# Defines the server logic for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

library(ruODK)
library(hash)

server <- function(input, output) {

  #######################################
  # ODK CENTRAL SERVER CONNECTION SETUP #
  #######################################

  # Connect to the ODK Central server using ruODK
  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = "Europe/Zurich",
    verbose = FALSE # Can be switched to TRUE for demo or debugging
  )

  ################
  # RAW ODK DATA #
  ################

  # Load ODK forms that have submissions
  odk_form_list <- ruODK::form_list()
  valid_odk_forms <- subset(odk_form_list, submissions > 0, select=c(fid))
  valid_odk_form_list <- valid_odk_forms$fid

  # Load ODK data
  odk_data <- hash::hash()
  for(form in valid_odk_form_list){
    current_odk_data <- ruODK::odata_submission_get(fid = form)
    # Process ODK data (this will depend on the form - here targeting RCT / LS main form)
    odk_data[[form]] <- format_odk_data(current_odk_data)
  }

  # Execute the form selection module
  current_odk_form <- timci::select_list_item_server("select_odk_form",
                                                     valid_odk_form_list,
                                                     odk_data)

  # Execute the info module
  timci::odk_data_info_server("raw_odk_info", current_odk_form)

  # Execute the table module
  timci::odk_data_table_server("raw_odk_table", current_odk_form)

  # Execute the CSV download module
  timci::csv_download_server("odk_data_download", current_odk_form)

}
