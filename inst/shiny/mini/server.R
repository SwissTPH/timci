# Defines the server logic for the minimalist Shiny web application
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
    tz = Sys.getenv("TZ"),
    verbose = FALSE # Can be switched to TRUE for demo or debugging
  )

  ################
  # RAW ODK DATA #
  ################

  # Load ODK forms that have at least 1 submission
  odk_form_list <- ruODK::form_list()
  valid_odk_forms <- subset(odk_form_list, submissions > 0, select = c(fid))
  valid_odk_form_list <- valid_odk_forms$fid

  # Load ODK data
  odk_data <- hash::hash()
  for (form in valid_odk_form_list) {
    odk_data[[form]] <- ruODK::odata_submission_get(fid = form)
  }

  # Execute the form selection module
  current_odk_form <- timci::select_list_item_server("select_odk_form",
                                                     valid_odk_form_list,
                                                     odk_data)

  # Execute the info module about the raw ODK data
  timci::reactive_odk_data_info_server("raw_odk_info", current_odk_form)

  # Execute the table module that displays the raw ODK data
  timci::reactive_data_table_server("raw_odk_table", current_odk_form)

  # Execute the CSV and Excel download modules
  timci::reactive_csv_download_server("raw_odk_csv_export", current_odk_form)
  timci::reactive_xlsx_download_server("raw_odk_xlsx_export", current_odk_form)

}
