# Defines the server logic for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

library(ruODK)
library(hash)
library(qrcode)

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
  odk_form_vlist <- subset(odk_form_list, submissions > 0, select=c(fid))

  # Load ODK data
  odk_data <- hash()
  for(form in odk_form_vlist){
    current_odk_data <- ruODK::odata_submission_get(fid = form)
    # Process ODK data (this will depend on the form - here targeting RCT / LS main form)
    odk_data[[form]] <- format_odk_data(current_odk_data)
  }

  # Execute the form selection module
  current_odk_form <- callModule(timci::select_list_item_server,
                          "select_odk_form",
                          odk_form_vlist$fid,
                          odk_data)

  # Execute the info module
  callModule(timci::odk_data_info_server,
             "raw_odk_info",
             current_odk_form)

  # Execute the table module
  res1 <- callModule(timci::odk_data_table_server,
                    "raw_odk_table",
                    current_odk_form)

  ############################
  # DE-IDENTIFIED STUDY DATA #
  ############################

  # De-identify ODK data
  research_data <- hash()
  for(form in odk_form_vlist){
    research_data[[form]] <- deidentify_data(odk_data[[form]])
  }

  # Execute the form selection module
  current_study <- callModule(timci::select_list_item_server,
                          "select_study",
                          odk_form_vlist$fid,
                          research_data)

  # Execute the info module
  callModule(timci::odk_data_info_server,
             "deidentified_study_info",
             current_study)

  # Execute the table module
  callModule(timci::odk_data_table_server,
             "deidentified_study_table",
             current_study)

  # Execute the download module
  callModule(timci::csv_download_server,
             "deidentified_study_download",
             current_study)

  ##############
  # DUMMY DATA #
  ##############

  # Load dummy data
  dummy_tuple <- timci::load_dummy_data()
  dummy_data <- dummy_tuple[['data']]

  # Display dummy data in a table
  timci::dummy_data_table_server("tab1", dummy_data[["Arm I CDSA POX"]])

  # Display enrolment gauge on dummy data
  timci::enrolment_tab_server("enrolment", dummy_tuple[['df']])

  #############
  # FOLLOW-UP #
  #############

  # Display the follow-up log
  outfile <- file.path(tempdir(), "qrplot.png")
  png(outfile)
  qrcode_gen("A-F01-P00025")
  dev.off()
  timci::qr_code_image_server("qrcode", outfile)

}
