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

  # Load ODK forms that have at least 1 submission
  odk_form_list <- ruODK::form_list()
  valid_odk_forms <- subset(odk_form_list, submissions > 0, select=c(fid))
  valid_odk_form_list <- valid_odk_forms$fid

  # Load ODK data
  odk_data <- hash::hash()
  for(form in valid_odk_form_list){
    current_odk_data <- ruODK::odata_submission_get(fid = form)
    # Process ODK data (this will depend on the form - here targeting RCT / LS main form)
    print(str(current_odk_data))
    odk_data[[form]] <- format_odk_data(current_odk_data)
  }

  # Execute the form selection module
  current_odk_form <- timci::select_list_item_server("select_odk_form",
                                                     valid_odk_form_list,
                                                     odk_data)

  # Execute the info module about the raw ODK data
  timci::reactive_odk_data_info_server("raw_odk_info", current_odk_form)

  # Execute the table module that displays the raw ODK data
  timci::reactive_data_table_server("raw_odk_table", current_odk_form)

  #############
  # ENROLMENT #
  #############

  # Extract data from enrolled participants only
  n_screened <- nrow(odk_data[["01-TIMCI-CRF-Facility"]])
  study_data <- extract_enrolled_participants(odk_data[["01-TIMCI-CRF-Facility"]])
  n_enrolled <- nrow(study_data)

  # Execute the pie chart module to plot enrolment vs. non-enrolment
  timci::pie_chart_server("enrolled",
                          data.frame(group=c("Enrolled", "Not enrolled"), value= c(n_enrolled, n_screened-n_enrolled)))

  # Execute the pie chart module to plot global causes of non-enrolment
  plot1 <- timci::pie_chart_server("inclusion_exclusion",
                                   count_screening(odk_data[["01-TIMCI-CRF-Facility"]]))

  # Execute the PNG download modules
  timci::report_download_server("report")

  #######################################
  # PERSONALLY IDENTIFIABLE INFORMATION #
  #######################################

  pii <- extract_pii(study_data)

  ############################
  # DE-IDENTIFIED STUDY DATA #
  ############################

  study_list = c("Pragmatic cluster randomised controlled trial",
                 "Observational longitudinal study",
                 "Service Provision Assessment")

  # De-identify ODK data
  research_data <- hash()
  for(form in study_list){
    if(form == "Pragmatic cluster randomised controlled trial"){
      research_data[[form]] <- deidentify_data(study_data)
    }
    else if(form == "Observational longitudinal study"){
      research_data[[form]] <- deidentify_data(study_data)
    }
    else{
      research_data[[form]] <- study_data
    }
  }

  # Execute the form selection module
  current_study <- timci::select_list_item_server("select_study",
                                                  study_list,
                                                  research_data)

  # Execute the info module
  timci::reactive_odk_data_info_server("deidentified_study_info", current_study)

  # Execute the table module
  timci::reactive_data_table_server("deidentified_study_table", current_study)

  # Execute the CSV and Excel download modules
  timci::reactive_csv_download_server("deidentified_study_csv_export", current_study)
  timci::reactive_xlsx_download_server("deidentified_study_xlsx_export", current_study)

  #####################
  #####################
  ###               ###
  ###   FOLLOW-UP   ###
  ###               ###
  #####################
  #####################

  ###################
  # Day 7 follow-up #
  ###################

  day7fu <- generate_fu_log(study_data, 7, 9)

  # Execute the info module
  timci::odk_data_info_server("day7fu_info", day7fu)

  # Execute the table module
  timci::data_table_server("day7fu_table", day7fu)

  # Execute the CSV and Excel download modules
  timci::csv_download_server("day7fu_csv_export", "day7fu", day7fu)
  timci::xlsx_download_server("day7fu_xlsx_export", "day7fu", day7fu)

  ############################
  # Referral level follow-up #
  ############################

  referralfu <- generate_fu_log(study_data, 7, 9) # To modify when follow-up data entered

  # Execute the info module
  timci::odk_data_info_server("referralfu_info", referralfu)

  # Execute the table module
  timci::data_table_server("referralfu_table", referralfu)

  # Execute the CSV and Excel download modules
  timci::csv_download_server("referralfu_csv_export", "referralfu", referralfu)
  timci::xlsx_download_server("referralfu_xlsx_export", "referralfu", referralfu)

  ####################
  # Day 28 follow-up #
  ####################

  day28fu <- generate_fu_log(study_data, 28, 32)

  # Execute the info module
  timci::odk_data_info_server("day28fu_info", day28fu)

  # Execute the table module
  timci::data_table_server("day28fu_table", day28fu)

  # Execute the CSV and Excel download modules
  timci::csv_download_server("day28fu_csv_export", "day28fu", day28fu)
  timci::xlsx_download_server("day28fu_xlsx_export", "day28fu", day28fu)

  ###############################
  ###############################
  ###                         ###
  ###   QUALITATIVE STUDIES   ###
  ###                         ###
  ###############################
  ###############################

  cg_log <- generate_cg_log(study_data)

  # Execute the info module
  timci::odk_data_info_server("cg_info", cg_log)

  # Execute the table module
  timci::data_table_server("cg_table", cg_log)

  # Execute the CSV and Excel download modules
  timci::csv_download_server("cg_csv_export", "caregiver-selection", cg_log)
  timci::xlsx_download_server("cg_xlsx_export", "caregiver-selection", cg_log)

}
