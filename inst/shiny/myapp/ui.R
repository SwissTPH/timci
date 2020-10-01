# Defines the UI for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# Define UI
ui <- shiny::fluidPage(

  shiny::navbarPage("TIMCI Dashboard", id="nav",

                    shiny::tabPanel("Dummy data",
                                    timci::dummy_data_table_ui("tab1")),

                    shiny::tabPanel("ODK data",

                                    # Sidebar layout with input and output definitions
                                    shiny::sidebarLayout(

                                      # Sidebar panel for inputs
                                      shiny::sidebarPanel(

                                        # Help text
                                        shiny::helpText("Select the form you would like to visualise."),

                                        # Display the list item selection module
                                        timci::select_list_item_ui("select_odk_form"),

                                        # Info text
                                        shiny::p(shiny::strong("Database information")),

                                        # Study data info
                                        timci::odk_data_info_ui("raw_odk_info")

                                      ),

                                      # Display the table module
                                      shiny::mainPanel(
                                        shiny::fluidRow(
                                          shiny::column(12, timci::odk_data_table_ui("raw_odk_table"))
                                        )
                                      )
                                    )),

                    shiny::tabPanel("Data Management",
                                    shiny::fluidRow(
                                      shiny::h2("Quality checks")),
                                    shiny::fluidRow(
                                      shiny::p("Number of queries raised"),
                                      shiny::p("Number of queries solved"),
                                      shiny::p("Number of running queries"),
                                      shiny::p("Time to resolution")),
                                    shiny::fluidRow(
                                      shiny::h2("Data reconciliation")),
                                    shiny::fluidRow(
                                      shiny::h2("Data de-identification")),
                                    shiny::fluidRow(
                                      shiny::p("Reconcile facility and follow-up data"),
                                      shiny::p("Reconcile facility and hospital data"),
                                      shiny::p("Reconcile research and CDSA data"),
                                      shiny::p("Reconcile main study and mixed-method study data")),
                                    shiny::fluidRow(
                                      shiny::p("Names"),
                                      shiny::p("Phone numbers"),
                                      shiny::p("Residence"),
                                      shiny::p("Dates"))
                                    ),

                    shiny::tabPanel("De-identified study data",
                                    # Sidebar layout with input and output definitions
                                    shiny::sidebarLayout(

                                      # Sidebar panel for inputs
                                      shiny::sidebarPanel(

                                        # Help text
                                        shiny::helpText("Select the form you would like to visualise."),

                                        # Display the list item selection module
                                        timci::select_list_item_ui("select_study"),

                                        # Info text
                                        shiny::p(shiny::strong("Database information")),

                                        # Study data info
                                        timci::odk_data_info_ui("deidentified_study_info"),

                                        # Download study data in *.csv format
                                        timci::csv_download_ui("deidentified_study_download")

                                      ),

                                      # Display the table module
                                      shiny::mainPanel(
                                        shiny::fluidRow(
                                          shiny::column(12, timci::odk_data_table_ui("deidentified_study_table"))
                                        )
                                      )
                                    )),

                    shiny::tabPanel("Enrolment",
                                    shiny::column(
                                      6,
                                      timci::enrolment_tab_ui("enrolment"))
                                    ),

                    shiny::tabPanel("Day 7 follow-up",
                                    timci::qr_code_image_ui("qrcode"))

                    )
  )
