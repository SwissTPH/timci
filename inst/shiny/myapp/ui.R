# Defines the UI for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# Define UI
ui <- shiny::fluidPage(

  shiny::navbarPage("TIMCI Dashboard", id="nav",

                    shiny::navbarMenu("Data",

                                      # Raw ODK data panel (will be hidden / access restricted)
                                      shiny::tabPanel("ODK data",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Help text
                                                          shiny::helpText("ODK form selection"),

                                                          # Display the list item selection module
                                                          timci::select_list_item_ui("select_odk_form"),

                                                          # Info text
                                                          shiny::p(shiny::strong("Database information")),

                                                          # Study data info
                                                          timci::odk_data_info_ui("raw_odk_info")),

                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            timci::data_table_ui("raw_odk_table"))))
                                                      ),

                                      # De-identified study data panel
                                      shiny::tabPanel("De-identified study data",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Help text
                                                          shiny::helpText("Study selection"),

                                                          # Display the list item selection module
                                                          timci::select_list_item_ui("select_study"),

                                                          # Info text
                                                          shiny::p(shiny::strong("Database information")),

                                                          # Study data info
                                                          timci::odk_data_info_ui("deidentified_study_info"),

                                                          # Info text
                                                          shiny::p(""),
                                                          shiny::p(shiny::strong("Export data")),

                                                          # Export study data in *.csv format
                                                          timci::csv_download_ui("deidentified_study_csv_export"),

                                                          # Export study data in *.xlsx format
                                                          timci::xlsx_download_ui("deidentified_study_xlsx_export")),

                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            timci::data_table_ui("deidentified_study_table"))))
                                                      )
                                      ),

                    shiny::navbarMenu("RCT / LS",

                                      shiny::tabPanel("Enrolment rate"),

                                      shiny::tabPanel("Inclusion / exclusion",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Help text
                                                          shiny::helpText("Monitoring")),

                                                        # Display a pie graph
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            shiny::column(6, timci::pie_chart_ui("enrolled")),
                                                            shiny::column(6, timci::pie_chart_ui("inclusion_exclusion")))))

                                                      ),

                                      # Day 7 follow-up data panel
                                      shiny::tabPanel("Day 7 follow-up",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Info text
                                                          shiny::p(shiny::strong("Database information")),

                                                          # Study data info
                                                          timci::odk_data_info_ui("day7fu_info")),

                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            timci::data_table_ui("day7fu_table"))))

                                                      ),

                                      # Day 28 follow-up data panel
                                      shiny::tabPanel("Day 28 follow-up",

                                                      # Sidebar layout with input and output definitions
                                                      shiny::sidebarLayout(

                                                        # Sidebar panel for inputs
                                                        shiny::sidebarPanel(

                                                          # Info text
                                                          shiny::p(shiny::strong("Database information")),

                                                          # Study data info
                                                          timci::odk_data_info_ui("day28fu_info")),

                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(
                                                            timci::data_table_ui("day28fu_table"))))

                                      )

                                      )
                    )
  )
