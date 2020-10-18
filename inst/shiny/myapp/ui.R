# Defines the UI for the TIMCI Shiny web application
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# Define UI
ui <- shiny::fluidPage(

  shiny::navbarPage("TIMCI Dashboard", id="nav",

                    shiny::navbarMenu("Data",

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
                                                          timci::odk_data_info_ui("raw_odk_info"),

                                                          # Info text
                                                          shiny::p(""),
                                                          shiny::p(shiny::strong("Export data")),

                                                          # Export CSV data
                                                          timci::csv_download_ui("odk_data_download")

                                                          ),

                                                        # Display the table module
                                                        shiny::mainPanel(

                                                          shiny::fluidRow(

                                                            shiny::column(12, timci::odk_data_table_ui("raw_odk_table")))

                                                          )
                                                        )
                                                      )

                                      )

                    )
  )
