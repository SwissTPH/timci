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
                                    timci::odk_data_table_ui("tab2")),
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
                    shiny::tabPanel("De-identified research data",
                                    timci::research_data_table_ui("tab3")),
                    shiny::tabPanel("Enrolment",
                                    timci::enrolment_tab_ui("enrolment"))
                    ),
  )
