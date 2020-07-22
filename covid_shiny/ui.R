source("dependencies.R")
shinyUI(fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    navbarPage("COVID-19 Analysis",
        tabPanel("Introduction",
            shinyWidgets::setBackgroundColor(color = "66B2FF"),
            fluidRow(
                column(width = 12, div("National Cumulative Cases", style = "font-size:20px;"), plotlyOutput(outputId = "introduction_plot", height = "500px"))
            ),
            fluidRow(
                column(width = 1, div("Date: ", style = "font-size:20px;")),
                column(
                    width = 2, 
                    div(textOutput(outputId = "introduction_date"), style = "font-size:20px; color:red")
                ),
                column(width = 1, div("Total Cases: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = "introduction_cases"), style = "font-size:20px; color:red")
                ),
                column(width = 1, div("Cases Today: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = "introduction_daily_cases"), style = "font-size:20px; color:red")
                ),
                column(width = 1, div("Total Deaths: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = "introduction_deaths"), style = "font-size:20px; color:red")
                ),
                column(width = 1, div("Deaths Today: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = "introduction_daily_deaths"), style = "font-size:20px; color:red")
                ),
                style = "padding-top:15px; padding-bottom:15px"

            ),
            fluidRow(
                div(
                    "NOTE: As of 15 June, confirmed cases include confirmed cases at UK government regional testing centres.
                    Previous data have not been updated to account for this hence the significant increase in testing capacity
                    and positive cases on this day.",
                    style = "padding-top:15px; padding-bottom:5px")
            ),
            fluidRow(
                div(
                    "NOTE: Please note that suspected COVID-19 patients in hospital 
                    and suspected Covid-19 patients in ICU will no longer be published from 22 July",
                    style = "padding-top:5px; padding-bottom:15px")
            ),
            fluidRow(
                div("Reference:"),
                a(
                    "https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/", 
                    href = "https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/",
                    style = "color:blue; font-size:15px"
                )
            )
        ),
        tabPanel("National Data",
            tabsetPanel(type = "tabs",
            tabPanel(
                h5("Testing"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Testing")
                    ),
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("tests_select"), width = 8),
                            column(uiOutput("tests_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "tests_plot", height = "700px")
                    )  
                )     
            ), 
            tabPanel(
                h5("Hospital Care"),
                tabsetPanel(type = "tabs",
                     tabPanel(
                         h6("Table"),
                         DT::dataTableOutput(outputId = "Hospital Care")
                     ), 
                     tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("hospital_select"), width = 8),
                            column(uiOutput("hospital_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "hospital_plot", height = "700px")
                    )
                )            
            ),
            tabPanel(
                h5("NHS 24"),
                tabsetPanel(type = "tabs", 
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "NHS 24")
                    ),
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("nhs_calls_select"), width = 8), 
                            column(uiOutput("nhs_calls_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "nhs_calls_plot", height = "700px")
                    )   
                )
            ),
            tabPanel(
                h5("Ambulance Attendances"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Ambulance Attendances")
                    ),
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("ambulance_select"), width = 8),
                            column(uiOutput("ambulance_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "ambulance_plot", height = "700px")
                    )
                )
            ),
            tabPanel(
                h5("Delayed Discharges"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Delayed Discharges")
                    ), 
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("discharge_select"), width = 8),
                            column(uiOutput("discharge_radio_select"), width = 4)

                        ),
                        plotlyOutput(outputId = "discharge_plot", height = "700px")
                    )
                )
            ),
            tabPanel(
                h5("Workforce Absences"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Workforce Absences")
                    ),
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("workforce_absence_select"), width = 8),
                            column(uiOutput("workforce_absence_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "workforce_absence_plot", height = "700px")
                    )
                )          
            ), 
            tabPanel(
                h5("Adult Care Homes"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Adult Care Homes")
                    ), 
                    tabPanel(
                        h6("Plot"), 
                        fluidRow(
                            column(uiOutput("carehome_cases_select"), width = 8),
                            column(uiOutput("casehome_cases_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "carehome_cases_plot", height = "700px")
                    )
                )  
            ),
            tabPanel(
                h5("Care Home Workforce"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Care Home Workforce")
                    ),
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("care_workforce_select"), width = 8),
                            column(uiOutput("care_workforce_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "care_workforce_plot", height = "700px")
                    )
                )  
            ),
            tabPanel(
                h5("Deaths"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h6("Table"),
                        DT::dataTableOutput(outputId = "Deaths")
                    ), 
                    tabPanel(
                        h6("Plot"),
                        fluidRow(
                            column(uiOutput("deaths_select"), width = 8),
                            column(uiOutput("deaths_radio_select"), width = 4)
                        ),
                        plotlyOutput(outputId = "deaths_plot", height = "700px")
                    )    
                )  
            ))
        ), 
        tabPanel("Regional Data",
           tabsetPanel(type = "tabs",
                tabPanel(
                    h5("Regional Cumulative Cases"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            h6("Table"),
                            DT::dataTableOutput(outputId = "regional_cumulative_cases")
                        ),
                        tabPanel(
                            h6("Plot"),
                            fluidRow(
                                column(uiOutput("regional_cumulative_select"), width = 8),
                                column(uiOutput("regional_cumulative_radio_select"), width = 4)
                            ),
                            plotlyOutput(outputId = "regional_cumulative_plot", height = "700px")
                        )    
                    )
                ),
                tabPanel(
                    h5("Regional COVID-19 Patients in ICU"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            h6("Table"),
                            DT::dataTableOutput(outputId = "regional_ICU")
                        ),
                        tabPanel(
                            h6("Plot"),
                            fluidRow(
                                column(uiOutput("regional_icu_select"), width = 8),
                                column(uiOutput("regional_icu_radio_select"), width = 4)
                            ),
                            plotlyOutput(outputId = "regional_icu_plot", height = "700px")
                        )                
                    )
                ),
                tabPanel(
                    h5("Regional Confirmed Hospital Cases"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            h6("Table"),
                            DT::dataTableOutput(outputId = "regional_hospital_confirmed")
                        ),
                        tabPanel(
                            h6("Plot"),
                            fluidRow(
                                column(uiOutput("regional_confirmed_select"), width = 8),
                                column(uiOutput("regional_confirmed_radio_select"), width = 4)
                            ),
                            plotlyOutput(outputId = "regional_confirmed_plot", height = "700px")
                        )
                    )
                ),
                tabPanel(
                    h5("Regional Suspected Hospital Cases"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                        h6("Table"),
                            DT::dataTableOutput(outputId = "regional_hospital_suspected")
                        ),
                        tabPanel(
                            h6("Plot"),
                            fluidRow(
                                column(uiOutput("regional_suspected_select"), width = 8),
                                column(uiOutput("regional_suspected_radio_select"), width = 4)
                            ),
                            plotlyOutput(outputId = "regional_suspected_plot", height = "700px")
                        )
                    )
                ),
                tabPanel(
                h4("Map"),
                    leafletOutput("map", height = 700),
                    fluidRow(
                        radioButtons(
                            inputId = "mapInput",
                            label = "Select Input",
                            choices = c(
                                "Regional Cases" = "cases",
                                "Regional ICU Patients" = "icu",
                                "Regional Hospital Confirmed" = "regional_confirmed",
                                "Regional Hospital Suspected" = "regional_suspected"
                            ),
                            inline = TRUE
                        )
                    )
                )
            )
        )
    )
))