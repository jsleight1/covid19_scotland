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
                style = "padding-top:30px; padding-bottom:30px"

            ),
            fluidRow(
                div("NOTE: As of 15 June, confirmed cases include confirmed cases at UK government regional testing centres, hence why testing plots show a massive increase in testing capacity and positive cases. Additionally, a breakdown by NHS Board is not available. This will hopefully be updated by 18 June.", style = "padding-top:30px; padding-bottom:30px")
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
                        h6("Cumulative Testing"),
                        plotlyOutput(outputId = "cumulative_testing", height = "700px")
                    ),
                    tabPanel(
                        h6("Daily Testing"),
                        plotlyOutput(outputId = "daily_tests", height = "700px")
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
                         h6("Daily Increase in Patients in Intensive Care"),
                         plotlyOutput(outputId = "daily_intensive_increase", height = "700px")
                     ),
                     tabPanel(
                         h6("Daily Increase in Patients in Hospital (including intensive care)"),
                         plotlyOutput(outputId = "daily_hospital_increase", height = "700px")
                     ),
                     tabPanel(
                         h6("Total Patients in Hospitals"),
                         plotlyOutput(outputId = "cumulative_hospital", height = "700px")
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
                        fluidRow(uiOutput("nhs_calls_select")),
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
                        fluidRow(uiOutput("ambulance_select")),
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
                        fluidRow(uiOutput("discharge_select")),
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
                        fluidRow(uiOutput("workforce_absence_select")),
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
                        fluidRow(uiOutput("carehome_cases_select")),
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
                        fluidRow(uiOutput("care_workforce_select")),
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
                        fluidRow(uiOutput("deaths_select")),
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
                            fluidRow(uiOutput("regional_cumulative_select")),
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
                            fluidRow(uiOutput("regional_icu_select")),
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
                            fluidRow(uiOutput("regional_confirmed_select")),
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
                            fluidRow(uiOutput("regional_suspected_select")),
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