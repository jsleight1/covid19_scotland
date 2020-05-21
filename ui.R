source("dependencies.R")
shinyUI(fluidPage(
    navbarPage("COVID-19 Analysis",
        tabPanel("Introduction",
            shinyWidgets::setBackgroundColor(color = "66B2FF"),
            fluidRow(
                div(
                    "Welcome the COVID-19 Scotland Analysis tool. 
                    The Scottish Government releases COVID-19 trend data daily split into
                    both National and Regional releases within two separate excel spreadsheets found here:",
                    style = "color:black"
                ),
                a("https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/.", href = "https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/.")
            ),
            fluidRow(
                column(8, align = "center", offset = 2, 
                    fileInput("file1", div("Please upload the 'Trends in daily COVID-19 data' excel spreadsheet here", style = "color:black"),
                        accept = c('text/xlsx')
                    )
                )
            ),
            fluidRow(
                column(8, align = "center", offset = 2, 
                    fileInput("file2", div("Please upload the 'COVID-19 data by NHS Board' excel spreadsheet here", style = "color:black"),
                        accept = c('text/xlsx')
                    )
                )
            ),
            fluidRow(
                column(width = 12, div("National Cumulative Cases", style = "font-size:20px;"), plotlyOutput(outputId = "introduction_plot")),
            ),
            fluidRow(
                div("Daily Updates", style = "font-size:20px;"),
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
                )

            )
        ),
        tabPanel("National Data",
            tabsetPanel(type = "tabs",
            tabPanel(
                h4("NHS 24"),
                tabsetPanel(type = "tabs", 
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "NHS 24")
                    ),
                    tabPanel(
                        h5("Daily NHS Calls"),
                        plotlyOutput(outputId = "nhs_calls", height = "600px")
                    )
                )
            ),
            tabPanel(
                h4("Hospital Care"),
                tabsetPanel(type = "tabs",
                     tabPanel(
                         h5("Table"),
                         DT::dataTableOutput(outputId = "Hospital Care")
                     ), 
                     tabPanel(
                         h5("Daily Increase in Patients in Intensive Care"),
                         plotlyOutput(outputId = "daily_intensive_increase", height = "600px")
                     ),
                     tabPanel(
                         h5("Daily Increase in Patients in Hospital (including intensive care)"),
                         plotlyOutput(outputId = "daily_hospital_increase", height = "600px")
                     ),
                     tabPanel(
                         h5("Total Patients in Hosptials"),
                         plotlyOutput(outputId = "cumulative_hospital", height = "600px")
                     )
                )            
            ),
            tabPanel(
                h4("Ambulance Attendances"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Ambulance Attendances")
                    ),
                    tabPanel(
                        h5("Number of Ambulance Attendances"),
                        plotlyOutput(outputId = "ambulance_plot", height = "600px")
                    )
                )
            ),
            tabPanel(
                h4("Delayed Discharges"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Delayed Discharges")
                    ), 
                    tabPanel(
                        h5("Number of Delayed Discharges"),
                        plotlyOutput(outputId = "discharge", height = "600px")
                    )
                )
            ), 
            tabPanel(
                h4("Testing"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Testing")
                    ),
                    tabPanel(
                        h5("Cumulative Testing"),
                        plotlyOutput(outputId = "cumulative_testing", height = "600px")
                    ),
                    tabPanel(
                        h5("Daily Positive Cases"),
                        plotlyOutput(outputId = "daily_positive_tests", height = "600px")
                    )
                )       
            ), 
            tabPanel(
                h4("Workforce Absences"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Workforce Absences")
                    ),
                    tabPanel(
                        h5("Daily Workforce Absences"),
                        plotlyOutput(outputId = "daily_workforce_absences", height = "600px")
                    )
                )          
            ), 
            tabPanel(
                h4("Adult Care Homes"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Adult Care Homes")
                    ), 
                    tabPanel(
                        h5("Carehome Cases"),
                        plotlyOutput(outputId = "carehome_cases_plot", height = "600px")
                    )
                )  
            ), 
            tabPanel(
                h4("Deaths"),
                tabsetPanel(type = "tabs",
                    tabPanel(
                        h5("Table"),
                        DT::dataTableOutput(outputId = "Deaths")
                    ), 
                    tabPanel(
                         h5("Cumulative Deaths"),
                        plotlyOutput(outputId = "cumulative_deaths", height = "600px")
                    ),
                    tabPanel(
                        h5("Daily Deaths"),
                        plotlyOutput(outputId = "daily_deaths", height = "600px")
                    )      
                )  
            ))
        ), 
        tabPanel("Regional Data",
            tabsetPanel(type = "tabs",
                tabPanel(
                    h4("Tables"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            h5("Regional Cumulative Cases"),
                            DT::dataTableOutput(outputId = "regional_cumulative_cases")
                        ),
                        tabPanel(
                            h5("Regional COVID-19 Inpatients"),
                            DT::dataTableOutput(outputId = "regional_COVID_inpatients")
                        ),
                        tabPanel(
                            h5("Regional Confirmed Hosptial Cases"),
                            DT::dataTableOutput(outputId = "regional_hospital_confirmed")
                        ),
                        tabPanel(
                            h5("Regional Suspected Hosptial Cases"),
                            DT::dataTableOutput(outputId = "regional_hospital_suspected")
                        )
                    )
                ),
                tabPanel(
                    h4("Plots"),
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            h5("Regional Cumulative Cases"),
                            plotlyOutput(outputId = "regional_cumulative_plot", height = "600px")
                        ),
                        tabPanel(
                            h5("Regional COVID-19 Patients in ICU"),
                            plotlyOutput(outputId = "regional_inpatient_plot", height = "600px")
                        ),
                        tabPanel(
                            h5("Regional Confirmed Hospital Cases"),
                            plotlyOutput(outputId = "regional_confirmed_plot", height = "600px")
                        ),
                        tabPanel(
                            h5("Regional Suspected Hospital Cases"),
                            plotlyOutput(outputId = "regional_suspected_plot", height = "600px")
                        )
                    )
                ),
                tabPanel(
                    h4("Map"),
                    leafletOutput("map", height = 600),
                    fluidRow(
                        radioButtons(
                            inputId = "mapInput",
                            label = "Select Input",
                            choices = c(
                                "Regional Cases" = "cases",
                                "Regional Inpatients" = "inpatients",
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