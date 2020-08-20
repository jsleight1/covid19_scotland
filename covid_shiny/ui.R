source("dependencies.R")
source("table_plot_module.R")

shinyUI(fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    navbarPage("COVID-19 Analysis",
        tabPanel("Introduction",
            shinyWidgets::setBackgroundColor(color = "66B2FF"),
            fluidRow(
                column(
                    width = 12, div("National Cumulative Cases", style = "font-size:20px;"), 
                    plotlyOutput(outputId = "introduction_plot", height = "500px")
                )
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
                    Previous data have note been updated to account for this hence the significant increase in testing capacity
                    and positive cases on this day.",
                    style = "padding-top:15px; padding-bottom:5px")
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
                panelUI(id = "Testing", text = ""),
                panelUI(id = "Hospital Care", 
                    text = "NOTE: Please note that as of 22/07/20 suspected
                        COVID-19 patients in hospital and ICU will no 
                        longer be reported"
                ),
                panelUI(id = "Ambulance Attendances",
                    text = "NOTE: Please note that as of 22/07/20 this table is 
                            no longer updated by the Scottish Government"
                ),
                panelUI(id = "NHS Calls", 
                    text = "NOTE: Please note that as of 22/07/20 this table is 
                            no longer updated by the Scottish Government"
                ),
                panelUI(id = "Delayed Discharges", text = ""),
                panelUI(id = "Workforce", 
                    text = "NOTE: Please note that as of 22/07/20 this table is 
                            updated on a weekly basis rather than a daily 
                            basis by the Scottish Government"
                ),
                panelUI(id = "Care Homes", 
                    text = "NOTE: Please note that as of 23/07/20 this table 
                            only includes the number of adult care homes with a
                            current suspected case and the proportion of all 
                            adult care homes with a current suspected case.
                            In addition, the Scottish Government will only
                            updated this on a weekly basis"    
                ),
                panelUI(id = "Care Home Workforce", text = ""),
                panelUI(id = "Deaths", text = "")
            )
        ), 
        tabPanel("Regional Data",
           tabsetPanel(type = "tabs",
                panelUI(id = "Regional Cases",  text = ""),
                panelUI(id = "Regional ICU",  text = ""),
                panelUI(id = "Regional Confirmed",  text = ""),
                panelUI(id = "Regional Suspected",  text = ""),
                tabPanel(
                h4("Map"),
                    leafletOutput("map", height = 700),
                    fluidRow(
                        radioButtons(
                            inputId = "mapInput",
                            label = "Select Input",
                            choices = c(
                                "Regional Cases" = "Table 1 - Cumulative cases",
                                "Regional ICU Patients" = "Table 2a - ICU patients",
                                "Regional Hospital Confirmed" = "Table 3a - Hospital Confirmed",
                                "Regional Hospital Suspected" = "Table 3b- Hospital Suspected"
                            ),
                            inline = TRUE
                        )
                    )
                )
            )
        )
    )
))