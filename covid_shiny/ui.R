source("dependencies.R")
source("table_plot_module.R")

shinyUI(fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    # tags$head(includeHTML(("google-analytics.html"))),
    navbarPage("COVID-19 Analysis",
        tabPanel("Introduction",
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
                style = "padding-top:15px"
            ),
            fluidRow(
                div(
                    "NOTE: As of 15 June, confirmed cases include confirmed cases at 
                    UK government regional testing centres. Previous data have not 
                    been updated to account for this hence the significant increase 
                    in testing capacity and positive cases on this day."
                ),
                style = "padding-top:15px"
            ),
        ),
        navbarMenu("National Data",
            panelUI(id = "Testing"),
            panelUI(id = "Hospital Care", 
                message = "NOTE: Please note that as of 22/07/20 suspected
                    COVID-19 patients in hospital and ICU will no 
                    longer be reported"
            ),
            panelUI(id = "Ambulance Attendances",
                message = "NOTE: Please note that as of 22/07/20 this table is 
                        no longer updated by the Scottish Government"
            ),
            panelUI(id = "NHS Calls", 
                message = "NOTE: Please note that as of 22/07/20 this table is 
                        no longer updated by the Scottish Government"
            ),
            panelUI(id = "Delayed Discharges"),
            panelUI(id = "Workforce", 
                message = "NOTE: Please note that as of 22/07/20 this table is 
                        updated on a weekly basis rather than a daily 
                        basis by the Scottish Government"
            ),
            panelUI(id = "Care Home (Cases)", 
                message = "NOTE: Please note that as of 23/07/20 this table 
                        only includes the number of adult care homes with a
                        current suspected case and the proportion of all 
                        adult care homes with a current suspected case.
                        In addition, the Scottish Government will only
                        updated this on a weekly basis"    
            ),
            panelUI(id = "Care Home (Homes)"),
            panelUI(id = "Care Home Workforce"),
            panelUI(id = "Deaths"),
            panelUI(id = "Education")
            
        ), 
        navbarMenu("Regional Data",
            panelUI(id = "Regional Cases"),
            panelUI(id = "Regional ICU"),
            panelUI(id = "Regional Confirmed"),
            panelUI(id = "Regional Suspected"),
            tabPanel(
                "Map",
                leafletOutput("regional_map", height = 700),
                fluidRow(
                    radioButtons(
                        inputId = "regional_mapInput",
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
        ),
        navbarMenu("Council Data",
            panelUI(id = "Council Deaths Per 100,000"),
            panelUI(id = "Council Negative Cases Per 100,000"),
            panelUI(id = "Council Positive Cases Per 100,000"),
            panelUI(id = "Council Cumulative Deaths"),
            panelUI(id = "Council Cumulative Negative"),
            panelUI(id = "Council Cumulative Positive"),
            panelUI(id = "Council Cumuilative Positive Percent"),
            panelUI(id = "Council Daily Deaths"),
            panelUI(id = "Council Daily Positive"),
            tabPanel(
                "Map",
                leafletOutput("council_map", height = 700),
                fluidRow(
                    radioButtons(
                        inputId = "council_mapInput",
                        label = "Select Input",
                        choices = c(
                            "Council Deaths Per 100,000" = "CrudeRateDeaths",
                            "Council Negative Cases Per 100,000" = "CrudeRateNegative",
                            "Council Positive Cases Per 100,000" = "CrudeRatePositive",
                            "Council Cumulative Deaths" = "CumulativeDeaths",
                            "Council Cumulative Negative" = "CumulativeNegative",
                            "Council Cumulative Positive" = "CumulativePositive",
                            "Council Cumuilative Positive Percent" = "CumulativePositivePercent",
                            "Council Daily Deaths" = "DailyDeaths",
                            "Council Daily Positive" = "DailyPositive"
                        ),
                        inline = TRUE
                    )
                )
            )
        ),
        navbarMenu("References",
            HTML("<a href=\"https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/\" style=\"font-size: 15px\"> National and Regional Data </a>"),
            HTML("<a href=\"https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/427f9a25-db22-4014-a3bc-893b68243055\" style=\"font-size: 15px\"> Council Data </a>"),
            HTML("<a href=\"https://github.com/jsleight1/covid19_scotland\" style=\"font-size: 15px\"> Source Code </a>")
        )
    )
))