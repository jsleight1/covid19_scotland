source("dependencies.R")
source("panel.R")
source("map.R")
source("intro.R")

shinyUI(fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    tags$head(includeHTML(("google-analytics.html"))),
    navbarPage("COVID-19 Analysis",
        introUI(id = "Introduction"),
        navbarMenu("National Data",
            panelUI(id = "Testing"),
            panelUI(id = "Hospital Care", 
                message = "NOTE: Please note that as of 15/09/20 this table
                           only includes patients who first tested positive during
                           their current stay in hospital or in the two weeks 
                           before their admission. Refer to reference link for 
                           further details."
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
            panelUI(id = "Regional ICU", 
                message = "NOTE: Please note that as of 15/09/20 this table
                           only includes patients who first tested positive during
                           their current stay in hospital or in the two weeks 
                           before their admission. For disclosure reasons blanks
                           values mean there were fewer than 5 patients. Refer to 
                           reference link for further details."
            ),
            panelUI(id = "Regional Hospital",
                message = "NOTE: Please note that as of 15/09/20 this table
                           only includes patients who first tested positive during
                           their current stay in hospital or in the two weeks 
                           before their admission. For disclosure reasons blanks
                           values mean there were fewer than 5 patients. Refer to 
                           reference link for further details."
            ),
            mapUI(id = "regional_map", 
                choices = c(
                    "Regional Cases" = "Table 1 - Cumulative cases",
                    "Regional ICU Patients" = "Table 2 - ICU patients",
                    "Regional Hospital Patients" = "Table 3 - Hospital patients"
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
            panelUI(id = "Council Percent Positive"),
            mapUI(id = "council_map",
                choices = c(
                    "Council Deaths Per 100,000" = "CrudeRateDeaths",
                    "Council Negative Cases Per 100,000" = "CrudeRateNegative",
                    "Council Positive Cases Per 100,000" = "CrudeRatePositive",
                    "Council Cumulative Deaths" = "CumulativeDeaths",
                    "Council Cumulative Negative" = "CumulativeNegative",
                    "Council Cumulative Positive" = "CumulativePositive",
                    "Council Percent Positive" = "CumulativePositivePercent"
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