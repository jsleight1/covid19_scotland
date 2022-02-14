source("dependencies.R")
source("server_functions.R")
source("panel.R")
source("plot.R")
source("map.R")
source("intro.R")
source("dropbox_download.R")

shinyServer(function(input, output) {
    
    # Introduction
    introServer(
        id = "Introduction", 
        data = regional_data[["Table 1 - Cumulative cases"]],
        date = as.character(last(pull(national_data[["Table 8 - Deaths"]], Date))), 
        cases = last(pull(regional_data[["Table 1 - Cumulative cases"]], Scotland)), 
        daily_cases = last(pull(national_data[["Table 5b - Testing (PCR)"]], `Daily Positive`)),
        deaths = last(pull(select(national_data[["Table 8 - Deaths"]], 
            `Number of COVID-19 confirmed deaths registered to date`))),
        daily_deaths = last(pull(national_data[["Table 8 - Deaths"]], `Daily Deaths`))
    )
    
    # National analysis
    panelServer(
        id = "Testing - PCR", 
        table = national_data[["Table 5b - Testing (PCR)"]]
    )
    panelServer(
        id = "Vaccinations", 
        table = national_data[["Table 10a - Vaccinations"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Vaccintation supply", 
        table = national_data[["Table 11 - Vac supply"]]
    )
    panelServer(
        id = "Vaccinations per JCVI group", 
        table = national_data[["Table 10b - Vac by JCVI group"]], 
        roll_ave = FALSE
    ) 
    panelServer(
        id = "Vacciations per age group", 
        table = national_data[["Table 10c - Vac by age"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Hospital Care", 
        table = national_data[["Table 2 - Hospital Care"]]
    )
    panelServer(
        id = "Delayed Discharges", 
        table = national_data[["Table 4 - Delayed Discharges"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Workforce", 
        table = national_data[["Table 6 - Workforce"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Care Home - Cases", 
        table = national_data[["Table 7a - Care Homes"]], 
        x = "Week", 
        roll_ave = FALSE
    )
    panelServer(
        id = "Care Home - Homes", 
        table = national_data[["Table 7c - Care Homes (Homes)"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Care Home Workforce", 
        table = national_data[["Table 7b - Care Home Workforce"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Deaths", 
        table = national_data[["Table 8 - Deaths"]]
    )

    # Regional analysis
    panelServer(
        id = "Regional Cases", 
        table = regional_data[["Table 1 - Cumulative cases"]]
    )
    panelServer(
        id = "Regional ICU", 
        table = regional_data[["Table 2 - ICU patients"]]
    )
    panelServer(
        id = "Regional Hospital", 
        table = regional_data[["Table 3 - Hospital patients"]]
    )
    mapServer(id = "regional_map", data = regional_data, json = region_json)

    # Council analysis
    panelServer(
        id = "Council Deaths Per 100000", 
        table = council_data[["CrudeRateDeaths"]]
    )
    panelServer(
        id = "Council Negative Cases Per 100000", 
        table = council_data[["CrudeRateNegative"]]
    )
    panelServer(
        id = "Council Positive Cases Per 100000", 
        table = council_data[["CrudeRatePositive"]]
    )
    panelServer(
        id = "Council Cumulative Deaths", 
        table = council_data[["CumulativeDeaths"]]
    )
    panelServer(
        id = "Council Cumulative Negative", 
        table = council_data[["CumulativeNegative"]]
    )
    panelServer(
        id = "Council Cumulative Positive", 
        table = council_data[["CumulativePositive"]]
    )
    panelServer(
        id = "Council Daily Positive", 
        table = council_data[["DailyPositive"]]
    )
    panelServer(
        id = "Council Daily Deaths", 
        table = council_data[["DailyDeaths"]]
    )
    panelServer(
        id = "Council 7 day Positive Per 100000", 
        table = council_data[["CrudeRate7DayPositive"]], 
        roll_ave = FALSE
    )
    panelServer(
        id = "Council 7 day Positve Percentage", 
        table = council_data[["PositivePercentage7Day"]], 
        roll_ave = FALSE
    )
    mapServer(id = "council_map", data = council_data, json = council_json)
})
