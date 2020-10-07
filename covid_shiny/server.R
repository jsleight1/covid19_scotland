source("dependencies.R")
source("server_functions.R")
source("table_plot_module.R")
source("map_module.R")
source("dropbox_download.R")

shinyServer(function(input, output) {
    
    # Introduction
    output[["introduction_plot"]] <- renderPlotly({
        daily_barplot(
            regional_data[["Table 1 - Cumulative cases"]], 
            x = "Date", 
            y = "Scotland",
            roll_ave = TRUE
        )
    })
    output[["introduction_date"]] <- renderText({
        as.character(last(pull(national_data[["Table 8 - Deaths"]], Date)))
    })
    output[["introduction_cases"]] <- renderText({
        last(pull(regional_data[["Table 1 - Cumulative cases"]], Scotland))
    })
    output[["introduction_daily_cases"]] <- renderText({
        last(pull(national_data[["Table 5 - Testing"]], `Daily Positive`))
    })
    output[["introduction_deaths"]] <- renderText({
        last(pull(select(national_data[["Table 8 - Deaths"]], 
            `Number of COVID-19 confirmed deaths registered to date`)))
    })
    output[["introduction_daily_deaths"]] <- renderText({
        last(pull(national_data[["Table 8 - Deaths"]], `Daily Deaths`))
    })

    # National analysis
    panelServer(id = "Testing", table = national_data[["Table 5 - Testing"]])
    panelServer(id = "Hospital Care", table = national_data[["Table 2 - Hospital Care"]])
    panelServer(id = "Ambulance Attendances", table = national_data[["Table 3 - Ambulance"]])
    panelServer(id = "NHS Calls", table = national_data[["Table 1 - NHS 24"]])
    panelServer(id = "Delayed Discharges", table = national_data[["Table 4 - Delayed Discharges"]], roll_ave = FALSE)
    panelServer(id = "Workforce", table = national_data[["Table 6 - Workforce"]], roll_ave = FALSE)
    panelServer(id = "Care Home (Cases)", table = national_data[["Table 7a - Care Homes"]], x = "Week", roll_ave = FALSE)
    panelServer(id = "Care Home (Homes)", table = national_data[["Table 7c - Care Homes (Homes)"]], roll_ave = FALSE)
    panelServer(id = "Care Home Workforce", table = national_data[["Table 7b - Care Home Workforce"]], roll_ave = FALSE)
    panelServer(id = "Deaths", table = national_data[["Table 8 - Deaths"]])
    panelServer(id = "Education", table = national_data[["Table 9 - School education"]], roll_ave = FALSE)

    # Regional analysis
    panelServer(id = "Regional Cases", table = regional_data[["Table 1 - Cumulative cases"]])
    panelServer(id = "Regional ICU", table = regional_data[["Table 2 - ICU patients"]])
    panelServer(id = "Regional Hospital", table = regional_data[["Table 3 - Hospital patients"]])
    mapServer(id = "regional_map", data = regional_data, json = region_json)

    # Council analysis
    panelServer(id = "Council Deaths Per 100,000", table = council_data[["CrudeRateDeaths"]])
    panelServer(id = "Council Negative Cases Per 100,000", table = council_data[["CrudeRateNegative"]])
    panelServer(id = "Council Positive Cases Per 100,000", table = council_data[["CrudeRatePositive"]])
    panelServer(id = "Council Cumulative Deaths", table = council_data[["CumulativeDeaths"]])
    panelServer(id = "Council Cumulative Negative", table = council_data[["CumulativeNegative"]])
    panelServer(id = "Council Cumulative Positive", table = council_data[["CumulativePositive"]])
    panelServer(id = "Council Percent Positive", table = council_data[["CumulativePositivePercent"]])
    mapServer(id = "council_map", data = council_data, json = council_json)
})
