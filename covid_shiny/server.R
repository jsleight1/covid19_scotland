source("dependencies.R")
source("server_functions.R")
source("table_plot_module.R")
source("data_download.R")

shinyServer(function(input, output) {
    
    # Introduction
    output[["introduction_plot"]] <- renderPlotly({
        daily_barplot(regional_data[["Table 1 - Cumulative cases"]], x = "Date", y = "Scotland")
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
    panelServer(id = "Delayed Discharges", table = national_data[["Table 4 - Delayed Discharges"]])
    panelServer(id = "Workforce", table = national_data[["Table 6 - Workforce"]])
    panelServer(id = "Care Home (Cases)", table = national_data[["Table 7a - Care Homes"]], x = "Week")
    panelServer(id = "Care Home (Homes)", table = national_data[["Table 7c - Care Homes (Homes)"]])
    panelServer(id = "Care Home Workforce", table = national_data[["Table 7b - Care Home Workforce"]])
    panelServer(id = "Deaths", table = national_data[["Table 8 - Deaths"]])
    panelServer(id = "Education", table = national_data[["Table 9 - School education"]])

    # Regional analysis
    panelServer(id = "Regional Cases", table = regional_data[["Table 1 - Cumulative cases"]])
    panelServer(id = "Regional ICU", table = regional_data[["Table 2a - ICU patients"]])
    panelServer(id = "Regional Confirmed", table = regional_data[["Table 3a - Hospital Confirmed"]])
    panelServer(id = "Regional Suspected", table = regional_data[["Table 3b- Hospital Suspected"]])
    output[["regional_map"]] <- renderLeaflet({
        df <- tail(regional_data[[input[["regional_mapInput"]]]], 1) %>% 
            pivot_longer(-Date) %>%
            inner_join(., readRDS("regions_scotland.RDS"), by = "name") %>% 
            mutate(Circle_size = scales::rescale(value, to = c(2000, 18000)))
        leaflet(df) %>% 
            addTiles(options = providerTileOptions(minZoom = 5, maxZoom = 9)) %>% 
            setView(lat = 57.4907, lng = -4.2026, zoom = 6) %>% 
            addCircles(lat = ~latitude, lng = ~longitude, radius = ~Circle_size,
                popup = paste(
                    "Date:", df[["Date"]], "<br>", 
                    "Regional Board:", df[["name"]], "<br>", 
                    "Value:", df[["value"]], "<br>"
                )
            ) 
    })

    # Council analysis
    panelServer(id = "Council Deaths Per 100,000", table = council_data[["CrudeRateDeaths"]])
    panelServer(id = "Council Negative Cases Per 100,000", table = council_data[["CrudeRateNegative"]])
    panelServer(id = "Council Positive Cases Per 100,000", table = council_data[["CrudeRatePositive"]])
    panelServer(id = "Council Cumulative Deaths", table = council_data[["CumulativeDeaths"]])
    panelServer(id = "Council Cumulative Negative", table = council_data[["CumulativeNegative"]])
    panelServer(id = "Council Cumulative Positive", table = council_data[["CumulativePositive"]])
    panelServer(id = "Council Cumulative Positive Percent", table = council_data[["CumulativePositivePercent"]])
    output[["council_map"]] <- renderLeaflet({
        df <- tail(council_data[[input[["council_mapInput"]]]], 1) %>% 
            pivot_longer(-Date) %>%
            inner_join(., readRDS("councils_scotland.RDS"), by = "name") %>% 
            mutate(Circle_size = scales::rescale(value, to = c(2000, 10000)))
        leaflet(df) %>% 
            addTiles(options = providerTileOptions(minZoom = 5, maxZoom = 9)) %>% 
            setView(lat = 57.4907, lng = -4.2026, zoom = 6) %>% 
            addCircles(lat = ~latitude, lng = ~longitude, radius = ~Circle_size,
                popup = paste(
                    "Date:", df[["Date"]], "<br>", 
                    "Council:", df[["name"]], "<br>", 
                    "Value:", df[["value"]], "<br>"
                )
            ) 
    })

})
