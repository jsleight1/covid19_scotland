source("dependencies.R")
source("server_functions.R")
source("data_download.R")
source("table_plot_module.R")

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
        last(pull(select(national_data[["Table 8 - Deaths"]], `Number of COVID-19 confirmed deaths registered to date`)))
    })
    output[["introduction_daily_deaths"]] <- renderText({
        last(pull(national_data[["Table 8 - Deaths"]], `Daily Change`))
    })

    # National analysis
    panelServer(id = "Testing", select = 4, 
        table = national_data[["Table 5 - Testing"]])
    panelServer(id = "Hospital Care", select = c(1, 3), 
        table = national_data[["Table 2 - Hospital Care"]])
    panelServer(id = "Ambulance Attendances",
        table = national_data[["Table 3 - Ambulance"]])
    panelServer(id = "NHS Calls",
        table = national_data[["Table 1 - NHS 24"]])
    panelServer(id = "Delayed Discharges",
        table = national_data[["Table 4 - Delayed Discharges"]])
    panelServer(id = "Workforce",
        table = national_data[["Table 6 - Workforce"]])
    panelServer(id = "Care Homes",
        table = national_data[["Table 7a - Care Homes"]])
    panelServer(id = "Care Home Workforce",
        table = national_data[["Table 7b - Care Home Workforce"]])
    panelServer(id = "Deaths",
        table = national_data[["Table 8 - Deaths"]])

    # Regional analysis
    panelServer(id = "Regional Cases", select = 15,
        table = regional_data[["Table 1 - Cumulative cases"]])
    panelServer(id = "Regional ICU", select = 16,
        table = regional_data[["Table 2a - ICU patients"]])
    panelServer(id = "Regional Confirmed", select = 16,
        table = regional_data[["Table 3a - Hospital Confirmed"]])
    panelServer(id = "Regional Suspected", select = 16,
        table = regional_data[["Table 3b- Hospital Suspected"]])

    output[["map"]] <- renderLeaflet({
        coords <- tribble(
            ~name,                            ~latitude,         ~longitude,
            "NHS Ayrshire & Arran",             55.4586,             -4.6292,
            "NHS Borders",                      55.5486,             -2.7861,
            "NHS Dumfries & Galloway",          55.0709,             -3.6051,
            "NHS Fife",                         56.2082,             -3.1495,
            "NHS Forth Valley",                 56.0253,             -3.8490,
            "NHS Grampian",                     57.1497,             -2.0943,            
            "NHS Greater Glasgow & Clyde",      55.8642,             -4.2518,
            "NHS Highland",                     57.4778,             -4.2247, 
            "NHS Lanarkshire",                  55.6736,             -3.7820,
            "NHS Lothian",                      55.9533,             -3.1883,
            "NHS Orkney",                       58.9809,             -2.9605,             
            "NHS Shetland",                     60.5297,             -1.2659,
            "NHS Tayside",                      56.4620,             -2.9707,
            "NHS Western Isles",                58.2094,             -6.3849,
            "Golden Jubilee National Hospital", 55.9060,             -4.4262
        )
        df <- tail(regional_data[[input[["mapInput"]]]], 1) %>% 
            pivot_longer(-Date) %>%
            inner_join(., coords, by = "name") %>% 
            mutate(Circle_size = scales::rescale(value, to = c(2000, 18000)))
        leaflet(df) %>% 
            addTiles(options = providerTileOptions(minZoom = 5, maxZoom = 9)) %>% 
            setView(lat = 56.4907, lng = -4.2026, zoom = 6) %>% 
            addCircles(lat = ~latitude, lng = ~longitude, radius = ~Circle_size,
                popup = paste(df[["name"]], "<br>",
                           input[["mapInput"]], df[["value"]], "<br>")
            ) 
    })
})
