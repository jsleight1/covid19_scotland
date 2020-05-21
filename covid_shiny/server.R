source("dependencies.R")
source("server_functions.R")

shinyServer(function(input, output) {
    trend_data <- reactive({
        file <- req(input$file1$datapath)
        readxl::excel_sheets(file) %>% 
            set_names() %>% 
            map(readxl::read_excel, path = file) %>% 
            tidy_trend_excel_sheets()
    })
    map_data <- reactive({
        file <- req(input$file2$datapath)
        sheets <- readxl::excel_sheets(file) %>% 
            set_names() %>% 
            map(readxl::read_excel, path = file)
        sheets[grep("Table", names(sheets))] %>% 
            map(., function(i) {
                i <- select_if(i, ~sum(!is.na(.)) > 0)
                tidy_table(df = i, row = 3)
            })
    })

    # Introduction
    output[["introduction_plot"]] <- renderPlotly({
        df <- map_data()[[1]] %>% 
            select(Date, Scotland)
        daily_barplot(df, x = "Date", y = "Scotland")
    })
    output[["introduction_date"]] <- renderText({
         as.character(pull(slice(map_data()[[1]], nrow(map_data()[[1]])), Date))
    })
    output[["introduction_cases"]] <- renderText({
         pull(slice(map_data()[[1]], nrow(map_data()[[1]])), Scotland)
    })
    output[["introduction_daily_cases"]] <- renderText({
        pull(slice(map_data()[[1]], nrow(map_data()[[1]])), Scotland) - 
        pull(slice(map_data()[[1]], nrow(map_data()[[1]]) - 1), Scotland)
    })
    output[["introduction_deaths"]] <- renderText({
        pull(slice(trend_data()[[9]], nrow(trend_data()[[9]])))

    })
    output[["introduction_daily_deaths"]] <- renderText({
        pull(slice(trend_data()[[9]], nrow(trend_data()[[9]]))) - 
        pull(slice(trend_data()[[9]], nrow(trend_data()[[9]]) - 1))

    })

    # National analysis
    output[["NHS 24"]] <- DT::renderDataTable({trend_data()[[1]]})
    output[["Hospital Care"]] <- DT::renderDataTable({trend_data()[[2]]})
    output[["Ambulance Attendances"]] <- DT::renderDataTable({trend_data()[[3]]})
    output[["Delayed Discharges"]] <- DT::renderDataTable({trend_data()[[4]]})
    output[["Testing"]] <- DT::renderDataTable({trend_data()[[5]]})
    output[["Workforce Absences"]] <- DT::renderDataTable({trend_data()[[6]]})
    output[["Adult Care Homes"]] <- DT::renderDataTable({trend_data()[[7]]})
    output[["Care Home Workforce"]] <- DT::renderDataTable({trend_data()[[8]]})
    output[["Deaths"]] <- DT::renderDataTable({trend_data()[[9]]})

    # NHS 24 plots
    output[["nhs_calls"]] <- renderPlotly({
        cumulative_group_plot(trend_data()[[1]], x = "Date", y = "value")
    })

    # Hosptial Care plots
    output[["daily_intensive_increase"]] <- renderPlotly({
        df <- find_daily_increase(trend_data()[[2]], "`COVID-19 patients in ICU or combined ICU/HDU Total`")
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })

    output[["daily_hospital_increase"]] <- renderPlotly({
        df <- find_daily_increase(trend_data()[[2]], "`COVID-19 patients in hospital (including those in ICU) Total`")
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })
    output[["cumulative_hospital"]] <- renderPlotly({
        df <- select(trend_data()[[2]], 
            Date, 
            `COVID-19 patients in ICU or combined ICU/HDU Total`, 
            `COVID-19 patients in hospital (including those in ICU) Total`
        )
        cumulative_group_plot(df, x = "Date", y = "value")
    })

    # Ambulance plots
    output[["ambulance_plot"]] <- renderPlotly({
        cumulative_group_plot(trend_data()[[3]], x = "Date", y = "value")
    })

    # Delayed Discharge plots
    output[["discharge"]] <- renderPlotly({
        daily_barplot(trend_data()[[4]], x = "Date", y = "`Number of delayed discharges`")
    })

    # Testing plots
    output[["daily_positive_tests"]] <- renderPlotly({
        df <- find_daily_increase(trend_data()[[5]], "Positive")
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })
    output[["cumulative_testing"]] <- renderPlotly({
        df <- select(trend_data()[[5]], Date, Negative, Positive) %>% 
            pivot_longer(-Date)
        p <- ggplot(data = df, aes(x = Date, y = value, fill = name)) +
            geom_bar(stat = "identity") +
            scale_fill_discrete(name = "Test Result")
        ggplotly(p)

    })

    # Workforce Absences plots
    output[["daily_workforce_absences"]] <- renderPlotly({
        cumulative_group_plot(trend_data()[[6]], x = "Date", y = "value")
    })

    # Adult care homes plots
    output[["carehome_cases_plot"]] <- renderPlotly({
        cumulative_plot(df = trend_data()[[7]], x = "Date", y = "`Cumulative number of suspected COVID-19 cases in adult care homes`")
    })
    output[["carehome_daily_plot"]] <- renderPlotly({
        daily_barplot(trend_data()[[7]], x = "Date", y = "`Daily number of new suspected COVID-19 cases in adult care homes`")
    })

    # Carehome workforce plots
    output[["staff_absence_rate"]] <- renderPlotly({
        daily_barplot(trend_data()[[8]], x = "Date", "`Staff absence rate`")
    })

    # Deaths plots
    output[["cumulative_deaths"]] <- renderPlotly({
        cumulative_plot(df = trend_data()[[9]], x = "Date", y = "`Number of COVID-19 confirmed deaths registered to date`")
    })
    output[["daily_deaths"]] <- renderPlotly({
        df <- find_daily_increase(trend_data()[[9]], "`Number of COVID-19 confirmed deaths registered to date`")  
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })

    # Regional analysis
    output[["regional_cumulative_cases"]] <- DT::renderDataTable(map_data()[[1]])
    output[["regional_COVID_inpatients"]] <- DT::renderDataTable(map_data()[[2]])
    output[["regional_hospital_confirmed"]] <- DT::renderDataTable(map_data()[[3]])
    output[["regional_hospital_suspected"]] <- DT::renderDataTable(map_data()[[4]])

    output[["regional_cumulative_plot"]] <- renderPlotly({
        cumulative_group_plot(map_data()[[1]], x = "Date", y = "value")
    })
    output[["regional_inpatient_plot"]] <- renderPlotly({
        cumulative_group_plot(map_data()[[2]], x = "Date", y = "value")
    })
    output[["regional_confirmed_plot"]] <- renderPlotly({
        cumulative_group_plot(map_data()[[3]] , x = "Date", y = "value")
    })
    output[["regional_suspected_plot"]] <- renderPlotly({
        cumulative_group_plot(map_data()[[4]], x = "Date", y = "value")
    })

    output$map <- renderLeaflet({
        coords <- tribble(
            ~Region,                            ~Latitude,         ~Longitude,
            "NHS Ayrshire & Arran",             55.4586,             -4.6292,
            "NHS Borders",                      55.5486,             -2.7861,
            "NHS Dumfries & Galloway",          55.0709,             -3.6051,
            "NHS Fife",                         56.2082,             -3.1495,
            "NHS Forth Valley",                 56.0253,             -3.8490,
            "NHS Grampian",                     57.4149,             -2.0991,            
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
        type <- switch(input$mapInput,
            "cases" = "Table 1 - Cumulative cases",   
            "inpatients" = "Table 2 - ICU patients",   
            "regional_confirmed" = "Table 3a - Hospital Confirmed",
            "regional_suspected" =  "Table 3b- Hospital Suspected"  
        )
        df <- map_data()[[type]] %>% 
            slice(nrow(.)) %>% 
            select(-Date) %>% 
            t() %>% 
            as.data.frame() %>% 
            rownames_to_column("Region") %>% 
            rename(Cases_to_date = "V1") %>% 
            inner_join(., coords, by = "Region") %>% 
            mutate(Circle_size = scales::rescale(Cases_to_date) * 20)
        leaflet(df) %>% 
            addTiles() %>% 
            setView(lat = 56.4907, lng = -4.2026, zoom = 6) %>% 
            addCircleMarkers(
                lat = ~Latitude, 
                lng = ~Longitude, 
                radius = ~Circle_size,
                label = ~Cases_to_date
            ) 
    })
})
