source("dependencies.R")
source("server_functions.R")

shinyServer(function(input, output) {
    # Read in national data
    url_trend <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Trends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B28%2BMay%2B2020.xlsx"
    GET(url_trend, write_disk(tf_national <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
    national_data <- readxl::excel_sheets(tf_national) %>% 
        set_names() %>% 
        map(readxl::read_excel, path = tf_national) %>% 
        tidy_trend_excel_sheets()

    # Read in regional data
    url_regional = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdata%2Bby%2BNHS%2BBoard%2B28%2BMay%2B2020.xlsx"
    GET(url_regional, write_disk(tf_regional <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
    sheets <- readxl::excel_sheets(tf_regional) %>% 
        set_names() %>% 
        map(readxl::read_excel, path = tf_regional)
    regional_data <- sheets[grep("Table", names(sheets))] %>% 
        map(., function(i) {
            i <- select_if(i, ~sum(!is.na(.)) > 0)
            tidy_table(df = i, row = 3)
        })
    
    # Introduction
    output[["introduction_plot"]] <- renderPlotly({
        daily_barplot(regional_data[["Table 1 - Cumulative cases"]], x = "Date", y = "Scotland") 
    })
    output[["introduction_date"]] <- renderText({
        as.character(last(pull(regional_data[["Table 1 - Cumulative cases"]], Date)))
    })
    output[["introduction_cases"]] <- renderText({
        last(pull(regional_data[["Table 1 - Cumulative cases"]], Scotland))
    })
    output[["introduction_daily_cases"]] <- renderText({
        last(pull(national_data[["Table 5 - Testing"]], Daily_Positive))
    })
    output[["introduction_deaths"]] <- renderText({
        last(pull(national_data[["Table 8 - Deaths"]], -Date))
    })
    output[["introduction_daily_deaths"]] <- renderText({
        df <- find_daily_increase(
            national_data[["Table 8 - Deaths"]], 
            column = "`Number of COVID-19 confirmed deaths registered to date`"
        )
        last(pull(df, `Daily Change`))
    })

    # National analysis
    output[["NHS 24"]] <- render_custom_datatable(national_data[["Table 1 - NHS 24"]], "NHS_24") 
    output[["Hospital Care"]] <- render_custom_datatable(national_data[["Table 2 - Hospital Care"]], "Hospital_Care")
    output[["Ambulance Attendances"]] <- render_custom_datatable(national_data[["Table 3 - Ambulance"]], "Ambulance_Attendances")
    output[["Delayed Discharges"]] <- render_custom_datatable(national_data[["Table 4 - Delayed Discharges"]], "Delayed_Discharges")
    output[["Testing"]] <- render_custom_datatable(national_data[["Table 5 - Testing"]], "COVID19_Testing")
    output[["Workforce Absences"]] <- render_custom_datatable(national_data[["Table 6 - Workforce"]], "Workforce_Absences")
    output[["Adult Care Homes"]] <- render_custom_datatable(national_data[["Table 7a - Care Homes"]], "Adult_Care_Homes")
    output[["Care Home Workforce"]] <- render_custom_datatable(national_data[["Table 7b - Care Home Workforce"]], "Care_Home_Workforce")
    output[["Deaths"]] <- render_custom_datatable(find_daily_increase(national_data[["Table 8 - Deaths"]], "`Number of COVID-19 confirmed deaths registered to date`"), "COVID19_Deaths")

    # NHS 24 plots
    output[["nhs_calls_select"]] <- renderUI({
        selectizeInput(
            inputId = "nhs_calls", 
            label = "Choose Y Axis Variable", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 1 - NHS 24"]]), "Date"))
        )
    })
    output[["nhs_calls_plot"]] <- renderPlotly({
        if (length(req(input[["nhs_calls"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 1 - NHS 24"]], 
                x = "Date", 
                y = paste0("`", req(input[["nhs_calls"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 1 - NHS 24"]], Date, req(input[["nhs_calls"]])), 
                x = "Date", 
                y = "value"
            )
        } 
    })

    # Hospital Care plots
    output[["daily_intensive_increase"]] <- renderPlotly({
        df <- find_daily_increase(
            national_data[["Table 2 - Hospital Care"]], 
            column = "`COVID-19 patients in ICU or combined ICU/HDU Total`"
        )
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })

    output[["daily_hospital_increase"]] <- renderPlotly({
        df <- find_daily_increase(
            national_data[["Table 2 - Hospital Care"]], 
            column = "`COVID-19 patients in hospital (including those in ICU) Total`"
        )
        daily_barplot(df, x = "Date", y = "`Daily Change`")
    })
    output[["cumulative_hospital"]] <- renderPlotly({
        df <- select(national_data[["Table 2 - Hospital Care"]], 
            Date, 
            `COVID-19 patients in ICU or combined ICU/HDU Total`, 
            `COVID-19 patients in hospital (including those in ICU) Total`
        )
        cumulative_group_plot(df, x = "Date", y = "value")
    })

    # Ambulance plots
    output[["ambulance_select"]] <- renderUI({
        selectizeInput(
            inputId = "ambulance", 
            label = "Choose Y Axis Variable", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 3 - Ambulance"]]), "Date"))
        )
    })
    output[["ambulance_plot"]] <- renderPlotly({
        if (length(req(input[["ambulance"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 3 - Ambulance"]], 
                x = "Date", 
                y = paste0("`", req(input[["ambulance"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 3 - Ambulance"]], Date, req(input[["ambulance"]])), 
                x = "Date", 
                y = "value"
            )
        } 
    })

    # Delayed Discharge plots
    output[["discharge"]] <- renderPlotly({
        daily_barplot(
            national_data[["Table 4 - Delayed Discharges"]], 
            x = "Date", 
            y = "`Number of delayed discharges`"
        )
    })

    # Testing plots
    output[["daily_tests"]] <- renderPlotly({
        positive <- find_daily_increase(national_data[["Table 5 - Testing"]], "Positive") %>% 
            select(Date, Positive = "Daily Change")
        negative <- find_daily_increase(national_data[["Table 5 - Testing"]], "Negative") %>% 
            select(Date, Negative = "Daily Change")
        df <- inner_join(positive, negative, by = "Date") %>% 
            pivot_longer(-Date)
        stacked_barplot(df, x = "Date", y = "value")
    })
    output[["cumulative_testing"]] <- renderPlotly({
        df <- select(national_data[["Table 5 - Testing"]], Date, Negative, Positive) %>% 
            pivot_longer(-Date)
        stacked_barplot(df, x = "Date", y = "value")
    })

    # Workforce Absences plots
    output[["workforce_absence_select"]] <- renderUI({
        selectizeInput(
            inputId = "workforce_absence", 
            label = "Choose Y Axis Variable", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 6 - Workforce"]]), "Date"))
        )
    })
    output[["workforce_absence_plot"]] <- renderPlotly({
        if (length(req(input[["workforce_absence"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 6 - Workforce"]], 
                x = "Date", 
                y = paste0("`", req(input[["workforce_absence"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 6 - Workforce"]], Date, req(input[["workforce_absence"]])), 
                x = "Date", 
                y = "value"
            )
        } 
    })

    # Adult care homes plots
    output[["carehome_cases_select"]] <- renderUI({
        selectizeInput(
            inputId = "care_cases", 
            label = "Choose Y Axis Variable", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 7a - Care Homes"]]), "Date"))
        )
    })
    output[["carehome_cases_plot"]] <- renderPlotly({
        if(length(req(input[["care_cases"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 7a - Care Homes"]], 
                x = "Date", 
                y = paste0("`", req(input[["care_cases"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 7a - Care Homes"]], Date, req(input[["care_cases"]])),
                x = "Date",
                y = "value"
            )
        }
        
    })

    # Carehome workforce plots
    output[["care_workforce_select"]] <- renderUI({
        selectizeInput(
            inputId = "care_work", 
            label = "Choose Y Axis Variable:", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 7b - Care Home Workforce"]]), "Date"))
        )
    })
    output[["care_workforce_plot"]] <- renderPlotly({
        if (length(req(input[["care_work"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 7b - Care Home Workforce"]], 
                x = "Date", 
                y = paste0("`", req(input[["care_work"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 7b - Care Home Workforce"]], Date, req(input[["care_work"]])),
                x = "Date",
                y = "value"
            )
        }
    })

    # Deaths plots
    output[["deaths_select"]] <- renderUI({
        selectizeInput(
            inputId = "deaths", 
            label = "Choose Y Axis Variable:", 
            width = "100%", 
            multiple = TRUE,
            choices = as.list(setdiff(colnames(national_data[["Table 8 - Deaths"]]), "Date"))
        )
    })
    
    output[["deaths_plot"]] <- renderPlotly({
        if (length(req(input[["deaths"]])) == 1) {
            daily_barplot(
                df = national_data[["Table 8 - Deaths"]], 
                x = "Date", 
                y = paste0("`", req(input[["deaths"]]), "`")
            )
        } else {
            cumulative_group_plot(
                df = select(national_data[["Table 8 - Deaths"]], Date, req(input[["deaths"]])),
                x = "Date",
                y = "value"
            )
        }  
    })

    # Regional analysis
    output[["regional_cumulative_cases"]] <- render_custom_datatable(regional_data[["Table 1 - Cumulative cases"]], "regional_cumulative_cases") 
    output[["regional_ICU"]] <- render_custom_datatable(regional_data[["Table 2 - ICU patients"]], "regional_ICU")
    output[["regional_hospital_confirmed"]] <- render_custom_datatable(regional_data[["Table 3a - Hospital Confirmed"]], "regional_hospital_confirmed")
    output[["regional_hospital_suspected"]] <- render_custom_datatable(regional_data[["Table 3b- Hospital Suspected"]], "regional_hospital_suspected")
    
    output[["regional_cumulative_plot"]] <- renderPlotly({
        cumulative_group_plot(regional_data[["Table 1 - Cumulative cases"]], x = "Date", y = "value")
    })
    output[["regional_icu_plot"]] <- renderPlotly({
        cumulative_group_plot(regional_data[["Table 2 - ICU patients"]], x = "Date", y = "value")
    })
    output[["regional_confirmed_plot"]] <- renderPlotly({
        cumulative_group_plot(regional_data[["Table 3a - Hospital Confirmed"]] , x = "Date", y = "value")
    })
    output[["regional_suspected_plot"]] <- renderPlotly({
        cumulative_group_plot(regional_data[["Table 3b- Hospital Suspected"]], x = "Date", y = "value")
    })

    output$map <- renderLeaflet({
        coords <- tribble(
            ~Region,                            ~Latitude,         ~Longitude,
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
        type <- switch(input$mapInput,
            "cases" = "Cumulative cases",   
            "icu" = "ICU patients",   
            "regional_confirmed" = "Hospital Confirmed",
            "regional_suspected" =  "Hospital Suspected"  
        )
        df <- regional_data[[grep(type, names(regional_data))]] %>% 
            slice(nrow(.)) %>% 
            select(-Date) %>% 
            t() %>% 
            as.data.frame() %>% 
            rownames_to_column("Region") %>% 
            rename(Cases_to_date = "V1") %>% 
            inner_join(., coords, by = "Region") %>% 
            mutate(Circle_size = scales::rescale(Cases_to_date, to = c(2000, 18000)))
        leaflet(df) %>% 
            addTiles(options = providerTileOptions(minZoom = 5, maxZoom = 9)) %>% 
            setView(lat = 56.4907, lng = -4.2026, zoom = 6) %>% 
            addCircles(
                lat = ~Latitude, 
                lng = ~Longitude, 
                radius = ~Circle_size,
                popup = paste(df$Region, "<br>",
                           type, df$Cases_to_date, "<br>")
            ) 
    })
})
