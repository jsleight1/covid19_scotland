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

# Read in national data
url_trend <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Trends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B28%2BMay%2B2020.xlsx"
GET(url_trend, write_disk(tf_national <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
national_data <- readxl::excel_sheets(tf_national) %>% 
    set_names() %>% 
    map(readxl::read_excel, path = tf_national)

national_data <- national_data[grep("Table", names(national_data))] %>% 
    map(., function(i) select_if(i, ~sum(!is.na(.)) > 0))

# NHS 24 stats
national_data[["Table 1 - NHS 24"]] <- tidy_table(
    df = national_data[["Table 1 - NHS 24"]],
    row = 3
)

# Ambulance stats
national_data[["Table 3 - Ambulance"]] <- tidy_table(
    df = select(national_data[["Table 3 - Ambulance"]], -1),
    row = 3
)

# Delayed discharges 
national_data[["Table 4 - Delayed Discharges"]] <- tidy_table(
    df = select(national_data[["Table 4 - Delayed Discharges"]], -1),
    row = 3
)

# Hospital care stats
national_data[["Table 2 - Hospital Care"]] <- national_data[["Table 2 - Hospital Care"]] %>% 
    slice(4:nrow(.)) %>% 
    select(1, 2, 5) %>% 
    set_names(
        "Date", 
        "COVID-19 patients in ICU or combined ICU/HDU Confirmed",
        "COVID-19 patients in hopsital (including those in ICU) Confirmed"
    ) %>% 
    mutate_all(~round(as.numeric(.))) %>% 
    mutate(
        Date = excel_numeric_to_date(Date),
        `Daily Change in Intensive Care Confirmed` = `COVID-19 patients in ICU or combined ICU/HDU Confirmed` - 
               lag(`COVID-19 patients in ICU or combined ICU/HDU Confirmed`),
        `Daily Change in Total Hospital Patients Confirmed` = `COVID-19 patients in hopsital (including those in ICU) Confirmed` - 
            lag(`COVID-19 patients in hopsital (including those in ICU) Confirmed`)
    )

# Care home workforce
national_data[["Table 7b - Care Home Workforce"]] <- tidy_table(
    df = national_data[["Table 7b - Care Home Workforce"]],
    row = 2
)

# Deaths
national_data[["Table 8 - Deaths"]] <- tidy_table(
        df = national_data[["Table 8 - Deaths"]],
        row = 3
    ) %>% 
    mutate(`Daily Deaths` = `Number of COVID-19 confirmed deaths registered to date` - 
        lag(`Number of COVID-19 confirmed deaths registered to date`)
    )

# Testing 
national_data[["Table 5 - Testing"]] <- national_data[["Table 5 - Testing"]] %>% 
    set_names(
        c("Date", "Negative", "Positive", "Total", "Daily Positive", 
        paste("NHS labs", c("Daily", "Cumulative"), sep = " "), 
        paste("Regional Centres", c("Daily", "Cumulative"), sep = " "),
        c("Total daily tests", "People tested in last 7 days", "Positive cases in last 7 days", "Tests in last 7 days")
    )) %>% 
    slice(4:nrow(.)) %>% 
    mutate_all(~round(as.numeric(.))) %>% 
    mutate(
        Date = excel_numeric_to_date(Date), 
        `Daily Negative` = Negative - lag(Negative)
    ) %>% 
    select(Date, Negative, `Daily Negative`, Positive, 
        `Daily Positive`, everything())

# Workforce absences
cols <- na.omit(unlist(slice(national_data[["Table 6 - Workforce"]], 1)))
national_data[["Table 6 - Workforce"]] <- national_data[["Table 6 - Workforce"]] %>% 
    slice(grep("Weekly", .data[["Table 6 - Number of NHS staff reporting as absent due to Covid-19"]]) + 1:nrow(.)) %>% 
    set_names(cols) %>% 
    mutate(Date = lubridate::dmy(gsub("week to ", "", .data[["Date"]]))) %>% 
    mutate_if(is.character, ~round(as.numeric(.), 2)) 
    
# Care homes
cols <- unlist(slice(national_data[["Table 7a - Care Homes (Cases)"]], 2))
national_data[["Table 7a - Care Homes"]] <- national_data[["Table 7a - Care Homes (Cases)"]] %>% 
    slice(3:nrow(.)) %>% 
    set_names(cols) %>% 
    mutate_at(c(1, 3), as.numeric) %>% 
    mutate(Week = factor(Week, levels = .data[["Week"]])) %>% 
    select(Week, everything())

    




