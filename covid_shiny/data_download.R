################################################################################
# Read in regional data
################################################################################
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

################################################################################
# Read in national data
################################################################################
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
    select_if(~sum(!is.na(.)) > 2) %>% 
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
    
# Care homes Cases
cols <- unlist(slice(national_data[["Table 7a - Care Homes (Cases)"]], 2))
national_data[["Table 7a - Care Homes"]] <- national_data[["Table 7a - Care Homes (Cases)"]] %>% 
    slice(3:nrow(.)) %>% 
    set_names(cols) %>% 
    mutate_at(c(1, 3), as.numeric) %>% 
    mutate(Week = factor(Week, levels = .data[["Week"]])) %>% 
    select(Week, everything())

# Care homes
national_data[["Table 7c - Care Homes (Homes)"]] <- tidy_table(
    df = national_data[["Table 7c - Care Homes (Homes)"]],
    row = 3
)

# Education 
national_data[["Table 9 - School education"]] <- tidy_table(
    df = select(national_data[["Table 9 - School education"]], 1:4), 
    row = 3
)

################################################################################
# Read in council data
################################################################################

# url_council_cumulative <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/e8454cf0-1152-4bcb-b9da-4343f625dfef/download/total_cases_by_la_20200908.csv"
# GET(url_council_cumulative, write_disk(tf_council_cumulative <- tempfile(fileext = "csv"), overwrite = TRUE))
url_council <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/427f9a25-db22-4014-a3bc-893b68243055/download/trend_ca_20200908.csv"
GET(url_council, write_disk(tf_council <- tempfile(fileext = "csv"), overwrite = TRUE))

# council_cumulative <- read_csv(tf_council_cumulative)
council_data <- read_csv(tf_council) %>% 
   left_join(., readRDS("councils_scotland.RDS"), by = c("CA" = "code")) %>% 
   select(name, everything(), -CA, -latitude, -longitude) %>% 
   mutate(CumulativePositivePercent = CumulativePositivePercent * 100) %>% 
   pivot_longer(cols = -c(name, Date), names_to = "key", values_to = "value")
council_data <- council_data %>% 
    group_split(key) %>% 
    set_names(unique(sort(council_data[["key"]]))) %>% 
    lapply(., function(.x) {
        .x %>% 
            mutate(Date = lubridate::ymd(Date)) %>% 
            pivot_wider(names_from = "name", values_from = "value") %>% 
            select(Date, sort(colnames(.)), -key, -"NA") %>% 
            mutate_if(is.numeric, ~round(as.numeric(.), 2))
    })

# council_local <- tribble(
#     ~name,                          ~latitude,     ~longitude,
#     "Aberdeen City",                57.1497,       -2.0943,
#     "Aberdeenshire",                57.2869,       -2.3816,
#     "Angus",                        56.6980,       -2.9124,
#     "Argyll and Bute",              56.4006,       -5.4807,
#     "Clackmannanshire",             56.1075,       -3.7529,
#     "Stirling",                     56.1165,       -3.9369,
#     "Dumfries and Galloway",        55.0709,       -3.6051,
#     "Dundee City",                  56.4620,       -2.9707,
#     "East Ayrshire",                55.4536,       -4.2648,
#     "East Dunbartonshire",          55.9743,       -4.2023,
#     "East Lothian",                 55.9587,       -2.7749, 
#     "East Renfrewshire",            55.7505,       -4.3360,
#     "City of Edinburgh",            55.9533,       -3.1883,
#     "Falkirk",                      56.0019,       -3.7839,
#     "Fife",                         56.2082,       -3.1495,
#     "Glasgow City",                 55.8642,       -4.2518,
#     "Highland",                     57.4596,       -4.2264,
#     "Inverclyde",                   55.9317,       -4.7895,
#     "Midlothian",                   55.8124,       -3.0702,
#     "Moray",                        57.6482,       -3.3114,
#     "North Ayrshire",               55.617,        -4.6795,
#     "North Lanarkshire",            55.8662,       -3.9613,
#     "Orkney Islands",               58.9814,       -2.9569,
#     "Perth and Kinross",            56.3978,       -3.4317,
#     "Renfrewshire",                 55.8467,       -4.5331,
#     "Scottish Borders",             55.5486,       -2.7861,
#     "Shetland Islands",             60.1542,       -1.1463,
#     "South Ayrshire",               55.4189,       -4.6292,
#     "South Lanarkshire",            55.6736,       -3.7820,
#     "West Dunbartonshire",          55.9843,       -4.5646,
#     "West Lothian",                 55.8818,       -3.6267,
#     "Na h-Eileanan Siar",           58.2091,       -6.3789
# )

# tmp <- read_csv("../../ca11_ca19.csv") %>% 
#     dplyr::select(code = "CA", name = "CAName") %>% 
#     unique %>% 
#     left_join(., council_local, by = "name")
# saveRDS(tmp, "covid_shiny/councils_scotland.RDS")
