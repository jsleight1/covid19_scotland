source("dependencies.R")
source("server_functions.R")

################################################################################
# Read in regional data
################################################################################
url_regional = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdaily%2Bdata%2B-%2Bby%2BNHS%2BBoard%2B-%2B7%2BNovember%2B2020.xlsx"
GET(url_regional, write_disk(tf_regional <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
regional_data <- readxl::excel_sheets(tf_regional) %>% 
    set_names() %>% 
    map(readxl::read_excel, path = tf_regional) %>% 
    .[c("Table 1 - Cumulative cases", "Table 2 - ICU patients", "Table 3 - Hospital patients")] %>% 
    map2(., c("Date notified", "Reporting date", "Reporting date"), function(.x, .y) {
        tidy_table(df = .x, row = 3, date_col = .y)
    })

# Shape file downnloaded from https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards
# Regions json generate by:
# ogr2ogr -progress -t_srs WGS84 -simplify 300 scotland_regions.shp SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp
# topojson -o scotland_regions.json scotland_regions.shp -p
region_json <- rgdal::readOGR("data/scotland_regions.json")
region_json[["name"]] <- paste("NHS", gsub(" and ", " & ", region_json[["HBName"]]))
stopifnot(region_json[["name"]] %in% colnames(regional_data[[1]]))

################################################################################
# Read in national data
################################################################################
url_trend <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Trends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B28%2BMay%2B2020.xlsx"
GET(url_trend, write_disk(tf_national <- tempfile(fileext = ".xlsx"), overwrite = TRUE))
national_data <- readxl::excel_sheets(tf_national) %>% 
    set_names() %>% 
    map(readxl::read_excel, path = tf_national) %>% 
    .[grep("Table", names(.))] %>% 
    map(., function(i) select_if(i, ~sum(!is.na(.)) > 0))

# Delayed discharges 
national_data[["Table 4 - Delayed Discharges"]] <- tidy_table(
    df = select(national_data[["Table 4 - Delayed Discharges"]], -1),
    row = 3
)

# Hospital care stats
national_data[["Table 2 - Hospital Care"]] <- tidy_table(
        df = national_data[["Table 2 - Hospital Care"]],
        row = 3, 
        date_col = "Reporting Date"
    ) %>%
    set_names(gsub("\\(i{1,}\\) |\\r|\\n", "", colnames(.))) %>% 
    mutate(
        `Daily Change in Intensive Care <= 28 days` = `COVID-19 patients in ICU or combined ICU/HDU (with length of stay 28 days or less)` - 
               lag(`COVID-19 patients in ICU or combined ICU/HDU (with length of stay 28 days or less)`),
        `Daily Change in Total Hospital Patients <= 28 days` = `COVID-19 patients in hospital (including those in ICU) (with length of stay 28 days or less)` - 
            lag(`COVID-19 patients in hospital (including those in ICU) (with length of stay 28 days or less)`), 
        `Daily Chance in Intensive Case > 28 days` = `COVID-19 patients in ICU or combined ICU/HDU (with length of stay more than 28 days)` -
            lag(`COVID-19 patients in ICU or combined ICU/HDU (with length of stay more than 28 days)`)
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
    select(1:9) %>% 
    set_names(c(
        "Date", 
        "PCR only",
        "New cases reported daily PCR only", 
        "New cases reported daily LFD only", 
        "Total number of positive cases (either PCR or LFD)", 
        "Percentage of newly reported cases which are reinfections", 
        "Total number of positive cases in the last 7 days (either PCR or LFD)", 
        "Cumulative number of positive cases since the start of the pandemic", 
        "Percentage of cases which are reinfections since the start of the pandemic"
    )) %>% 
    slice(4:nrow(.)) %>% 
    mutate_all(~round(as.numeric(.), 2)) %>% 
    mutate(Date = excel_numeric_to_date(Date))

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

# Care home workforce
national_data[["Table 7b - Care Home Workforce"]] <- tidy_table(
    df = national_data[["Table 7b - Care Home Workforce"]],
    row = 2
)

# Care homes
national_data[["Table 7c - Care Homes (Homes)"]] <- tidy_table(
    df = national_data[["Table 7c - Care Homes (Homes)"]],
    row = 3
)

# Vaccinations
national_data[["Table 10a - Vaccinations"]] <- tidy_table(
        df = national_data[["Table 10a - Vaccinations"]], 
        row = 3
    ) %>% 
    mutate(
        `Daily First Dose Vaccinations` = `Number of people who have received the first dose of the Covid vaccination` - 
            lag(`Number of people who have received the first dose of the Covid vaccination`), 
        `Daily Second Dose Vaccinations` = `Number of people who have received the second dose of the Covid vaccination` -
            lag(`Number of people who have received the second dose of the Covid vaccination`), 
        `Daily Booster Vaccinations` = `Number of people who have received a third dose or booster Covid vaccination` -
            lag(`Number of people who have received a third dose or booster Covid vaccination`)
    )

national_data[["Table 11 - Vac supply"]] <- tidy_table(
    df = national_data[["Table 11 - Vac supply"]], 
    row = 3
)

################################################################################
# Read in council data
################################################################################

url_council <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/427f9a25-db22-4014-a3bc-893b68243055/download/trend_ca_20200908.csv"
GET(url_council, write_disk(tf_council <- tempfile(fileext = "csv"), overwrite = TRUE))
council_data <- read_csv(tf_council)
council_data <- council_data %>% 
    select(name = "CAName", everything(), -CA) %>% 
    pivot_longer(cols = -c(name, Date), names_to = "key", values_to = "value") %>% 
    group_split(key) %>% 
    set_names(unique(sort(setdiff(colnames(council_data), c("Date", "CA", "CAName"))))) %>% 
    lapply(., function(.x) {
        .x %>% 
            mutate(Date = lubridate::ymd(Date)) %>% 
            pivot_wider(names_from = "name", values_from = "value") %>% 
            select(Date, sort(colnames(.)), -key) %>% 
            mutate_if(is.numeric, ~round(as.numeric(.), 2)) %>% 
            set_names(gsub(" & ", " and ", colnames(.)))
    })

# Shape file downloaded from https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=09FA9EA46E60A59D1EF83383E3819105#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
# Converted to json in similar fashion to scotland_regions.json
council_json <- rgdal::readOGR("data/scotland_councils.json")
council_json[["name"]] <- gsub("Eilean Siar", "Na h-Eileanan Siar", council_json[["local_auth"]])
stopifnot(council_json[["name"]] %in% colnames(council_data[[1]]))

# Set up cron script to run data_download.R to download and process data
# Then upload the processed data to dropbox
processed_data <- list(
    "regional_data" = regional_data,
    "region_json" = region_json,
    "national_data" = national_data,
    "council_data" = council_data,
    "council_json" = council_json
)

saveRDS(processed_data, file = "processed_covid_data.RDS")