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