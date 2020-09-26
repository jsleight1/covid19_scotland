

token <- drop_auth()
saveRDS(token, file.path(tempdir(), "token.RDS"))

result <- drop_download(
    path = "covid_shiny/processed_covid_data.RDS", 
    local_path = file.path(tempdir(), "downloaded_covid_data.RDS"),
    dtoken = readRDS(file.path(tempdir(), "token.RDS"))
)
file.remove(file.path(tempdir(), "token.RDS"))

if (result) {
    downloaded_data <- readRDS(file.path(tempdir(), "downloaded_covid_data.RDS"))
}

regional_data <- downloaded_data[["regional_data"]]
region_json <- downloaded_data[["region_json"]]
national_data <- downloaded_data[["national_data"]]
council_data <- downloaded_data[["council_data"]]
council_json <- downloaded_data[["council_json"]]