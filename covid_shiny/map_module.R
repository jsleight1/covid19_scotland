# UI
mapUI <- function(id, choices) {
    ns <- NS(id)
    tabPanel(
        "Map",
        leafletOutput(ns(id), height = 700),
        fluidRow(
            radioButtons(
                inputId = ns(paste0(id, "input")),
                label = "Select Input",
                choices = choices,
                inline = TRUE
            )
        )
    )
}

# Server
mapServer <- function(id, data, locations) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- renderLeaflet({  
                df <- tail(data[[req(input[[paste0(id, "input")]])]], 1) %>% 
                    pivot_longer(-Date) %>%
                    inner_join(., locations, by = "name") %>% 
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
        }
    )
}