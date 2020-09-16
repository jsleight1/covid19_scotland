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
mapServer <- function(id, data, json) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- renderLeaflet({
                input <- req(input[[paste0(id, "input")]])
                json[[input]] <- unlist(select(tail(data[[input]], 1), json[["name"]]))
                json[["label"]] <- paste(
                    "Area:", json[["name"]], 
                    input, json[[input]]
                )

                pal <- colorNumeric("viridis", NULL)
                leaflet(json) %>%
                    addTiles() %>%
                    addPolygons(
                        fillOpacity = 0.7,
                        smoothFactor = 0.3, 
                        stroke = FALSE,
                        fillColor = pal(json[[input]]),
                        label = ~label
                    ) %>% 
                    addLegend(pal = pal, values = json[[input]], opacity = 1)
            })
        }
    )
}