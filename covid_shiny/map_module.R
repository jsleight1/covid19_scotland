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
                values <- unlist(select(tail(data[[input]], 1), json$name))
                labels <- paste("Area:", json$name, input, values)

                pal <- colorNumeric("viridis", NULL)
                leaflet(json) %>%
                    addTiles() %>%
                    addPolygons(
                        fillOpacity = 0.7,
                        smoothFactor = 0.3, 
                        stroke = FALSE,
                        fillColor = pal(values),
                        label = labels
                    ) %>% 
                    addLegend(pal = pal, values = values, opacity = 1)
            })
        }
    )
}