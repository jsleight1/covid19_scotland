# UI
panelUI <- function(id, text) {
    ns <- NS(id)
    tabPanel(
        h5(id),
        tabsetPanel(type = "tabs",
            tabPanel(
                h6("Table"),
                DT::dataTableOutput(outputId = ns(id)),
                fluidRow(div(text, style = "padding-top:5px; padding-bottom:15px"))
            ),
            tabPanel(
                h6("Plot"),
                fluidRow(
                    column(uiOutput(ns(paste0(id, "_select"))), width = 8),
                    column(uiOutput(ns(paste0(id, "_radio_select"))), width = 4)
                ),
                plotlyOutput(ns(paste0(id, "_plot")), height = "700px")
            )
        )
    )
}

# Server
panelServer <- function(id, table) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- render_custom_datatable(table, id)
            output[[paste0(id, "_select")]] <- renderUI(
                selectizeInput(
                    inputId = ns(id), 
                    label = "Choose Y Axis Variables", 
                    width = "100%", 
                    multiple = TRUE,
                    choices = setdiff(colnames(table), "Date"),
                    selected = setdiff(colnames(table), "Date")[1]
                )
            )
            output[[paste0(id, "_radio_select")]] <- renderUI(
                decide_checkbox_output(
                    data = table,
                    input = req(input[[id]]),
                    id = ns(paste0(id, "_radio_select_in"))
                ) 
            )
            output[[paste0(id, "_plot")]] <- renderPlotly(
                decide_plotly_output(
                    data = table,
                    input = req(input[[id]]),
                    type = req(input[[paste0(id, "_radio_select_in")]])
                )
            )
        }
    ) 
}