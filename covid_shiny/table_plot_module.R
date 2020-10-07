# UI
panelUI <- function(id, message = "") {
    ns <- NS(id)
    tabPanel(id,
        tabsetPanel(type = "tabs",
            tabPanel(
                h6("Table"),
                DT::dataTableOutput(outputId = ns(id)),
                fluidRow(
                    downloadButton(ns(paste0(id, "download")), "Download data"),
                    div(message), style = "padding-top:15px"
                )
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
panelServer <- function(id, table, x = "Date", first_col = x, roll_ave = TRUE) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- render_custom_datatable(table)
            output[[paste0(id, "_select")]] <- renderUI(
                selectizeInput(
                    inputId = ns(id), 
                    label = "Choose Y Axis Variables", 
                    width = "100%", 
                    multiple = TRUE,
                    choices = setdiff(colnames(table), first_col),
                    selected = setdiff(colnames(table), first_col)[1]
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
                    type = req(input[[paste0(id, "_radio_select_in")]]),
                    x = x, 
                    first_col = first_col, 
                    roll_ave = roll_ave
                )
            )
            output[[paste0(id, "download")]] <- downloadHandler(
                filename = paste0(gsub(" ",  "_", id), ".tsv"), 
                content = function(file) {
                    write_tsv(table, file)
                }
            )
        }
    ) 
}