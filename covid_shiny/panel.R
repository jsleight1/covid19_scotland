# UI
panelUI <- function(id, message = "") {
    ns <- NS(id)
    tabPanel(id,
        tabsetPanel(type = "tabs",
            tabPanel(
                h6("Table"),
                DT::dataTableOutput(outputId = ns(id)),
                fluidRow(
                    downloadButton(ns("download"), "Download data"),
                    div(message), style = "padding-top:15px"
                )
            ),
            tabPanel(
                h6("Plot"),
                fluidRow(
                    column(uiOutput(ns("select")), width = 8),
                    column(uiOutput(ns("radio_select")), width = 4)
                ),
                plotlyOutput(ns("plot"), height = "700px")
            )
        )
    )
}

# Server
panelServer <- function(id, table, x = "Date", roll_ave = TRUE) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- DT::renderDataTable(
                table,
                extensions = c("Scroller", "FixedColumns"), 
                rownames = FALSE,
                options = list(
                    dom = "lrti",
                    scrollY = 500,
                    scrollX = TRUE,
                    scroller = TRUE,
                    fixedColumns = list(leftColumn = 1)
                )
            )
            output[["select"]] <- renderUI(
                selectizeInput(
                    inputId = ns(id), 
                    label = "Choose Y Axis Variables", 
                    width = "100%", 
                    multiple = TRUE,
                    choices = setdiff(colnames(table), x),
                    selected = setdiff(colnames(table), x)[1]
                )
            )
            selected <- reactive({req(input[[id]])})
            data <- reactive({table[, c(x, selected())]})
            radio <- reactive({req(input[["radio_select_in"]])})
            output[["radio_select"]] <- renderUI(
                radioButtons(
                    inputId = ns("radio_select_in"),
                    label = "Select Plot Type",
                    choices = case_when(
                        length(selected()) <= 1 ~ c("Bar Plot", "Line Plot"), 
                        TRUE ~ c("Grouped Line Plot", "Stacked Barplot")
                    ),
                    inline = TRUE,
                    width = "100%"      
                )
            )
            output[["plot"]] <- renderPlotly(
                switch(radio(),
                    "Bar Plot" = daily_barplot(df = data(), x = x, y = selected(), roll_ave),
                    "Line Plot" = daily_lineplot(df = data(), x = x, y = selected(), roll_ave),
                    "Stacked Barplot" = stacked_barplot(df = data(), x = x),
                    "Grouped Line Plot" = grouped_lineplot(df = data(), x = x) 
                )
            )
            output[["download"]] <- downloadHandler(
                filename = str_c(id, ".tsv"), 
                content = function(file) {
                    write_tsv(table, file)
                }
            )
        }
    ) 
}