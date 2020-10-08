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
            data <- reactive({
                select(table, all_of(c(first_col, req(input[[id]]))))
            })
            selected <- reactive({req(input[[id]])})
            radio <- reactive({req(input[[paste0(id, "_radio_select_in")]])})
            output[[paste0(id, "_radio_select")]] <- renderUI(
                radioButtons(
                    inputId = ns(paste0(id, "_radio_select_in")),
                    label = "Select Plot Type",
                    choices = case_when(
                        length(selected()) <= 1 ~ c("Bar Plot", "Line Plot"), 
                        length(selected()) > 1 ~ c("Line Plot", "Stacked Barplot")
                    ),
                    inline = TRUE,
                    width = "100%"      
                )
            )
            output[[paste0(id, "_plot")]] <- renderPlotly(
                if (length(selected()) == 1) {
                    switch(radio(),
                        "Bar Plot" = daily_barplot(df = data(), x = x, y = selected(), roll_ave),
                        "Line Plot" = daily_lineplot(df = data(), x = x, y = selected(), roll_ave)
                    )
                } else {
                    switch(radio(),
                        "Stacked Barplot" = stacked_barplot(df = data(), x = x),
                        "Line Plot" = grouped_lineplot(df = data(), x = x) 
                    )
                }
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