plotUI <- function(id) {
    ns <- NS(id)
    div(
        fluidRow(
            column(uiOutput(ns("select")), width = 8),
            column(uiOutput(ns("radio_select")), width = 4)
        ), 
        plotlyOutput(ns("plot"), height = "700px")
    )
}

plotServer <- function(id, table, x, roll_ave) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
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
        }
    )
}