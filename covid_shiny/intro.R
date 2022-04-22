#UI
introUI <- function(id) {
    ns <- NS(id)
    tabPanel(id,
        fluidRow(
            div(
                "DISCLAIMER April 2022: Please note due to changes in Scottish 
                Government reporting daily covid statistics are no longer 
                available. Please see 'National and Regional Data' reference
                for details."
            ),
            style = "padding-top:15px; color:red"
        ),
        fluidRow(
            column(
                width = 12, div("National Cumulative Cases", style = "font-size:20px;"), 
                plotlyOutput(outputId = ns("plot"), height = "700px")
            )
        ),
        fluidRow(
            column(width = 1, div("Date: ", style = "font-size:20px;")),
            column(
                width = 2, 
                div(textOutput(outputId = ns("date")), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Total Cases: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns("cases")), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Cases Today: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns("daily_cases")), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Total Deaths: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns("deaths")), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Deaths Today: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = ns("daily_deaths")), style = "font-size:20px; color:red")
                ),
            style = "padding-top:15px"
        )
    )
}


introServer <- function(id, data, date, cases, daily_cases, deaths, daily_deaths) {
    moduleServer(
        id, 
        function(input, output, session) {
            output[["plot"]] <- renderPlotly({
                daily_barplot(
                    data, 
                    x = "Date", 
                    y = "Scotland",
                    roll_ave = FALSE
                )
            })
            output[["date"]] <- renderText({date})
            output[["cases"]] <- renderText({cases})
            output[["daily_cases"]] <- renderText({daily_cases})
            output[["deaths"]] <- renderText({deaths})
            output[["daily_deaths"]] <- renderText({daily_deaths})
        }
    )
}