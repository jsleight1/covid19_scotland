#UI
introUI <- function(id) {
    ns <- NS(id)
    tabPanel(id,
        fluidRow(
            column(
                width = 12, div("National Cumulative Cases", style = "font-size:20px;"), 
                plotlyOutput(outputId = ns(paste0(id, "_plot")), height = "500px")
            )
        ),
        fluidRow(
            column(width = 1, div("Date: ", style = "font-size:20px;")),
            column(
                width = 2, 
                div(textOutput(outputId = ns(paste0(id, "date"))), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Total Cases: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns(paste0(id, "cases"))), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Cases Today: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns(paste0(id, "daily_cases"))), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Total Deaths: ", style = "font-size:20px;")),
            column(
                width = 1,
                div(textOutput(outputId = ns(paste0(id, "deaths"))), style = "font-size:20px; color:red")
            ),
            column(width = 1, div("Deaths Today: ", style = "font-size:20px;")),
                column(
                    width = 1,
                    div(textOutput(outputId = ns(paste0(id, "daily_deaths"))), style = "font-size:20px; color:red")
                ),
            style = "padding-top:15px"
        ),
        fluidRow(
            div(
                "NOTE: As of 15 June, confirmed cases include confirmed cases at 
                UK government regional testing centres. Previous data have not 
                been updated to account for this hence the significant increase 
                in testing capacity and positive cases on this day."
            ),
            div(
                "DISCLAIMER: This app is specifically designed as a visualisation tool
                that utilises public COVID-19 data and therefore should 
                not be used for any official decision making. Links to data sources can 
                be found under the references tab and should be consulted to further 
                understand the nuances and exact meaning of the data presented."
            ),
            style = "padding-top:15px; color:red"
        )
    )
}


introServer <- function(id, data, date, cases, daily_cases, deaths, daily_deaths) {
    moduleServer(
        id, 
        function(input, output, session) {
            output[[paste0(id, "_plot")]] <- renderPlotly({
                daily_barplot(
                    data, 
                    x = "Date", 
                    y = "Scotland",
                    roll_ave = TRUE
                )
            })
            output[[paste0(id, "date")]] <- renderText({date})
            output[[paste0(id, "cases")]] <- renderText({cases})
            output[[paste0(id, "daily_cases")]] <- renderText({daily_cases})
            output[[paste0(id, "deaths")]] <- renderText({deaths})
            output[[paste0(id, "daily_deaths")]] <- renderText({daily_deaths})
        }
    )
}