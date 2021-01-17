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
                # plotUI(ns("plot1")), 
                actionButton(ns("addPlot"), "Add new plot panel"),
                # actionButton(ns("removePlot"), "Remove plot panel"),
                tags$div(id = gsub(" ", "_", ns("newplot")))
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
            output[["download"]] <- downloadHandler(
                filename = str_c(id, ".tsv"), 
                content = function(file) {
                    write_tsv(table, file)
                }
            )
            # plotServer("plot1", table = table, x = x, roll_ave = roll_ave)
            plots <- c()
            observeEvent(input[["addPlot"]], {
                browser()
                plotid <- paste0(gsub(" ", "_", ns("newplot")), input[["addPlot"]])
                insertUI(
                    selector = paste0("#", gsub(" ", "_",ns("newplot"))),
                    ui = plotUI(ns(plotid))
                )
                plotServer(
                    id = plotid, 
                    table = table, 
                    x = x, 
                    roll_ave = roll_ave
                )
                plots <<- c(plotid, plots)
            })
            # observeEvent(input[["removePlot"]], {
            #     removeUI(
            #         selector = paste0("#", plots[length(plots)])
            #     )
            #     plots <<- plots[-length(plots)]
            # })
        }
    ) 
}