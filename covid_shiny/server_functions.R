tidy_trend_excel_sheets <- function(sheets) {
    # Filter to only tables and remove columns with all NAs
    sheets <- sheets[grep("Table", names(sheets))]
    sheets <- map(sheets, function(i) select_if(i, ~sum(!is.na(.)) > 0))

    final_sheets <- list()

    # NHS 24 stats
    final_sheets[["Table 1 - NHS 24"]] <- tidy_table(
        df = sheets[[grep("Table 1 - NHS 24", names(sheets))]],
        row = 3
    )

    # Hospital care stats
    first_cat <- sheets[[grep("Table 2 - Hospital Care", names(sheets))]][[2, 2]] %>% 
        str_remove("\\(i\\) |\\(ii\\) ")
    second_cat <- sheets[[grep("Table 2 - Hospital Care", names(sheets))]][[2, 5]] %>% 
        str_remove("\\(i\\) |\\(ii\\) ")
            
    final_sheets[["Table 2 - Hospital Care"]] <- sheets[[grep("Table 2 - Hospital Care", names(sheets))]] %>% 
        slice(4:nrow(.)) %>% 
        set_names(gsub("\\r|\\n", "", c("Date", paste(first_cat, c("Confirmed", "Suspected", "Total")), paste(second_cat, c("Confirmed", "Suspected", "Total"))))) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, as.numeric)
    
    # Ambulance stats
    final_sheets[["Table 3 - Ambulance"]] <- tidy_table(
        df = select(sheets[[grep("Table 3 - Ambulance", names(sheets))]], -1),
        row = 3
    )

    # Delayed discharges 
    final_sheets[["Table 4 - Delated Discharges"]] <- tidy_table(
        df = select(sheets[[grep("Table 4 - Delayed Discharges", names(sheets))]], -1),
        row = 3
    )

    # # Testing 
    final_sheets[["Table 5 - Testing"]] <- sheets[[grep("Table 5 - Testing", names(sheets))]] %>% 
        select(-(ncol(.))) %>% 
        set_names(c("Date", "Negative", "Positive", "Total", "Daily_Positive", paste("NHS_labs", c("Daily", "Cumulative"), sep = "_"), paste("Regional_Centres", c("Daily", "Cumulative"), sep = "_"))) %>% 
        slice(4:nrow(.)) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, as.numeric)

    # Workforce absences
    final_sheets[["Table 6 - Workforce"]] <- tidy_table(
        df = sheets[[grep("Table 6 - Workforce", names(sheets))]],
        row = 2
    )

    # Care homes
    final_sheets[["Table 7a - Care Homes"]] <- tidy_table(
        df = sheets[[grep("Table 7a - Care Homes", names(sheets))]],
        row = 3
    )

    # Care home workforce
    final_sheets[["Table 7b - Care Home Workforce"]] <- tidy_table(
        df = sheets[[grep("Table 7b - Care Home Workforce", names(sheets))]],
        row = 2
    )

    # Deaths
    final_sheets[["Table 8 - Deaths"]] <- tidy_table(
            df = sheets[[grep("Table 8 - Deaths", names(sheets))]],
            row = 3
        ) %>% 
        find_daily_increase(df = ., column = setdiff(colnames(.), "Date"))

    names(final_sheets) <- names(sheets)
    final_sheets
}

tidy_table <- function(df, row) {
    col_names <- na.omit(unlist(slice(df, row-1)))
    df %>% 
        slice(row:nrow(.)) %>% 
        select(1:length(col_names)) %>% 
        set_names(col_names) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, as.numeric) %>% 
        filter(rowSums(is.na(.)) != ncol(.))
}

daily_barplot <- function(df, x, y) {
    p <- ggplot(data = df, aes_string(x = x, y = y)) +
        geom_bar(stat = "identity", fill = "#619CFF") 
    ggplotly(p)
}

find_daily_increase <- function(df, column) {
    df[["Daily Change"]] <- df[[gsub("\`", "", column)]] - lag(df[[gsub("\`", "", column)]])
    df
}

cumulative_group_plot <- function(df, x, y) {
    df <- pivot_longer(df, -Date)
    p <- ggplot(data = df, aes_string(x = x, y = y, label1 = x, label2 = y, label3 = "name")) +
        geom_line(aes(color = fct_reorder2(name, .data[[x]], .data[[y]]), linetype = name)) +
        theme(legend.title = element_blank())
    ggplotly(p, tooltip = c("label1", "label2", "label3")) 
}

cumulative_plot <- function(df, x, y) {
    p <- ggplot(data = df, aes_string(x = x, y = y)) +
        geom_point() +
        geom_line()
    ggplotly(p)
}

stacked_barplot <- function(df, x, y) {
    p <- ggplot(data = df, aes_string(x = x, y = y, fill = "name")) +
            geom_bar(stat = "identity") +
        theme(legend.title = element_blank())
    ggplotly(p)
}

render_custom_datatable <- function(df, title) {
    DT::renderDataTable(
        df,
        extensions = c("Buttons", "Scroller", "FixedColumns"), 
        rownames = FALSE,
        options = list(
            dom = "tB",
            scrollY = 500,
            scrollX = TRUE,
            scroller = TRUE,
            buttons = list(list(extend = "excel", filename = title)),
            fixedColumns = list(leftColumn = 1)
        )
    )
}

decide_plotly_output <- function(data, input) {
    data <- select(data, Date, all_of(input))
    message(ncol(data))
    if (ncol(data) == 2) {
        daily_barplot(
            df = data, 
            x = "Date", 
            y = paste0("`", setdiff(input, "Date"), "`")
        )
    } else {
        cumulative_group_plot(
            df = data, 
            x = "Date", 
            y = "value"
        )
    } 
}