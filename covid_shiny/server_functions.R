tidy_trend_excel_sheets <- function(sheets) {
    # Filter to only tables and remove columns with all NAs
    sheets <- sheets[grep("Table", names(sheets))]
    sheets <- map(sheets, function(i) select_if(i, ~sum(!is.na(.)) > 0))

    # NHS 24 stats
    sheets[[grep("NHS 24", names(sheets))]] <- tidy_table(
        df = sheets[grep("NHS 24", names(sheets))][[1]],
        row = 3
    )

    # Hospital care stats
    first_cat <- sheets[[grep("Hospital Care", names(sheets))]][[2, 2]] %>% 
        str_remove("\\(i\\) |\\(ii\\) ")
    second_cat <- sheets[[grep("Hospital Care", names(sheets))]][[2, 5]] %>% 
        str_remove("\\(i\\) |\\(ii\\) ")
            
    sheets[[grep("Hospital Care", names(sheets))]] <- sheets[[grep("Hospital Care", names(sheets))]] %>% 
        slice(4:nrow(.)) %>% 
        set_names(gsub("\\r|\\n", "", c("Date", paste(first_cat, c("Confirmed", "Suspected", "Total")), paste(second_cat, c("Confirmed", "Suspected", "Total"))))) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, as.numeric)
    
    # Ambulance stats
    sheets[[grep("Ambulance", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Ambulance", names(sheets))]],
        row = 3
    )

    # Delayed discharges 
    sheets[[grep("Discharge", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Discharge", names(sheets))]],
        row = 3
    )

    # # Testing 
    sheets[[grep("Testing", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Testing", names(sheets))]],
        row = 4
    )


    # Workforce absences
    sheets[[grep("Workforce", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Workforce", names(sheets))]],
        row = 2
    )

    # Care homes
    sheets[[grep("Care Home", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Care Home", names(sheets))]],
        row = 3
    )

    # Deaths
    sheets[[grep("Deaths", names(sheets))]] <- tidy_table(
        df = sheets[[grep("Deaths", names(sheets))]],
        row = 3
    )

    sheets
}

tidy_table <- function(df, row) {
    col_names <- na.omit(unlist(slice(df, row-1)))
    if(any(grepl("tested", names(df)))) col_names <- prepend(col_names, "Date", 1)
    df %>% 
        slice(row:nrow(.)) %>% 
        select(1:length(col_names)) %>% 
        set_names(col_names) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, as.numeric)
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
    p <- ggplot(data = df, aes_string(x = x, y = y)) +
        geom_line(aes(color = name, linetype = name)) +
        theme(legend.title = element_blank())
    ggplotly(p)     
}