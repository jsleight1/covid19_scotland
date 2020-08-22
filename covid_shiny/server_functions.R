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
    final_sheets[["Table 2 - Hospital Care"]] <- sheets[[grep("Table 2 - Hospital Care", names(sheets))]] %>% 
        slice(4:nrow(.)) %>% 
        select(1, 2, 5) %>% 
        set_names(
            "Date", 
            "COVID-19 patients in ICU or combined ICU/HDU Confirmed",
            "COVID-19 patients in hopsital (including those in ICU) Confirmed"
        ) %>% 
        mutate_all(~round(as.numeric(.))) %>% 
        mutate(
            Date = excel_numeric_to_date(Date),
            `Daily Change in Intensive Care Confirmed` = `COVID-19 patients in ICU or combined ICU/HDU Confirmed` - 
                lag(`COVID-19 patients in ICU or combined ICU/HDU Confirmed`),
            `Daily Change in Total Hospital Patients Confirmed` = `COVID-19 patients in hopsital (including those in ICU) Confirmed` - 
                lag(`COVID-19 patients in hopsital (including those in ICU) Confirmed`)
        )

    # Ambulance stats
    final_sheets[["Table 3 - Ambulance"]] <- tidy_table(
        df = select(sheets[[grep("Table 3 - Ambulance", names(sheets))]], -1),
        row = 3
    )

    # Delayed discharges 
    final_sheets[["Table 4 - Delayed Discharges"]] <- tidy_table(
            df = select(sheets[[grep("Table 4 - Delayed Discharges", names(sheets))]], -1),
            row = 3
        )

    # Testing 
    final_sheets[["Table 5 - Testing"]] <- sheets[[grep("Table 5 - Testing", names(sheets))]] %>% 
        set_names(
            c("Date", "Negative", "Positive", "Total", "Daily Positive", 
            paste("NHS labs", c("Daily", "Cumulative"), sep = " "), 
            paste("Regional Centres", c("Daily", "Cumulative"), sep = " "),
            c("Total daily tests", "People tested in last 7 days", "Positive cases in last 7 days", "Tests in last 7 days")
        )) %>% 
        slice(4:nrow(.)) %>% 
        mutate_all(~round(as.numeric(.))) %>% 
        mutate(
            Date = excel_numeric_to_date(Date), 
            `NHS % Positive` = round(`Daily Positive` / `NHS labs Daily` * 100, 2),
            `Daily Negative` = Negative - lag(Negative)
        ) %>% 
        select(Date, Negative, `Daily Negative`, Positive, 
            `Daily Positive`, `NHS % Positive`, everything())

    # Workforce absences
    cols <- na.omit(unlist(slice(sheets[[grep("Table 6 - Workforce", names(sheets))]], 1)))
    final_sheets[["Table 6 - Workforce"]] <- sheets[[grep("Table 6 - Workforce", names(sheets))]] %>% 
        slice(grep("Weekly", .data$"Table 6 - Number of NHS staff reporting as absent due to Covid-19") + 1:nrow(.)) %>% 
        set_names(cols) %>% 
        mutate(Date = lubridate::dmy(gsub("week to ", "", .data$Date))) %>% 
        mutate_if(is.character, ~round(as.numeric(.), 2)) 
    
    # Care homes
    cols <- unlist(slice(sheets[[grep("Table 7a - Care Homes", names(sheets))]], 2))
    final_sheets[["Table 7a - Care Homes"]] <- sheets[[grep("Table 7a - Care Homes", names(sheets))]] %>% 
        slice(3:nrow(.)) %>% 
        set_names(cols) %>% 
        mutate_at(c(1, 3), as.numeric) %>% 
        mutate(Week = factor(Week, levels = .data$Week)) %>% 
        select(Week, everything())

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
        mutate(`Daily Deaths` = `Number of COVID-19 confirmed deaths registered to date` - 
            lag(`Number of COVID-19 confirmed deaths registered to date`)
        )

    final_sheets
}

tidy_table <- function(df, row) {
    col_names <- na.omit(unlist(slice(df, row-1)))
    df %>% 
        slice(row:nrow(.)) %>% 
        select(1:length(col_names)) %>% 
        set_names(col_names) %>% 
        mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>% 
        mutate_if(is.character, ~round(as.numeric(.), 2)) %>% 
        filter(rowSums(is.na(.)) != ncol(.))
}

daily_barplot <- function(df, x, y) {
    p <- ggplot(data = df, aes_string(x = x, y = y)) +
        geom_bar(stat = "identity", fill = "#619CFF") +
        theme(axis.text.x = element_text(angle = xlab_angle(df, x)))
    ggplotly(p)
}

cumulative_group_plot <- function(df, x) {
    df <- pivot_longer(df, -x)
    p <- ggplot(data = df, aes_string(x = x, y = "value", label1 = x, label2 = "value", label3 = "name")) +
        geom_line(aes(group = name, color = fct_reorder2(name, .data[[x]], .data[["value"]]), linetype = name)) +
        theme(
            legend.title = element_blank(),
            axis.text.x = element_text(angle = xlab_angle(df, x))
        )
    ggplotly(p, tooltip = c("label1", "label2", "label3")) 
}

cumulative_plot <- function(df, x, y) {
    p <- ggplot(data = df, aes_string(x = x, y = y)) +
        geom_point(colour = "#619CFF") +
        geom_line(aes(group = 1), colour = "#619CFF") +
        theme(axis.text.x = element_text(angle = xlab_angle(df, x)))
    ggplotly(p)
}

stacked_barplot <- function(df, x) {
    p <- ggplot(data = df, aes_string(x = x, y = "value", fill = "name")) +
            geom_bar(stat = "identity") +
        theme(
            legend.title = element_blank(), 
            axis.text.x = element_text(angle = xlab_angle(df, x))
        )
    ggplotly(p)
}

xlab_angle <- function(df, x) {
    if (nchar(as.character(df[[x]][1])) > 8) 90
    else 0
}

render_custom_datatable <- function(df, title, ...) {
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
        ), 
        ...
    )
}

decide_plotly_output <- function(data, input, type, x, first_col) {
    # browser()
    data <- select(data, all_of(c(first_col, input)))
    if (ncol(data) == 2) {
        switch(type,
            "Bar Plot" = daily_barplot(df = data, x = x, y = paste0("`", input, "`")),
            "Line Plot" = cumulative_plot(df = data, x = x, y = paste0("`", input, "`"))
        )
    } else {
        switch(type,
            "Stacked Barplot" = stacked_barplot(pivot_longer(data, -x), x = x),
            "Line Plot" = cumulative_group_plot(df = data, x = x) 
        )
    } 
}

decide_checkbox_output <- function(data, input, id, ...) {
    data <- select(data, all_of(input))
    if (ncol(data) == 1) {
        radioButtons(
            inputId = id,
            label = "Select Plot Type",
            choices = c("Bar Plot", "Line Plot"),
            inline = TRUE,
            width = "100%",
            ...          
        )  
    } else {
        radioButtons(
            inputId = id,
            label = "Select Plot Type",
            choices = c("Line Plot", "Stacked Barplot"),
            inline = TRUE,
            width = "100%",
            ...            
        )  
    }
}