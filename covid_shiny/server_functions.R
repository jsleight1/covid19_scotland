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
    p <- ggplot(data = df, aes_string(x = x, y = y, label1 = x, label2 = y)) +
        geom_bar(stat = "identity", fill = "#619CFF") +
        theme(axis.text.x = element_text(angle = xlab_angle(df, x)))
    ggplotly(p, tooltip = c("label1", "label2"))
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
    if (nchar(as.character(df[[x]][1])) > 8 & class(df[[x]]) != "Date") 90
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