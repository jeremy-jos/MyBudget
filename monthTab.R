### Month Tab Module ###

monthTab <- function(input, output, session, expenses, income, categories, balance, month, previous_month){
  
  ns <- session$ns
  
  monthly_expenses <- reactive({
    expenses()[expenses()$Months == month,]
  })
  
  monthly_income <- reactive({
    income()[income()$Months == month,]
  })
  
  monthly_balance <- reactive({
    balance()[months(balance()$Dates) == month,]
  })
  
  previous_month_monthly_balance <- reactive({
    balance()[months(balance()$Dates) == previous_month,]
  })
  
  
  # table for monthly summary
  
  monthly_summary_table <- reactive({
    balance_beg <- ifelse(!is.null(previous_month), tail(previous_month_monthly_balance(), n=1)$Balance, head(monthly_balance(), n=1)$Balance)
    validate(need(Sys.Date()>=head(monthly_balance(), n=1)$Dates, "No data for this month yet."))
    balance_end <- tail(monthly_balance(), n=1)$Balance
    total_expenses <- sum(monthly_expenses()$Amount)
    total_income <- sum(monthly_income()$Amount)
    monthly_summary <- c(balance_beg, total_income, total_expenses, balance_end)
    if(months(Sys.Date())==month){
      monthly_summary <- data.frame(Summary = c("Initial Balance", "Total Income", "Total Expenses", 
                                        "Current Balance"), Value = monthly_summary)
    } else {
    monthly_summary <- data.frame(Summary = c("Initial Balance", "Total Income", "Total Expenses", 
                                        "End Balance"), Value = monthly_summary)
    }
    monthly_summary
  })
  output$monthlysummary <- renderDataTable(
    datatable(monthly_summary_table(), rownames = FALSE, colnames = "", options = list(dom = 't', ordering = FALSE)) 
    %>% formatCurrency(c("Value"))
  )
  
  # table for monthly overview
  
  monthly_expenses_sum_by_type <- reactive({
    temp <- as.data.frame(aggregate(monthly_expenses()$Amount, by=list(Type=monthly_expenses()$Type), FUN=sum))
    ordered <- temp[match(categories()$Type, temp$Type),]
    for(i in 1:length(ordered[,2])){
      if (is.na(ordered[i,2])==TRUE){
        ordered[i,2] <- 0
        }
    }
    as.data.frame(ordered[,2])
  })
  
  monthly_budget <- reactive({
    index <- which(names(categories())==paste0("Budget.", month))
    as.data.frame(categories()[,index])
  })
  
  monthly_difference <- reactive({
     as.data.frame(round(monthly_budget()-monthly_expenses_sum_by_type(), digits = 2))
  })
  
  monthly_overview_table <- reactive({
    validate(need(Sys.Date()>=head(monthly_balance(), n=1)$Dates, ""))
    if(input$displayChoice == "Type"){
      to_show <- as.data.frame(cbind(categories()$Type, monthly_budget(), 
                   monthly_expenses_sum_by_type(), monthly_difference()))
      colnames(to_show) <- c("Type", "Budget", "Spent", "Difference")
      if(input$removeRent==TRUE){
        to_show <- to_show[!(to_show$Type == "Rent"),]
        }
    } else if(input$displayChoice == "Category"){
      cat_to_show <- unique(categories()$Category)
      budget_to_show <- as.data.frame(aggregate(monthly_budget(), by=list(Category=categories()$Category), FUN=sum))
      budget_to_show <- as.data.frame(budget_to_show[match(cat_to_show, budget_to_show$Category),][,2])
      spent_to_show <- as.data.frame(aggregate(monthly_expenses_sum_by_type(), by=list(Category=categories()$Category), FUN=sum))
      spent_to_show <- as.data.frame(spent_to_show[match(cat_to_show, spent_to_show$Category),][,2])
      diff_to_show <- as.data.frame(aggregate(monthly_difference(), by=list(Category=categories()$Category), FUN=sum))
      diff_to_show <- as.data.frame(diff_to_show[match(cat_to_show, diff_to_show$Category),][,2])
      to_show <- as.data.frame(cbind(cat_to_show, budget_to_show, 
                                     spent_to_show, diff_to_show))
      colnames(to_show) <- c("Category", "Budget", "Spent", "Difference")
      if(input$removeRent==TRUE){
        to_show <- to_show[!(to_show$Category == "Housing"),]
      }
    }
    to_show
  })
  
  monthly_overview_table_with_total <- reactive({
    if(input$displayChoice == "Type"){
      lastline <- data.frame(Type = "TOTAL", Budget = sum(monthly_overview_table()$Budget), 
                             Spent = sum(monthly_overview_table()$Spent), Difference = sum(monthly_overview_table()$Difference))
    } else if(input$displayChoice == "Category"){
      lastline <- data.frame(Category = "TOTAL", Budget = sum(monthly_overview_table()$Budget), 
                             Spent = sum(monthly_overview_table()$Spent), Difference = sum(monthly_overview_table()$Difference))
    }
    as.data.frame(rbind(monthly_overview_table(), lastline))
  })
  
  output$monthlyoverviewtable <- renderDataTable(datatable(monthly_overview_table_with_total(), rownames = FALSE, filter = 'none',
                                                           options = list(dom = 't', pageLength = length(monthly_overview_table_with_total()[,1]), 
                                                                          ordering = FALSE)) 
                                                 %>% formatCurrency(c("Budget", "Spent", "Difference"))
                                                 %>% formatStyle("Difference", color = styleInterval(c(-0.001,0.001), c('red', 'gray', 'green')))
                                                 %>% formatStyle(
                                                   1,
                                                   target = 'row',
                                                   fontWeight = styleEqual("TOTAL", "bold"))
                                                )
  
  
  # Bar Plot and Pie Chart for monthly overview
  
  output$monthly_overview_plot <- renderPlotly({
    
    if(input$displayChoice == "Type"){
    p <- ggplot(monthly_overview_table(), aes(x=monthly_overview_table()$Type, 
           y=monthly_overview_table()$Spent, fill = monthly_overview_table()$Type,
           text = paste('Type: ', monthly_overview_table()$Type,
                        '<br>Amount Spent:', monthly_overview_table()$Spent))) + 
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
      scale_fill_discrete(name="Types") +
      labs(x = "Types", y = "")
    ggplotly(p, tooltip = c("text"))
    } else if(input$displayChoice == "Category"){
      p <- ggplot(monthly_overview_table(), aes(x=monthly_overview_table()$Category, 
             y=monthly_overview_table()$Spent, fill = monthly_overview_table()$Category,
             text = paste('Type: ', monthly_overview_table()$Category,
                          '<br>Amount Spent:', monthly_overview_table()$Spent))) + 
        geom_bar(stat="identity") +
        theme_minimal() +
        theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
        scale_fill_discrete(name="Categories") +
        labs(x = "Categories", y = "")
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  
  
    output$monthly_overview_pie <- renderPlot(
    if(input$displayChoice == "Type"){
      ggplot(data = monthly_overview_table(), aes(x="", y= monthly_overview_table()$Spent, 
                                                  fill = monthly_overview_table()$Type)) +
        geom_bar(width = 1, color = "white", stat = "identity") +
        coord_polar(theta = "y") + 
        theme_void() +
        scale_fill_discrete(name="Types")
    } else if(input$displayChoice == "Category"){
      ggplot(data = monthly_overview_table(), aes(x="", y= monthly_overview_table()$Spent, 
                                                  fill = monthly_overview_table()$Category)) +
        geom_bar(width = 1, color = "white", stat = "identity") +
        coord_polar(theta = "y") + 
        theme_void() +
        scale_fill_discrete(name="Categories")
    }
    )
  
  
  #table for all monthly expenses
  monthly_expenses_table <- reactive({
    data.frame(Dates = as.Date(monthly_expenses()$Dates, origin = "1899-12-30"), 
                                       Expense = monthly_expenses()$Expense, Type = monthly_expenses()$Type, 
                                       Amount = monthly_expenses()$Amount)
  })
  output$monthlyexpensetable <- renderDataTable(datatable(monthly_expenses_table(), rownames = FALSE, filter = 'top',
                                                          options = list(paging = FALSE, searching = FALSE))
                                                          %>% formatCurrency(c("Amount")))

  
  #table for monthly income
  monthly_income_table <- reactive({
    data.frame(Dates = as.Date(monthly_income()$Dates, origin = "1899-12-30"), 
               Income = monthly_income()$Income, Source = monthly_income()$Source, 
               Amount = monthly_income()$Amount)
  })
  output$monthlyincometable <- renderDataTable(datatable(monthly_income_table(), rownames = FALSE, filter = 'none',
                                                          options = list(dom = 't')) %>% formatCurrency(c("Amount")))

}


monthTabUI <- function(id) {
  
  
  ns <- NS(id)
  tagList(
    br(),
    br(),
    tabBox(
      title = "",
      id = "monthtab", width = "65%", 
      tabPanel("Monthly Overview",
               br(),
               fluidRow(
                 column(12,
                        box(title = "Monthly Summary", width = NULL, status = "primary", solidHeader = TRUE,
                          fluidRow(
                          column(5, tags$div(class = "monthlysumtable", dataTableOutput(ns("monthlysummary")))),
                          column(6, offset = 1,  
                                 br(),
                                 fluidRow(
                                   column(6, 
                                      radioButtons(ns("displayChoice"), "Group Expenses By", inline = TRUE, 
                                              choices = c("Type" = "Type", "Category" = "Category"), selected = "Type")),
                                    column(6,
                                      radioButtons(ns("plotChoice"), "Plot Type", inline = TRUE,
                                              choices = c("Bar Plot" = "barplot", "Pie Chart" = "piechart")))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(11, offet = 1,
                                          checkboxInput(ns("removeRent"), label = "Remove Rent / Housing from the Overview Plots and Tables", 
                                                        value = FALSE))
                                 )
                                )
                          ),br()
                        )
                      )
               ),
               fluidRow(
               column(5, box(title = "Expenses Overview Table", width = NULL, status = "primary", solidHeader = TRUE,
                             dataTableOutput(ns("monthlyoverviewtable")))),
               column(7, box(title = "Expenses Overview Plots", width = NULL, status = "primary", solidHeader = TRUE,
                             conditionalPanel(
                               condition = paste0("input['", ns("plotChoice"),"'] == 'barplot'"),
                              plotlyOutput(ns("monthly_overview_plot"))
                             ),
                             conditionalPanel(
                               condition = paste0("input['", ns("plotChoice"),"'] == 'piechart'"),
                               plotOutput(ns("monthly_overview_pie"))
                              )))
               )
              ),
      tabPanel("Expenses",
               dataTableOutput(ns("monthlyexpensetable"))),
      tabPanel("Income",
               dataTableOutput(ns("monthlyincometable")))
    )
  )

} 


