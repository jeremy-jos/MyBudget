### MyBudget App Server Side ###

library(shiny)
library(shinydashboard)
#library(openxlsx)
library(janitor)
library(ggplot2)
library(plotly)
library(ggrepel)
library(scales)
library(DT)
library(data.table)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(XLConnect)


function(input, output, session) { 
  
  ## Importing data
  
  data_path <- "/Users/jeremyjoslove/Documents/Coding/MyBudget/dataset.xlsx"
  
  import_expense_data <- read.xlsx(data_path, sheet = 1)
  import_expense_categories <- read.xlsx(data_path, sheet = 2)
  import_income_data <- read.xlsx(data_path, sheet = 3)
  import_income_categories <- read.xlsx(data_path, sheet = 4)
  
  expense_dates <- import_expense_data[, 1]
  expense_months <- months(as.Date(expense_dates, origin = "1899-12-30"))
  expense_description <- import_expense_data[, 2]
  expense_type <- import_expense_data[, 3]
  expense_amount <- import_expense_data[, 4]
  
  expense_different_types <- import_expense_categories[,1]
  expense_different_categories <- import_expense_categories[,2]
  expense_different_budgets <- import_expense_categories[,3:length(import_expense_categories)]
  
  income_dates <- import_income_data[,1]
  income_months <- months(as.Date(income_dates, origin = "1899-12-30"))
  income_description <- import_income_data[,2]
  income_source <- import_income_data[,3]
  income_amount <- import_income_data[,4]
  init_balance <- import_income_data[1,5]
  
  expense_categories <- rep(0, length(expense_dates))
  for (i in (1:length(expense_categories))){
    for (j in (1:length(expense_different_categories))){
      if(expense_type[[i]]==expense_different_types[[j]]){
        expense_categories[[i]] <- expense_different_categories[[j]]
      }
    }
  }
  
  ## Creating Datasets based on imported data and setting reactive values
  
  values <- reactiveValues()
  
  values$expense_types_categories <- data.frame(Type = expense_different_types, Category = expense_different_categories, 
                                         expense_different_budgets)
  
  values$expenses <- data.frame(Dates = expense_dates, Months = expense_months, Expense = expense_description, 
                                Type = expense_type, Category = expense_categories, Amount = expense_amount)
    
  values$income <- data.frame(Dates = income_dates, Months = income_months, Income = income_description, 
                              Source = income_source, Amount = income_amount)
  
  values$expenses_to_plot <- NULL
  values$expenses_to_plot2 <- NULL # for second pie chart that is plotted at the same time as the first
  values$expenses_table_to_show <- NULL
  
  
  ## Select Inputs for adding Expense / Income
  
  output$new_expense_type <- renderUI({
    selectInput("new_expense_types", label = "Type", choices = expense_different_types, selected = "Take Out")
  })
  output$new_income_source <- renderUI({
    selectInput("new_income_source", label = "Source", choices = unique(income_source), selected = "Salary")
  })
  
  
  ## Adding an Expense
  
  
  observeEvent(input$addexp,{
    #withBusyIndicatorServer("addexp", {
  
    new_expense_category <- as.character(values$expense_types_categories$Category
                                         [values$expense_types_categories$Type == input$new_expense_types])
    
    
    validate(
      need(is.numeric(input$new_expense_amount), "Amount not a number"))
    new_expense <- data.frame(Dates = as.numeric(input$new_expense_date + 70*365+19), 
                              Months = months(as.Date(input$new_expense_date, origin = "1899-12-30")),
                         Expense = input$new_expense_description, Type = input$new_expense_types, 
                         Category = new_expense_category, Amount = as.numeric(input$new_expense_amount))
    values$expenses <- rbind(values$expenses,new_expense)
    
    expense_to_save <- data.frame(Date = input$new_expense_date, Description = new_expense$Expense, Type = new_expense$Type,
                                  Amount = new_expense$Amount)
    
    wb <- loadWorkbook("dataset.xlsx", create = FALSE)
    writeWorksheet(wb, expense_to_save, sheet="Expenses",   
                   startRow=200 #getLastRow(wb, "Expenses")+1
                   , startCol=1,            
                   header=FALSE)                      
    saveWorkbook(wb)
    #})
    
    #xlsx::write.xlsx(expense_to_save, "test3.xlsx", sheetName = "Expenses", col.names=FALSE, row.names=FALSE,
    #                 append=TRUE, showNA=TRUE)
    
      #})
    })
  
  
  ## Adding an Income
  
  
  observeEvent(input$addinc,{
    #withBusyIndicatorServer("addinc", {
    
    validate(need(is.numeric(input$new_income_amount), "Amount not a number"))
    new_income <- data.frame(Dates = as.numeric(input$new_income_date + 70*365+19), Months = months(as.Date(input$new_income_date, 
                                                                                                origin = "1899-12-30")),
                             Income = input$new_income_description, Source = input$new_income_source, 
                             Amount = as.numeric(input$new_income_amount))
    
    values$income <- rbind(values$income, new_income)
    
  })
  
  
  ## Bank balance dataset
  
  balance_dataset <- reactive({
  
    balance_dates_raw <- data.frame(Dates = append(values$expenses$Dates, values$income$Dates))
    balance_amount_raw <- data.frame(Balance = append(-values$expenses$Amount, values$income$Amount))
    
    balance_dataset <- data.frame(Dates = balance_dates_raw, Balance = balance_amount_raw)
    balance_dataset <- balance_dataset[order(balance_dataset$Dates),]
    balance_dataset$Balance <- cumsum(balance_dataset$Balance)
    balance_dataset$Dates <- as.Date(balance_dataset$Dates, origin = "1899-12-30")
  
    init <- data.frame(Dates = head(balance_dataset$Dates, n=1), Balance = init_balance)
    balance_dataset <- data.frame(rbind(init, balance_dataset))
    balance_dataset
  
})
  
  ## Balance Plot
  output$balance <- renderPlotly({
    
    Date <- balance_dataset()$Dates     # for ggplotly tooltip because the "text" technique won't work for here
    Balance <- balance_dataset()$Balance
    
    p <- ggplot(data = balance_dataset(), aes(x=Date, y=Balance)) + 
    geom_line(size = 1, color = "aquamarine2") +
    theme_minimal() +
    labs(x = NULL, y = NULL)
    ggplotly(p)
  })
  
  ## Summary
  
  output$summary <- renderDataTable({
    
    balance_beg <- head(balance_dataset()$Balance, n=1) # income_amount[[1]]+ income_amount[[2]]
    total_income <- tail(cumsum(values$income$Amount), n=1)  # - balance_beg
    total_expense <- tail(cumsum(values$expenses$Amount), n=1) 
    balance_actual <- tail(balance_dataset()$Balance, n=1)
    summary <- c(balance_beg, total_income, total_expense, balance_actual)
    summary <- data.frame(Summary = c("Initial Balance", "Total Income", "Total Expenses", "Current Balance"), Value = summary)
  
    datatable(summary, rownames = FALSE, colnames = "", options = list(dom = 't', ordering = FALSE)) %>% formatCurrency(c("Value"))
  
  })
  
  ## Yearly bar charts
  
  output$barchart <- renderPlotly({
    
  # total year by type
    if(input$barplotchoice == "yearly" & input$barplotgroupchoice == "type"){
      values$expenses_to_plot <- values$expenses
      if(input$removeRent == TRUE){
        values$expenses_to_plot <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Type == "Rent"),])
      }
      p <- ggplot(values$expenses_to_plot, aes(Type, Amount, fill = Type)) + 
      geom_bar(stat="summary", fun.y="sum") + 
      theme_minimal() + 
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
      labs(x = "Types", y = "")
      ggplotly(p)
    } 
  
  # total year by category
  else if (input$barplotchoice == "yearly" & input$barplotgroupchoice == "category"){
    values$expenses_to_plot <- values$expenses
    if(input$removeRent == TRUE){
      values$expenses_to_plot <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Category == "Housing"),])
    }
      p <- ggplot(values$expenses_to_plot, aes(Category, Amount, fill = Category)) + 
        geom_bar(stat="summary", fun.y="sum") + 
        theme_minimal() + 
        theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
        labs(x = "Categories", y = "")
      ggplotly(p)
    }
  
  # monthly by type
    else if (input$barplotchoice == "monthly" & input$barplotgroupchoice == "type"){
      values$expenses_to_plot <- values$expenses
      if(input$removeRent == TRUE){
        values$expenses_to_plot <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Type == "Rent"),])
      }
      Month <- factor(values$expenses_to_plot$Months,levels=c("août", "septembre", "octobre", "novembre","décembre", 
                                                              "janvier", "février", "mars", "avril", "mai", "juin", "juillet"))
      p <- ggplot(values$expenses_to_plot, aes(Month, Amount, fill = Type)) + 
      geom_bar(stat="summary", fun.y="sum", position = "dodge") +
      labs(x = "", y = "") +
      theme_minimal()
      ggplotly(p)
    }
  
  
  # monthly by category
  else if (input$barplotchoice == "monthly" & input$barplotgroupchoice == "category"){
    values$expenses_to_plot <- values$expenses
    if(input$removeRent == TRUE){
      values$expenses_to_plot <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Category == "Housing"),])
    }
    Month <- factor(values$expenses_to_plot$Months,levels=c("août", "septembre", "octobre", "novembre","décembre", 
                                                            "janvier", "février", "mars", "avril", "mai", "juin", "juillet"))
    p <- ggplot(values$expenses_to_plot, aes(Month, Amount, fill = Category)) + 
      geom_bar(stat="summary", fun.y="sum", position = "dodge") +
      labs(x = "", y = "") +
      theme_minimal()
    ggplotly(p)
  }
  
  })
  
  
  ## Yearly Expenses Pie Charts
  output$yearlypiecharttype <- renderPlot({
    if(input$removeRent == TRUE){
      values$expenses_to_plot <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Type == "Rent"),])
    }
    p <- ggplot(data = values$expenses_to_plot, aes(x=factor(1), y= Amount, fill = Type)) +
      geom_bar(width = 1, color = "white", stat = "summary", fun.y = "sum") +
      coord_polar(theta = "y") + 
      theme_void() + ggtitle("Yearly Expenses by Type") +
      theme(plot.title = element_text(hjust = 0.5, lineheight=.8, face="bold"))
    p
  })
  
  output$yearlypiechartcategory <- renderPlot({
    values$expenses_to_plot2 <- values$expenses_to_plot
    if(input$removeRent == TRUE){
      values$expenses_to_plot2 <- isolate(values$expenses_to_plot[!(values$expenses_to_plot$Category == "Housing"),])
    }
    p <- ggplot(data = values$expenses_to_plot2, aes(x=factor(1), y= Amount, fill = Category)) +
      geom_bar(width = 1, color = "white", stat = "summary", fun.y = "sum") +
      coord_polar(theta = "y") + 
      theme_void() + ggtitle("Yearly Expenses by Category") +
      theme(plot.title = element_text(hjust = 0.5, lineheight=.8, face="bold"))
    p
  })
  
  
  ## Yearly Income Bar Charts
  
  output$yearlyincomebarchart <- renderPlotly({
  
  # Yearly by Type
  if (input$incomebarplotgroup == "yearly"){
    p <- ggplot(values$income, aes(Source, Amount, fill = Source)) + 
      geom_bar(stat="summary", fun.y="sum") + 
      theme_minimal() + 
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
      labs(x = "Sources", y = NULL)
    ggplotly(p)
  } 
  
  # Monthly by Type
  else if (input$incomebarplotgroup == "monthly"){
    Month <- factor(income_months,levels=c("août", "septembre", "octobre", "novembre","décembre", "janvier", "février",
                                           "mars", "avril", "mai", "juin", "juillet"))
    p <- ggplot(values$income, aes(Month,Amount, fill = Source)) + 
      geom_bar(stat="summary", fun.y="sum", position = "dodge") +
      labs(x = "", y = "") +
      theme_minimal()
    ggplotly(p)
  }
  
  })
  
  
  ## Yearly Income Pie Charts
  output$yearlyincomepiechart <- renderPlot(
      ggplot(data = values$income, aes(x=factor(1), y= Amount, fill = Source)) +
      geom_bar(width = 1, color = "white", stat = "summary", fun.y = "sum") +
      coord_polar(theta = "y") + 
      theme_void() + ggtitle("Income by Source") +
      theme(plot.title = element_text(hjust = 0.5, lineheight=.8, face="bold"))
  )
  
  
  ## Yearly expense table output 
  output$yearlyexpensetable <- renderDataTable({
    
    values$expenses_table_to_show <- data.frame(Dates = as.Date(values$expenses$Dates, origin = "1899-12-30"),  
                               Expense = values$expenses$Expense, Type = values$expenses$Type, 
                               Amount = values$expenses$Amount)
    
    datatable(values$expenses_table_to_show, rownames = FALSE, filter = 'top',
              options = list(paging = FALSE, searching = FALSE)) %>% formatCurrency(c("Amount"))
  })
  
  ## Yearly income table output
  output$yearlyincometable <- renderDataTable({
    
    values$income_table_to_show <- data.frame(Dates = as.Date(values$income$Dates, origin = "1899-12-30"),  
                             Income = values$income$Income, Source = values$income$Source, Amount = values$income$Amount)
  
    datatable(values$income_table_to_show, rownames = FALSE, filter = 'top',
              options = list(paging = FALSE, searching = FALSE)) %>% formatCurrency(c("Amount"))
    })

  
  
  ### Month Tabs  
  
  #August
  
  callModule(monthTab, "august", expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "août", previous_month = NULL)
  
  #September
  callModule(monthTab, "september", expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "septembre", previous_month = "août")
  
  #October
  callModule(monthTab, "october", expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "octobre", previous_month = "septembre")
  
  #November
  callModule(monthTab, "november", expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "novembre", previous_month = "octobre")
  
  #December
  callModule(monthTab, "december", expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "décembre", previous_month = "novembre")
  
  #January
  callModule(monthTab, "january",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "janvier", previous_month = "décembre")
  
  #February
  callModule(monthTab, "february",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "février", previous_month = "janvier")
  
  #March
  callModule(monthTab, "march",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "mars", previous_month = "février")
  
  #April
  callModule(monthTab, "april",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "avril", previous_month = "mars")
  
  #May
  callModule(monthTab, "may",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "mai", previous_month = "avril")
  
  #June
  callModule(monthTab, "june",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "juin", previous_month = "mai")
  
  #Juillet
  callModule(monthTab, "july",  expenses = reactive({values$expenses}), income = reactive({values$income}), 
             categories = reactive({values$expense_types_categories}), balance_dataset, 
             month = "juillet", previous_month = "juin")
  
  
  
  
}