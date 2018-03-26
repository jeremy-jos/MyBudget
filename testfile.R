### Test File ###

library(openxlsx)
library(janitor)
library(ggplot2)
library(ggmap)


import_expense_data <- read.xlsx("/Users/jeremyjoslove/Documents/Coding/MyBudget/dataset.xlsx", sheet = 1)
import_expense_categories <- read.xlsx("/Users/jeremyjoslove/Documents/Coding/MyBudget/dataset.xlsx", sheet = 2)
import_income_data <- read.xlsx("/Users/jeremyjoslove/Documents/Coding/MyBudget/dataset.xlsx", sheet = 3)
import_income_categories <- read.xlsx("/Users/jeremyjoslove/Documents/Coding/MyBudget/dataset.xlsx", sheet = 4)

expense_dates <- import_expense_data[,1]
expense_months <- months(as.Date(expense_dates, origin = "1899-12-30"))
expense_description <- import_expense_data[, 2]
expense_type <- import_expense_data[, 3]
expense_amount <- import_expense_data[, 4]
expenses <- data.frame(Dates = expense_dates, Expense = expense_description, Months = expense_months, Type = expense_type, Amount = expense_amount)

expense_different_types <- import_expense_categories[,1]
expense_different_categories <- import_expense_categories[,2]
expense_different_types_categories <- data.frame(Types = expense_different_types, Categories = expense_different_categories)

income_dates <- import_income_data[,1]
income_description <- import_income_data[,2]
income_source <- import_income_data[,3]
income_amount <- import_income_data[,4]
income <- data.frame(Dates = income_dates, Income = income_description, Source = income_source, Amount = income_amount)

income_sources <- data.frame(Sources = import_income_categories[,1])


balance_dates <- data.frame(Dates = expense_dates)
balance <- data.frame(-expense_amount)
for (i in (1:length(income_dates))){
  if(income_dates[[i]] <= balance_dates[1,]){
    balance <- rbind(income_amount[[i]], balance)
    balance_dates <- rbind(income_dates[[i]], balance_dates)
  } else {
    cutoff_date_index <- which(balance_dates==income_dates[[i]])
    temp1_balance <- data.frame(amount = balance[1:cutoff_date_index-1,])
    temp2_balance <- data.frame(amount = balance[cutoff_date_index: length(balance[,1]),])
    temp1_balance_dates <- data.frame(dates = balance_dates[1:cutoff_date_index-1,])
    temp2_balance_dates <- data.frame(dates = balance_dates[cutoff_date_index:length(balance[,1]),])
    balance <- rbind(temp1_balance, income_amount[[i]], temp2_balance)
    balance_dates <- rbind(temp1_balance_dates, income_dates[[i]], temp2_balance_dates)
  }
}
balance = cumsum(balance)
balance_dates = balance_dates = as.Date(balance_dates$dates, origin = "1899-12-30")
balance_dataset <- data.frame(Dates = balance_dates, Balance = balance)
balance_dataset[months(balance_dates)=='août',]
