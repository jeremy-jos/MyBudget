### MyBudget App UI Side ###

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

dashboardPage(
  dashboardHeader(title = "MyBudget"),
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Yearly Recap", tabName = "yearly", icon = icon("calendar")),
      hr(),
      menuItem("August", tabName = "august", icon = icon("sun-o")),
      menuItem("September", tabName = "september", icon = icon("briefcase")),
      menuItem("October", tabName = "october", icon = icon("leaf")),
      menuItem("November", tabName = "november", icon = icon("umbrella")),
      menuItem("December", tabName = "december", icon = icon("gift")),
      menuItem("January", tabName = "january", icon = icon("thermometer-empty")),
      menuItem("February", tabName = "february", icon = icon("plane")),
      menuItem("March", tabName = "march", icon = icon("bicycle")),
      menuItem("April", tabName = "april", icon = icon("slack")),
      menuItem("May", tabName = "may", icon = icon("tree")),
      menuItem("June", tabName = "june", icon = icon("sellsy")),
      menuItem("July", tabName = "july", icon = icon("thermometer-full"))
    )
  
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mybudgetcss.css")
    ),
    tabItems(
      tabItem(tabName = "yearly",
              fluidRow(
                column(5,
                       h2("Yearly Recap")),
                
                column(2, offset = 4, 
                       br(), 
                       br(), 
                       actionButton("addexplink","Add Expense / Income ", icon = icon("plus-circle")),
                       bsModal("addexpmodal", "Add an Expense", "addexplink", size = "small",
                               fluidRow(
                                 column(12, radioButtons("addchoice", "What would you like to do?", inline = TRUE, 
                                              choices = c("Add an Expense" = "addexpchoice", "Add an Income" = "addincchoice"))
                               )),
                               br(), 
                               conditionalPanel(
                                   condition = "input.addchoice == 'addexpchoice'",
                                   fluidRow(
                                   column(2, dateInput("new_expense_date", label = "Date")),
                                   column(4, textInput("new_expense_description", label = "Description")),
                                   column(4, uiOutput("new_expense_type")),
                                   column(2, numericInput("new_expense_amount", label = "Amount", value = 10.00, min = 0, step = 0.01))
                                   ),
                                   br(),
                                   br(),
                                   tags$div(#withBusyIndicatorUI(
                                     actionButton("addexp", "Add Expense")#)
                                     , align = "center")
                               ),
                               conditionalPanel(
                                 condition = "input.addchoice == 'addincchoice'",
                                 fluidRow(
                                   column(2, dateInput("new_income_date", label = "Date")),
                                   column(4, textInput("new_income_description", label = "Description")),
                                   column(4, uiOutput("new_income_source")),
                                   column(2, numericInput("new_income_amount", label = "Amount", value = 1380.02, min = 0, step = 0.01))
                                 ),
                                 br(),
                                 br(),
                                 tags$div(#withBusyIndicatorUI(
                                   actionButton("addinc", "Add Income")#)
                                   , align = "center")
                                 )
                                 
                       )
                )
                       
              ),
              fluidRow(
                br(),
                br(),
                tabBox(
                  id="yearly",
                  width = 12, 
                  tabPanel("Recap",
                           br(),
                           fluidRow(
                             column(4, box(title = "Summary", collapsible = TRUE, collapsed = FALSE, width = NULL, solidHeader = TRUE, status = "primary", 
                                           tags$div(class = "yearlysumtable", dataTableOutput("summary")), br(), br())),
                             column(8, box(title = "Bank Balance", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE,
                                           plotlyOutput("balance")))),
                           fluidRow(
                             column(12, box(title = "Expenses Bar Charts", collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE, width = NULL, status = "primary",
                                            fluidRow(column(3, offset = 1, radioButtons("barplotchoice", "Bar Plot Choice",
                                                                   c("Yearly" = "yearly",
                                                                     "Monthly" = "monthly"), inline = TRUE)),
                                                    column(3, radioButtons("barplotgroupchoice", "Group Expenses by",
                                                                c("Type" = "type",
                                                                  "Category" = "category"), inline = TRUE)),
                                                    column(5, checkboxInput("removeRent", label = "Remove Rent / Housing from the Bar and Pie Charts", 
                                                                            value = FALSE))),
                                            br(),
                                            fluidRow(column(12,plotlyOutput("barchart")))
                                            ),
                                        box(title = "Expenses Pie Charts", width = NULL, solidHeader = TRUE, status = "primary",collapsible = TRUE, collapsed = TRUE,
                                            column(6, plotOutput("yearlypiecharttype")),
                                            column(6, plotOutput("yearlypiechartcategory"))))
                           ),
                           fluidRow(
                             column(12, box(title = "Income Bar and Pie Charts", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, width = NULL, status = "primary",
                                            column(6, fluidRow(radioButtons("incomebarplotgroup", "Bar Plot Choice",
                                                                            c("Yearly" = "yearly",
                                                                              "Monthly" = "monthly"), inline = TRUE)),
                                                      fluidRow(plotlyOutput("yearlyincomebarchart"))),
                                            column(6, br(), br(),plotOutput("yearlyincomepiechart")))
                                    )
                           )
                           ),  
                  tabPanel("Expenses",
                           dataTableOutput("yearlyexpensetable")),
                  tabPanel("Income",
                           dataTableOutput("yearlyincometable"))
                )
              )
              ),
      tabItem(tabName = "august",
              h2("August"),
              fluidRow(
                column(12,
                       monthTabUI("august")
                ))
              ),
      tabItem(tabName = "september",
              h2("September"),
              fluidRow(
                column(12,
                       monthTabUI("september")
                ))
              ),
      tabItem(tabName = "october",
              h2("October"),
              fluidRow(
                column(12,
                       monthTabUI("october")
                ))
              ),
      tabItem(tabName = "november",
              h2("November"),
              fluidRow(
                column(12,
                       monthTabUI("november")
                ))
              ),
      tabItem(tabName = "december",
              h2("December"),
              fluidRow(
                column(12,
                       monthTabUI("december")
                ))
              ),
      tabItem(tabName = "january",
              h2("January"),
              fluidRow(
                column(12,
                       monthTabUI("january")
                ))
              ),
      tabItem(tabName = "february",
              h2("February"),
              fluidRow(
                column(12,
                       monthTabUI("february")
                ))
      ),
      tabItem(tabName = "march",
              h2("March"),
              fluidRow(
                column(12,
                       monthTabUI("march")
                ))
      ),
      tabItem(tabName = "april",
              h2("April"),
              fluidRow(
                column(12,
                       monthTabUI("april")
                ))
      ),
      tabItem(tabName = "may",
              h2("May"),
              fluidRow(
                column(12,
                       monthTabUI("may")
                ))
      ),
      tabItem(tabName = "june",
              h2("June"),
              fluidRow(
                column(12,
                       monthTabUI("june")
                ))
      ),
      tabItem(tabName = "july",
              h2("July"),
              fluidRow(
                column(12,
                       monthTabUI("july")
                ))
      )
      
    #)
    )
  )
)
