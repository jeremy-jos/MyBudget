library(shiny)
library(shinydashboard)
library(openxlsx)
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

if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}

library(rJava)
library(XLConnect)

source("monthTab.R")

## keep an eye out for the clunky fix for the date of a new expense / income I add
## write in excel file when adding a new expense / income
## changer la partie budget de la même façon pour qu'elle soit réactive et réagisse à l'ajout d'un budget personnalisé
