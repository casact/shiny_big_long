#Server side (computaton/interpretation)

library(shiny)
library(shinydashboard)
library(ggplot2)

source('ui_team_setup.R')
source('ui_profit_loss.R')
source('ui_rate_change.R')
source('ui_admin.R')

#' This code will run at the app startup and is meant to prep a data base
#' that holds our class characteristics and premium
source('db_setup.R')

server <- function(input, output, session) {

  eval(expr_team_setup)
  eval(expr_profit_loss)
  eval(expr_rate_change)
  eval(expr_admin)
  
}
