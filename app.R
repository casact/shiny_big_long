library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)

source('app_startup.R')

source('ui_player_setup.R')
source('ui_profit_loss.R')
source('ui_industry.R')
source('ui_leaderboard.R')
source('ui_rate_change.R')
source('ui_segments.R')
source('ui_admin.R')
source('utility.R')

ui <- dashboardPage(
  
  dashboardHeader(title = "The Big Long")
  , dashboardSidebar(
    
    sidebarMenu(
        mnu_player_setup
      , mnu_profit_loss
      , mnu_profit_loss_industry
      , mnu_leaderboard
      , mnu_rate_change
      , mnu_segments
      , mnu_admin
    )
    
  )
  , dashboardBody(
    tabItems(
        tab_player_setup
      , tab_profit_loss
      , tab_industry
      , tab_leaderboard
      , tab_rate_change
      , tab_segments
      , tab_admin
    )
  )
)

server <- function(input, output, session) {

  db_con(dbConnect(SQLite(), str_db_filename))

  eval(expr_player_setup)
  eval(expr_profit_loss)
  eval(expr_industry)
  eval(expr_leaderboard)
  eval(expr_rate_change)
  eval(expr_admin)
  eval(expr_segments)
  
}

shinyApp(ui = ui, server = server)