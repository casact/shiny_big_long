library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)

source('app_startup.R')

source('ui_player_setup.R')
source('ui_profit_loss.R')
source('ui_rate_change.R')
source('ui_segments.R')
source('ui_admin.R')

fetch_db_column <- function(db_con, tbl_name, col_name) {
  
  if (dbIsValid(db_con)) {
    tbl_in <- dbReadTable(db_con, tbl_name)
    tbl_in[[col_name]]
  } else {
    NULL
  }
  
}

fetch_db_table <- function(db_con, tbl_name) {
  
  if (dbIsValid(db_con)) {
    dbReadTable(db_con, tbl_name)
  } else {
    NULL
  }
  
}

ui <- dashboardPage(
  
  dashboardHeader(title = "The Big Long")
  , dashboardSidebar(
    
    sidebarMenu(
        mnu_player_setup
      , mnu_profit_loss
      , mnu_rate_change
      , mnu_segments
      , mnu_admin
    )
    
  )
  , dashboardBody(
    tabItems(
        tab_player_setup
      , tab_profit_loss
      , tab_rate_change
      , tab_segments
      , tab_admin
    )
  )
)

server <- function(input, output, session) {
  
  db_con(dbConnect(SQLite(), sqlite_filename))

  observe({
    
    segment_names(fetch_db_column(db_con(), 'tbl_segment', 'name'))
    player_names(fetch_db_column(db_con(), 'tbl_player', 'name'))
    # tbl_segment(fetch_db_table(db_con(), 'tbl_segment'))
    num_players(fetch_db_column(db_con(), 'tbl_player', 'name') %>% length())
    current_round(
      fetch_db_column(db_con(), 'tbl_player_experience', 'round_num') 
        %>% max()
    )
    
  })
  
  eval(expr_player_setup)
  eval(expr_profit_loss)
  eval(expr_rate_change)
  eval(expr_admin)
  eval(expr_segments)
  
}

shinyApp(ui = ui, server = server)