library(shiny)
library(shinydashboard)

source('ui_team_setup.R')
source('ui_profit_loss.R')
source('ui_rate_change.R')
source('ui_admin.R')

source('db_setup.R')

ui <- dashboardPage(
  
  dashboardHeader(title = "The Big Long")
  , dashboardSidebar(
    
    sidebarMenu(
      mnu_team_setup
      , mnu_profit_loss
      , mnu_rate_change
      , mnu_admin
    )
    
  )
  , dashboardBody(
    tabItems(
      tab_team_setup
      , tab_profit_loss
      , tab_rate_change
      , tab_admin
    )
  )
)

server <- function(input, output, session) {
  
  eval(expr_team_setup)
  eval(expr_profit_loss)
  eval(expr_rate_change)
  eval(expr_admin)
  
}

shinyApp(ui = ui, server = server)