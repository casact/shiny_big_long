library(shiny)
library(shinydashboard)
library(tidyverse)

#=======================================
# UI ITEMS
mnu_profit_loss <- menuItem("Profit/Loss", tabName = "tab_profit_loss")

tab_profit_loss <- tabItem(
  
    "tab_profit_loss"
  , fluidRow(
    dataTableOutput("tbl_profit_loss")
  )
  , fluidRow(
    plotOutput('plt_profit_loss')
  )
  
)

#====================================
# SERVER CODE
expr_profit_loss <- quote({
  
  data(mtcars)
  
  output$tbl_profit_loss <- renderDataTable({
   renderDataTable(mtcars) 
  })

  output$plt_profit_loss <- renderPlot({
    mtcars %>% 
      ggplot(aes(disp, mpg)) + 
      geom_point(aes(color = cyl > 6))
  })  

})