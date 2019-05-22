library(shiny)
library(shinydashboard)
library(tidyverse)

#=======================================
# UI ITEMS
mnu_leaderboard <- menuItem("Leaderboard", tabName = "tab_leaderboard")

tab_leaderboard <- tabItem(
  
  "tab_leaderboard"
  , conditionalPanel(
    condition = "output.game_state != 'after'"
    
    ,h2("Leaderboard")
    , fluidRow(dataTableOutput("tbl_leaderboard")
    )
  )
  , conditionalPanel(
    condition = "output.game_state != 'during'"
    , p("The game has not yet started")
  )
  
)

#====================================
# SERVER CODE
expr_leaderboard <- quote({
  
  output$tbl_leaderboard <- renderDataTable({
    #' 5 year Experience
    tbl_policyholder_experience() %>% 
      filter(round_num < current_round()) %>%
      group_by(current_market) %>% 
      summarise(Income = sum(income, na.rm = TRUE)
                ,Premium=sum(current_premium)
                ,Frequency=sum(observed_claims,na.rm=TRUE)/n()
                ,Total_Losses=sum(observed_cost,na.rm=TRUE)) %>%
      mutate(Company=current_market
             ,Income=scales::dollar(Income)
             ,Premium=scales::comma(Premium)
             ,Frequency=round(Frequency,2)
             ,Total_Losses=scales::comma(Total_Losses))%>%
      select(Company,Income,Premium,Frequency,Total_Losses)    
  })
  
})