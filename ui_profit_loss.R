library(shiny)
library(shinydashboard)
library(tidyverse)

tbl_policyholder_experience <- reactiveVal(tibble())

#=======================================
# UI ITEMS
mnu_profit_loss <- menuItem("Profit/Loss", tabName = "tab_profit_loss")

tab_profit_loss <- tabItem(
  
    "tab_profit_loss"
  , conditionalPanel(
      condition = "output.game_state != 'after'"
      , fluidRow(
        plotOutput('plt_profit_loss')
      )
      , fluidRow(
        dataTableOutput("tbl_profit_loss")
      )
    )
  , conditionalPanel(
    condition = "output.game_state != 'during'"
    , p("The game has not yet started")
  )
  
)

#====================================
# SERVER CODE
expr_profit_loss <- quote({
  
    tbl_policyholder_experience() %>% 
      filter(round_num < current_round()) %>% 
      group_by(segment_name, round_num) %>% 
      summarise(income = sum(income, na.rm = TRUE)
                ,policies=n()
                ,obs_freq=sum(observed_claims,na.rm=TRUE)/n()
                ,obs_losses=sum(observed_cost,na.rm=TRUE)
                ,obs_severity=obs_losses/obs_freq) %>%
      mutate(income=scales::dollar(income)
             ,policies=scales::comma(policies)
             ,obs_freq=scales::percent(obs_freq)
             ,obs_losses=scales::comma(obs_losses)
             ,obs_severity=scales::comma(obs_severity))
      
  })

  output$plt_profit_loss <- renderPlot({
    
    tbl_policyholder_experience() %>% 
      filter(round_num < current_round()) %>% 
      ggplot(aes(income)) + 
      geom_histogram() + 
      facet_wrap(~ round_num, scales = 'free_x')
  })  

})