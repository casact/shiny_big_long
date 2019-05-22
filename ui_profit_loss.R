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
      ,h2("One Year Experience")
      , fluidRow(
        dataTableOutput("tbl_profit_loss")
      )
      ,h2("Five Year Experience")
      , fluidRow(title="5 year aggregate",
        dataTableOutput("tbl_profit_loss_5yr")
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
  
  output$tbl_profit_loss <- renderDataTable({
    
    #' One year Experience
    tbl_policyholder_experience() %>% 
      filter(round_num == current_round()-1,current_market==player_name()) %>% 
      group_by(segment_name, current_market) %>% 
      summarise(income = sum(income, na.rm = TRUE)
                ,avg_premium=sum(current_premium)/n()
                ,policies=n()
                ,obs_freq=sum(observed_claims,na.rm=TRUE)/n()
                ,obs_losses=sum(observed_cost,na.rm=TRUE)
                ,obs_severity=obs_losses/sum(observed_claims,na.rm=TRUE)) %>%
      mutate(income=scales::dollar(income)
             ,avg_premium=scales::comma(avg_premium)
             ,policies=scales::comma(policies)
             ,obs_freq=round(obs_freq,2)
             ,obs_losses=scales::comma(obs_losses)
             ,obs_severity=scales::comma(obs_severity)) %>%
      select(segment_name,income,policies,avg_premium,obs_freq,obs_losses)
      
  })

  
  
  output$tbl_profit_loss_5yr <- renderDataTable({
        #' 5 year Experience
        expr=tbl_policyholder_experience() %>% 
          filter(round_num > current_round()-6,current_market==player_name()) %>% 
          group_by(segment_name, current_market) %>% 
          summarise(income = sum(income, na.rm = TRUE)
                    ,avg_premium=sum(current_premium)/n()
                    ,policies=n()
                    ,obs_freq=sum(observed_claims,na.rm=TRUE)/n()
                    ,obs_losses=sum(observed_cost,na.rm=TRUE)
                    ,obs_severity=obs_losses/sum(observed_claims,na.rm=TRUE)) %>%
          mutate(income=scales::dollar(income)
                 ,avg_premium=scales::comma(avg_premium)
                 ,policies=scales::comma(policies)
                 ,obs_freq=round(obs_freq,2)
                 ,obs_losses=scales::comma(obs_losses)
                 ,obs_severity=scales::comma(obs_severity))%>%
        select(segment_name,income,policies,avg_premium,obs_freq,obs_losses)     
  })
  output$plt_profit_loss <- renderPlot({
    
    tbl_policyholder_experience() %>% 
      filter(round_num < current_round(),current_market==player_name()) %>% 
      ggplot(aes(income)) + 
      geom_histogram() + 
      facet_wrap(~ round_num, scales = 'free_x')
  })  

})