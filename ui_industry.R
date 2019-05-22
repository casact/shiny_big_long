library(shiny)
library(shinydashboard)
library(tidyverse)

#=======================================
# UI ITEMS
mnu_profit_loss_industry <- menuItem("Industry Data", tabName = "tab_industry")

tab_industry <- tabItem(
  
  "tab_industry"
  , conditionalPanel(
    condition = "output.game_state != 'after'"
    
    ,h2("One Year Experience")
    , fluidRow(
      dataTableOutput("tbl_industry")
    )
    ,h2("Five Year Experience")
    , fluidRow(title="5 year aggregate",
               dataTableOutput("tbl_5yr_industry")
    )
  )
  , conditionalPanel(
    condition = "output.game_state != 'during'"
    , p("The game has not yet started")
  )
  
)

#====================================
# SERVER CODE
expr_industry <- quote({
  
  output$tbl_industry <- renderDataTable({
    
    #' One year Experience
    tbl_policyholder_experience() %>% 
      filter(round_num == current_round()-1) %>% 
      group_by(segment_name) %>% 
      summarise(income = sum(income, na.rm = TRUE)
                ,avg_premium=sum(current_premium)/n()
                ,policies=n()
                ,obs_freq=sum(observed_claims,na.rm=TRUE)/n()
                ,obs_losses=sum(observed_cost,na.rm=TRUE)
                ,total_prem=sum(current_premium)
                ,obs_severity=obs_losses/sum(observed_claims,na.rm=TRUE))%>%
      mutate(loss_ratio=obs_losses/total_prem
             ,indication=loss_ratio-1) %>%
      mutate(income=scales::dollar(income)
             ,avg_premium=scales::comma(avg_premium)
             ,policies=scales::comma(policies)
             ,obs_freq=round(obs_freq,2)
             ,obs_losses=scales::comma(obs_losses)
             ,obs_severity=scales::comma(obs_severity)
             ,loss_ratio=scales::percent(loss_ratio)
             ,indication=scales::percent(indication)) %>%
      select(segment_name,income,policies,avg_premium,loss_ratio,indication)
    
  })
  
  
  
  output$tbl_5yr_industry <- renderDataTable({
    #' 5 year Experience
    expr=tbl_policyholder_experience() %>% 
      filter(round_num > current_round()-6,!is.na(current_premium)) %>% 
      group_by(segment_name) %>% 
      summarise(income = sum(income, na.rm = TRUE)
                ,avg_premium=sum(current_premium)/n()
                ,policies=n()
                ,obs_freq=sum(observed_claims,na.rm=TRUE)/n()
                ,obs_losses=sum(observed_cost,na.rm=TRUE)
                ,total_prem=sum(current_premium)
                ,obs_severity=obs_losses/sum(observed_claims,na.rm=TRUE))%>%
      mutate(loss_ratio=obs_losses/total_prem
             ,indication=loss_ratio-1) %>%
      mutate(income=scales::dollar(income)
             ,avg_premium=scales::comma(avg_premium)
             ,policies=scales::comma(policies)
             ,obs_freq=round(obs_freq,2)
             ,obs_losses=scales::comma(obs_losses)
             ,obs_severity=scales::comma(obs_severity)
             ,loss_ratio=scales::percent(loss_ratio)
             ,indication=scales::percent(indication)) %>%
      select(segment_name,income,policies,avg_premium,loss_ratio,indication)
    
  })
  
})