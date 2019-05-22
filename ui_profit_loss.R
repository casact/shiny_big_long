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
  output$plt_profit_loss <- renderPlot({
    
    data<-
    tbl_policyholder_experience() %>% 
      filter(round_num < current_round(),current_market==player_name()) %>%
      group_by(round_num,segment_name)%>%
      summarise(policies=n()
             ,obs_loss_cost=sum(observed_cost,na.rm=TRUE)/n())
     
    if(nrow(data)>0){
      ggplot(aes(x=round_num,y=obs_loss_cost)) + 
      geom_boxplot() + 
      facet_wrap(~ segment_name, scales = 'free_x')
    }
  })  

})