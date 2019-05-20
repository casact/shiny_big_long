library(shinydashboard)

#=======================================
# UI ITEMS
mnu_rate_change <- menuItem("Rate change", tabName = "tab_rate_change")

tab_rate_change <- tabItem(
 
     "tab_rate_change"
    , textOutput("txt_current_round_rate_change")
    , conditionalPanel(
      condition = "output.game_state != 'during'"
      , textOutput("You will be able to enter rate changes once the game has begun.")
    )
    , conditionalPanel(
      condition = "output.game_state == 'during'"
      , uiOutput("rate_changes")
      , actionButton("btn_update_rates", "Submit rates")
    )

)

#====================================
# SERVER CODE
expr_rate_change <- quote({

  tbl_player_experience <- reactivePoll(
    500
    , session
    , checkFunc = db_updated
    , valueFunc = function() {
      dbReadTable(db_con(), 'tbl_player_experience') %>% 
        mutate(round_num = round_num %>% as.integer())
    }
  )
  
  output$rate_changes <- renderUI({
  
    lapply(tbl_segment$name, function(x) {
      # textOutput()
      numericInput(paste0("rate_change_", x), paste0("Rate change ", x), 0, -0.05, 0.05, 0.005)
    })
  })
  
  output$txt_current_round_rate_change <- renderText({
    paste0("It is currently round: ", current_round())
  })

  observeEvent(input$btn_update_rates, {
    
    for (i in seq_along(tbl_segment$name)) {
      str_sql <- sqlInterpolate(
        db_con()
        , "UPDATE tbl_player_experience SET
          rate_change = ?rate_change
          , offer_premium = prior_offer_premium * (1 + ?rate_change)
        WHERE player_name = ?player_name
        AND segment_name = ?segment_name
        AND round_num = ?round_num
      "
        , rate_change = input[[paste0('rate_change_', tbl_segment$name[i])]]
        , segment_name = tbl_segment$name[i]
        , player_name = player_name()
        , round_num = current_round()
      )
      dbExecute(db_con(), str_sql)

    }
    
  })
  
})