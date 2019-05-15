library(shinydashboard)

tbl_player_experience <- reactiveVal(tibble())

#=======================================
# UI ITEMS
mnu_rate_change <- menuItem("Rate change", tabName = "tab_rate_change")

tab_rate_change <- tabItem(
 
     "tab_rate_change"
    , actionButton("btn_update_rates", "Submit rates")

)

#====================================
# SERVER CODE
expr_rate_change <- quote({
  
  observeEvent(input$btn_update_rates, {
    str_sql <- sqlInterpolate(
      db_con()
      , "UPDATE tbl_player_experience SET
          rate_change = ?rate_change
        WHERE segment_name = ?segment_name"
      , rate_change = input$rate_change
    )
    dbExecute(db_con(), str_sql)
    
  })
  
})