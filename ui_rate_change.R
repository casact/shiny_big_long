library(shinydashboard)

#=======================================
# UI ITEMS
mnu_rate_change <- menuItem("Rate change", tabName = "tab_rate_change")

tab_rate_change <- tabItem(
 
     "tab_rate_change"
    , h2("This is the place where we put a dynamic set of UI")
    , actionButton("btn_update_rates", "Submit rates")

)

#====================================
# SERVER CODE
expr_rate_change <- quote({
  

  
})