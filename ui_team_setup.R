library(shiny)
library(shinydashboard)

#=======================================
# UI ITEMS
mnu_team_setup <- menuItem("Team setup", tabName = "tab_team_setup")

tab_team_setup <- tabItem(
    "tab_team_setup"
  , conditionalPanel(
        condition = paste0("output.team_created == false")
      , textInput("txt_team_name", "Team name")
      , actionButton("btn_create_team", "Create team")
    )
  , conditionalPanel(
        condition = paste0("output.team_created == true")
      , h1("Welcome aboard!")
  )
)

#====================================
# SERVER CODE
expr_team_setup <- quote({
  
  team_created <- reactiveVal(FALSE)
  
  
  output$team_created <- reactive({
    team_created()
  })
  
  outputOptions(output, "team_created", suspendWhenHidden = FALSE)
  
  observeEvent(input$btn_create_team, {
    # db_add_player
    if (input$txt_team_name != '') {
      team_created(TRUE)
    }
    
  })
  
})