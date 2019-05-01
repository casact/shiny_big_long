library(shiny)
library(shinydashboard)

#=======================================
# GLOBAL VALS
num_rounds <- reactiveVal(0)
game_state <- reactiveVal('before')
is_admin <- reactiveVal(FALSE)

#=======================================
# UI ITEMS
mnu_admin <- menuItem("Admin", tabName = "tab_admin")

tab_admin <- tabItem(
    "tab_admin"
  , conditionalPanel(
        condition = paste0("output.is_admin == false")
      , passwordInput("txt_password", "Password")
      , actionButton("btn_login", "Login as admin")
    )
  , conditionalPanel(
        condition = paste0("output.is_admin == true")
      , conditionalPanel(
            condition = paste0("output.game_state == 'before'")
          , numericInput("num_rounds", "Number of rounds: ", 10, min = 5, max = 20, step = 1)
          , textOutput('txt_num_rounds')
          , actionButton("btn_new_game", 'New game')
        )
      , conditionalPanel(
            condition = "output.game_state == 'during'"
          , actionButton("btn_next_round", 'Next round')
          , actionButton("btn_end_game", 'End game')
        )
      , conditionalPanel(
          condition = "output.game_state == 'after'"
        , actionButton("btn_clear_results", 'Clear results')
      )
  )
)

#====================================
# SERVER CODE
expr_admin <- quote({
  
  output$game_state <- reactive({
    game_state()
  })
  
  output$is_admin <- reactive({
    is_admin()
  })
  
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  
  outputOptions(output, "game_state", suspendWhenHidden = FALSE)
  
  observeEvent(input$btn_login, {
    is_admin(input$txt_password == '88kayfabe')
  })
  
  output$txt_num_rounds <- renderText({
    paste0("This game will have ", input$num_rounds, " rounds.")
  })
  
  reactive(
    num_rounds(input$num_rounds)
  )
  
  observeEvent(input$btn_new_game, {
    game_state('during')
  })
  
  observeEvent(input$btn_next_round, {
    # gm_rounds_update()
    # db_update_rounds()
  })
  
  observeEvent(input$btn_end_game, {
    game_state('after')
  })
  
  observeEvent(input$btn_clear_results, {
    game_state('before')
  })
  
})