#=======================================
# GLOBAL VALS
sqlite_filename <- 'big_long.sqlite'
game_state <- reactiveVal('before')
is_admin <- reactiveVal(TRUE)
db_con <- reactiveVal(NULL)
current_round <- reactiveVal(0)

tbl_policyholders <- reactiveVal(tibble())
tbl_policyholder_experience <- reactiveVal(tibble())
tbl_player_experience <- reactiveVal(tibble())

#=======================================
# UI ITEMS
mnu_admin <- menuItem("Admin", tabName = "tab_admin")

tab_admin <- tabItem(
    "tab_admin"
  , actionButton("btn_connect_db", "Connect to db")
  , actionButton("btn_test_connection", "Test connection")
  , conditionalPanel(
        condition = "output.is_admin == false"
      , passwordInput("txt_password", "Password")
      , actionButton("btn_login", "Login as admin")
    )
  , conditionalPanel(
        condition = "output.is_admin == true"
      , actionButton("btn_logout", "Logout as admin")
        # Set up the game before it begins
      , conditionalPanel(
            condition = paste0("output.game_state == 'before'")
          # , numericInput("num_rounds", "Number of rounds: ", 10, min = 5, max = 20, step = 1)
          # , textOutput('txt_num_rounds')
          , actionButton("btn_start_game", 'Start game')
        )

        # Advance the round and decide when to end it
      , conditionalPanel(
            condition = "output.game_state == 'during'"
          , actionButton("btn_next_round", 'Next round')
          , actionButton("btn_end_game", 'End game')
        )

        # Clear the results so that you can start a new game. (This might not be necessary)
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

  output$tbl_players <- renderDataTable(tbl_player)
  
  # reactive(
  #   num_rounds(input$num_rounds)
  # )
  
  db_connected <- reactive({
    
    if (is.null(db_con())) {
      FALSE
    } else {
      dbIsValid(db_con())
    }
    
  })
  
  observeEvent(input$btn_login, {
    is_admin(input$txt_password == '88kayfabe')
  })
  
  observeEvent(input$btn_logout, {
    is_admin(FALSE)
  })

  observeEvent(input$btn_connect_db, {

    if (db_connected()) {
      showNotification("Database is connected.")
    }
    
  })

  observeEvent(input$btn_test_connection, {

    if (!db_connected()) {
      showNotification("Database is not connected.", type = 'warning')
    } else {
      showNotification("Database is connected.")
    }

  })

  output$txt_num_rounds <- renderText({
    paste0("This game will have ", input$num_rounds, " rounds.")
  })

  observeEvent(input$btn_start_game, {
    # create policyholders
    # create rounds
    # gm_rounds_create()
    tbl_player_experience(fetch_db_table(db_con()), 'tbl_player_experience')
    current_round(1)
    game_state('during')
  })

  observeEvent(input$btn_next_round, {
    # gm_rounds_update()
    # db_update_rounds()
    current_round(current_round() + 1)
  })

  observeEvent(input$btn_end_game, {
    game_state('after')
  })

  observeEvent(input$btn_clear_results, {
    game_state('before')
  })
  
})