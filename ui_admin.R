#=======================================
# GLOBAL VALS
sqlite_filename <- 'big_long.sqlite'
game_state <- reactiveVal('after')
is_admin <- reactiveVal(FALSE)
db_con <- reactiveVal(NULL)

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
          , actionButton("btn_start_game", 'Start game')
        )

        # Advance the round and decide when to end it
      , conditionalPanel(
            condition = "output.game_state == 'during'"
          , actionButton("btn_next_round", 'Next round')
          , actionButton("btn_end_game", 'End game')
        )

        # Clear the results so that you can start a new game. This is where we 
        # establish the policyholder and policyholder_experience tables
      , conditionalPanel(
          condition = "output.game_state == 'after'"
        , actionButton("btn_clear_results", 'Clear results/prep new game')
      )
  )
)

#====================================
# SERVER CODE
expr_admin <- quote({
  
  current_round <- reactivePoll(
    500
    , session
    , checkFunc = db_updated
    , valueFunc = function() {
      
      tbl <- dbReadTable(db_con(), 'tbl_player_experience')
      tbl$round_num %>% 
        as.integer() %>% 
        max()
    }
  )
  
  output$game_state <- reactive({
    game_state()
  })
  
  output$is_admin <- reactive({
    is_admin()
  })
  
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  outputOptions(output, "game_state", suspendWhenHidden = FALSE)

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

    tmp_player_experience <- tbl_player_experience() %>% 
      gm_player_experience_update(tbl_policyholder_experience()) %>%
      gm_player_experience_update_bots(tbl_player())
    
    dbWriteTable(db_con(), 'tbl_player_experience', tmp_player_experience, overwrite = TRUE)
    
    Sys.sleep(1)
    
    tbl_policyholder_experience() %>%
      gm_policyholder_experience_update(tbl_player_experience()) %>% 
      tbl_policyholder_experience()
    
    game_state('during')
    
  })

  observeEvent(input$btn_next_round, {
    
    # We have already updated the player experience
    tbl_policyholder_experience() %>%
      gm_policyholder_experience_update(tbl_player_experience()) %>% 
      tbl_policyholder_experience()
    
    tmp_player_experience <- tbl_player_experience() %>% 
      gm_player_experience_update(tbl_policyholder_experience()) %>%
      gm_player_experience_update_bots(tbl_player())
    
    dbWriteTable(db_con(), 'tbl_player_experience', tmp_player_experience, overwrite = TRUE)
    
    Sys.sleep(1)
    
  })

  observeEvent(input$btn_end_game, {
    game_state('after')
  })

  observeEvent(input$btn_clear_results, {
    
    dbExecute(db_con(), "DELETE FROM tbl_player")
    dbExecute(db_con(), "DELETE FROM tbl_player_experience")
    
    num_policyholders <- 50
    burn_in_rounds <- 5
    num_rounds <- 10
    
    tbl_bot_player <- gm_dummy_players(1)  %>%
      mutate(name = 'Mona Pauley', attenuation = 0)
    
    dbWriteTable(db_con(), 'tbl_player', tbl_bot_player, append = TRUE)
    dbWriteTable(db_con(), 'tbl_player_experience', gm_player_experience_create(tbl_bot_player, tbl_segment), append = TRUE)
    
    tbl_policyholder <- tbl_segment %>% 
      gm_policyholders_create(num_policyholders)

    tmp_policyholder_experience <- tbl_policyholder %>%
      gm_policyholder_experience_create(num_rounds) %>%
      gm_policyholder_experience_update(tbl_player_experience())

    tmp_player_experience <- tbl_player_experience()
    
    for (i in seq_len(burn_in_rounds - 1)) {
      
      tmp_player_experience <- tmp_player_experience %>%
        gm_player_experience_update(tmp_policyholder_experience) %>%
        gm_player_experience_update_bots(tbl_player())
      
      tmp_policyholder_experience <- tmp_policyholder_experience %>%
        gm_policyholder_experience_update(tmp_player_experience)
      
    }

    dbWriteTable(db_con(), 'tbl_player_experience', tmp_player_experience, overwrite = TRUE)
    tbl_policyholder_experience(tmp_policyholder_experience)
    
    # delay while waiting for the database to recognize the change
    Sys.sleep(1)
    
    game_state('before')
  })
  
})