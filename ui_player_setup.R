player_name <- reactiveVal(NA_character_)
num_players <- reactiveVal(0)
player_names <- reactiveVal(NULL)

#=======================================
# UI ITEMS
mnu_player_setup <- menuItem("Player setup", tabName = "tab_player_setup")

tab_player_setup <- tabItem(
    "tab_player_setup"
  , fluidRow(
      column(
        12
        , textOutput("txt_num_players")
        , textOutput("txt_current_round")
        , tableOutput("tbl_player")
      )
  )
  , conditionalPanel(
        condition = paste0("output.player_created == false")
        , fluidRow(
            column(12
            , textInput("txt_player_name", "Player name")
            , actionButton("btn_create_player", "Create player")
          )
        )
    )
  , conditionalPanel(
        condition = paste0("output.player_created == true")
      , h1(textOutput("txt_welcome_message"))
    )
)

#====================================
# SERVER CODE
expr_player_setup <- quote({

  tbl_player <- reactivePoll(
    500
    , session
    , checkFunc = db_updated
    , valueFunc = function() {
        dbReadTable(db_con(), 'tbl_player') %>% 
          mutate(bot = as.logical(bot))
      }
  )
  
  observe({
    player_names(tbl_player()$name %>% unique())
    num_players(tbl_player()$name %>% length())
  })
  
  player_created <- reactiveVal(FALSE)
  
  output$player_created <- reactive({
    player_created()
  })
  
  output$txt_current_round <- renderText({
    paste0("It is currently round: ", current_round())
  })
  
  outputOptions(output, "player_created", suspendWhenHidden = FALSE)
  
  observeEvent(input$btn_create_player, {
    
    if (length(input$txt_player_name) == 0) {
      showNotification("The player name cannot be blank.", type = 'error')
      return()
    }
    
    if (!db_connected()) {
      showNotification("You are not connected to the database.", type = 'error')
      return()
    }
    
    if (intersect(player_names(), input$txt_player_name) %>% length() > 0) {
      showNotification("That player name already exists.", type = 'error')
      return()
    }

    tmp_player <- tibble(
      name = input$txt_player_name
      , bot = FALSE
    )
    
    dbWriteTable(
        db_con()
      , 'tbl_player'
      , tmp_player
      , append = TRUE
    )
    
    tbl_median_premium <- tbl_player_experience() %>% 
      filter(round_num == current_round()) %>% 
      group_by(segment_name) %>% 
      summarise(offer_premium = median(offer_premium))
    
    tmp_player_add <- tibble(
      player_name = input$txt_player_name
      , round_num = current_round()
    ) %>% 
      crossing(tbl_median_premium)
    
    dbWriteTable(
      db_con()
      , 'tbl_player_experience'
      , tmp_player_add
      , append = TRUE
    )

    player_name(input$txt_player_name)
    player_created(TRUE)
    
  })
  
  output$txt_welcome_message <- renderText(
    paste0("Welcome aboard ", player_name(), "!")
  )
  
  output$tbl_player <- renderTable({
    invalidateLater(2000)
    
    tbl_player()
  })
  
  output$txt_num_players <- renderText({
    if (num_players() == 1) {
      "There is 1 player in this game."
    } else {
      paste0("There are ", num_players(), " players in this game.")
    }
  })
  
})