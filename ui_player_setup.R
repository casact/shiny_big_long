#=======================================
# UI ITEMS
mnu_player_setup <- menuItem("Player setup", tabName = "tab_player_setup")

tab_player_setup <- tabItem(
    "tab_player_setup"
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
  
  player_created <- reactiveVal(FALSE)
  
  output$player_created <- reactive({
    player_created()
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
    
    dbWriteTable(
        db_con()
      , 'tbl_player'
      , tibble(
          name = input$txt_player_name
          , bot = FALSE
          )
      , append = TRUE)
    
    player_created(TRUE)
    
  })
  
  output$txt_welcome_message <- renderText(
    paste0("Welcome aboard ", input$txt_player_name, "!")
  )
  
})