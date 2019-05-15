segment_names <- reactiveVal(NULL)
# tbl_segment <- reactiveVal(tibble())

mnu_segments <- menuItem("Segments", tabName = "tab_segments")

tab_segments <- tabItem(
  "tab_segments"
  , fluidPage(
    fluidRow(
      tableOutput("tbl_segments")
    )
    , fluidRow(
        selectInput("select_segment", "Select segment", "no segment defined")
      , column(4
        , p("The plot below shows the propensity to compare prices in the marketplace")
        , plotOutput("plt_compare")
      )
      , column(4
        , p("Below is a probability density plot for the claim frequency for this segment")
        , plotOutput("plt_freq")
      )
      , column(4
        , p("Below is a pdf of the claim severity for this segment")
        , plotOutput("plt_severity")
      )
    )
  )
)

expr_segments <- quote({
  
  observe({
    
    updateSelectInput(
      session
      , "select_segment"
      , choices = segment_names()
    )
    
  })
  
  tbl_selected_segment <- reactive({
    
    tbl_segment %>% 
      filter(name == input$select_segment)
    
  })
  
  output$tbl_segments <- renderTable({
    tbl_segment
  })
  
  output$plt_compare <- renderPlot({
    
    tibble(
      x = seq(.001, .999, length.out = 250)
      , y = dbeta(x, tbl_selected_segment()$compare_alpha, tbl_selected_segment()$compare_beta)
    ) %>%
      ggplot(aes(x, y)) +
      geom_line() +
      labs(x = 'p', y = 'f(p)')

  })

  output$plt_freq <- renderPlot({
    tibble(
      x = seq(.001, 20, length.out = 250)
      , y = dgamma(x, tbl_selected_segment()$freq_shape, scale = tbl_selected_segment()$freq_scale)
    ) %>%
      ggplot(aes(x, y)) +
      geom_line() +
      labs(x = 'N', y = 'f(N)')
  })

  output$plt_severity <- renderPlot({
    tibble(
      x = seq(.001, 25e3, length.out = 250)
      , y = dgamma(x, tbl_selected_segment()$sev_shape, scale = tbl_selected_segment()$sev_scale)
    ) %>%
      ggplot(aes(x, y)) +
      geom_line() +
      labs(x = 'severity', y = ' f(severity')
  })
  
})