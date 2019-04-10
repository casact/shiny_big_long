#Server side (computaton/interpretation)

library(shiny)
library(shinydashboard)
library(ggplot2)


#' This code will run at the app startup and is meant to prep a data base
#' that holds our class characteristics and premium

server<-function(input, output,session) {

  
    #' Needs:
    #'   Round Counter that increases by 1 for each click of submit. Put it table so that we can query info only known up to this round.
    #'   Team name input to draw back the report data specifically for the team
    #'   Code up the proper reports
    #'   An entry table for rate changes (rhandsontable has worked well for me in the past)
    
    observeEvent(input$submit, {
        #' Is SQLite the right solution? It doesn't do simultaneous write so small risk 
        #' of error issued if two teams write exactly same time.
        con<-dbConnect(SQLite(),"CAS_LTPAWP.sqlite")
        
        dbExecute(con
                  ,'INSERT INTO Market_Premiums
                    VALUES (?,?)'
                  ,param=list(session$user,input$round,input$class,input$premium)) #gotta find a way to lock down round count. Also, I should first make a table then just append the whole table.
        
        dbDisconnect(con)
        
        #' Probably want to signal the user that the submission went in so they don't click 100 times.
        
    })    
    
    output$report1_plot <- renderPlot({

        #' play chart; reports need to be more elaborate.
        ggplot(data=data.frame(round=1:5,prem=rep(100,5),loss=rnorm(5,75,3)))+geom_line(aes(x=round,y=loss/prem))+theme_minimal()

    })
    

    

}
