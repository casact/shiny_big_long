#'
#' UI definition (how stuff should be laid out)
#'
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui<-dashboardPage(
     dashboardHeader(title = "CAS_Pricing"),
     dashboardSidebar(
        #' Here we should ad an team idenfier use in the SQL to push/pull into table between rounds.
        #' We need to somehow allow it to be open for them to entre a name/number, but fix it so they
        #' can't mess it up in later roungs. But maybe we just trust them not to mess up.
    
        sidebarMenu(
            #' Maybe the reports are indications by class (loss vs loss and time variable classes),
            #' market share, and the standard profit/loss?
            menuItem("ReportName1", tabName = "report1"),
            menuItem("ReportName2", tabName = "report2"),
            menuItem("ReportName3", tabName = "report3"),
            menuItem("Make Your Move", tabName = "round_selects")
        )
    
    ),
    
     dashboardBody(
        tabItems(
            
            tabItem("report1",
                    #'Here we do all the shiny design work for report 1.
                    #' Copy this block for all the reports needed.
                    #' use uiOutput() to provide the conditional reports
                    fluidRow(
                        
                    uiOutput("report1_plot", width = "100%", height = 600)
                        
                    )
            ),
            
            tabItem("round_selects",
                 actionButton("submit", "Change Rates!")
                 #" Maybe add another button so people can see 'rate filings' coming in periodically?
                    
            )
        )
    )
)
