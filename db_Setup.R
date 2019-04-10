#' Purpose is to whip up our required data base for user needs

library(RSQLite) #using sqlite for now
library(tweedie) #to generate loss data

#' copy current to backup, delete current, and recreate the data base
#' This does obliterate the current backup though, so keep that in mind if we need to retain backups
#' Alternatively, we could just write tables and overwrite and not care about backups. Saves a step.
if(file.exists('./CAS_LTPAWP.sqlite')) file.rename('./CAS_LTPAWP.sqlite','./CAS_LTPAWP.sqlite.bak')
con<-dbConnect(SQLite(),"CAS_LTPAWP.sqlite") #This creates the data base

#' TABLE THAT WILL HOLD ALL THE LOSS AND PERSISTENCY CLASS NAMES
dbWriteTable(conn=con
             ,name = 'InsuredClass'
             ,value = data.frame(Class='loss_x_persist',Loss_Class='loss',Persistency_Class='persist')
            )


#' TABLE TO HOLD THE PREMIUM MOVES OVER TIME.
#' This would be what we read from mid-round to show the market moves and what we write to with a submit button.
#' Users will need to provide:
#' a) If they are privy to persistency classes, the premium by both classes
#' b) If they have 'standard' exhibits, we must write to all persistency classes for a given loss class.
#' 
dbWriteTable(conn=con
             ,name = 'Market_Premiums'
             ,value = data.frame(Insurer='unique',As_of_Round=0,Class='unique',Premium=0)
            )

#' INSURED DATA
#' These will be unique insureds in various classes.
#' We will house the premium and loss results here as well as who holds the policy for the round.
#' This will be the main table to updated in between rounds.
#' Loss is the losses as a random draw from the distribution
#' Lapse is if the insured moves to a new company. The Insured should always move to lowest premium, with ties settled by random draw.
#' This updating should be coded in the Shiny dashboard and reactive by a special moderator button.
dbWriteTable(conn=con
             ,name = 'Insured_File'
             ,value = data.frame(Insured='unique#',As_of_Round=0,Insurer='unique',Loss_Class='loss',Persistency_Class='persist',Premium=0,Loss=0,Lapse=0)
)

#' GENERATE INSURED CLASSES AND INSURED LIST
#' Basically just a 1M or so records strewn between loss and persistency class names.

#'AT THIS POINT WE WOULD HAVE A DATABASE OF INSUREDS
#'