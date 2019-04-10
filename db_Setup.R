#' Purpose is to whip up our required data base for user needs

library(RSQLite) #using sqlite for now
library(tweedie) #to generate loss data

#' copy current to backup, delete current, and recreate the data base
#' This does obliterate the current backup though, so keep that in mind if we need to retain backups
#' Alternatively, we could just write tables and overwrite and not care about backups. Saves a step.
if(file.exists('./CAS_LTPAWP.sqlite')) file.rename('./CAS_LTPAWP.sqlite','./CAS_LTPAWP.sqlite.bak')
con<-dbConnect(SQLite(),"CAS_LTPAWP.sqlite") #This creates the data base

#' Write our insured classes table frame
#' the Loss_Model and Persistency_Model are intended to capture the R function call 
#' required to describe the distribution. This may or may not be easy to implement.
dbWriteTable(conn=con
             ,name = 'InsuredClass'
             ,value = data.frame(Class='loss_x_persist',Loss_Class='loss',Persistency_Class='persist',Loss_Model='equation_in_text',Persistency_Model='equation_in_text')
            )


#' Now we create a table for us to write to for market rates put forth by insurers, indexed over time
dbWriteTable(conn=con
             ,name = 'Market_Premiums'
             ,value = data.frame(Insurer='unique',As_of_Round=0,Class='unique',Premium=0)
            )

#' Now we create a table to hold the insured data; as in characteristics and premium paid, etc. indexed over time
dbWriteTable(conn=con
             ,name = 'Insured_File'
             ,value = data.frame(Insured='unique#',As_of_Round=0,Insurer='unique',Loss_Class='loss',Persistency_Class='persist',Premium=0)
)

#' GENERATE INSURED CLASSES AND INSURED LIST