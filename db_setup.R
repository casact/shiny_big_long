#' Purpose is to whip up our required data base for user needs

library(RSQLite) #using sqlite for now
library(DBI) #usually use RODBCext, but it doesn't seem to like SQLite

source('game.R')
source('db.R')

#' copy current to backup, delete current, and recreate the data base
#' This does obliterate the current backup though, so keep that in mind if we need to retain backups
#' Alternatively, we could just write tables and overwrite and not care about backups. Saves a step.

if (file.exists('./CAS_LTPAWP.sqlite')) 
  file.rename('./CAS_LTPAWP.sqlite', './CAS_LTPAWP.sqlite.bak')

# This creates the data base
con <- dbConnect(SQLite(),"CAS_LTPAWP.sqlite") 

dbDisconnect(con)
rm(con)

#' AT THIS POINT WE HAVE OUR STARTING DATABASE AND DATA
