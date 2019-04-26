#' This script is intended to be run between rounds. 
#' Thus, the insured_data object and SQLite data base are
#' assumed to be set up & in the global.env of the shiny app
#' This needs to be integrated into the sever.R code at some point.

round<-3 #This would be dynamic in the app so we can check the round advance.

#' Pull Premium from data base
con<-dbConnect(SQLite(),"CAS_LTPAWP.sqlite")

premium_data<-dbGetQuery(con,
                         "SELECT * FROM Market_Premiums")
setDT(premium_data)

#insured_data<-dbGetQuery(con,
#                paste0("SELECT * FROM Insured_File WHERE As_of_Round>",round-2))) #grab last two rounds to compute prem diffs

#' Pair in new premium information to losses
insured_data[,New_Premium:=premium_data[insured_data[,.(Insurer,As_of_Round,Loss_Class,Persistency_Class)]
                                       ,Premium
                                       ,on=.(Insurer,Loss_Class,Persistency_Class,As_of_Round)
                                       ,roll=Inf]]

#' Calcualte min market rate for LossxPersistency class
#' Calcualte premium change (shift by Insured and round)
#' Calculate the binomial mean base on above two.
#' simulate the cancellations.
#' for those that cancel, sample new insured from those with the minimal rate for their class.
#' Set Premium for the round = to the premium from the paired insurer.
