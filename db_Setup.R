#' Purpose is to whip up our required data base for user needs

library(RSQLite) #using sqlite for now
library(tweedie) #to generate lossdata
library(data.table)

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

#' GENERATE INSURED CLASSES, INSURED LIST, AND STARTER EXPERIENCE
#' Basically just a 1M or so records strewn between loss and persistency class names.
market_size<-1000000

#' I'm going to set up a set of tweedie models for which the mean, power, and phi will be set.
#' We can achieve several distribuitons from Tweedie assuming we pick the parameters.
#' If you don't like Tweedie, then be my guest and implement a different model.
#' All the fixed values are 100% arbitrary as of this point.
loss_classes<-c('A','B','C','D','E')
loss_means<-seq(1000,by=100,length.out = length(loss_classes))+rnorm(n=length(loss_classes),mean=25,sd=5)
loss_power<-runif(length(loss_classes))*0.66+1
loss_phi<-rep(1,length(loss_classes))

loss_models<-data.table(cbind(loss_classes,loss_means,loss_power,loss_phi))

#' going to join everything but the class_names (loss_classes)
add_loss_names<-setdiff(names(loss_models),'loss_classes') 

rm(loss_classes,loss_means,loss_power,loss_phi)

#' I'm going to set up a set of binomials models for which the mean will depend on rate change and 
#' distance to market minimum price. There is an additional parameter for 'time with company' controlling
#' rate of departure simply due to duration of compnay.
persistency_classes<-c('loayalist','moderate','mover')
rate_change_mult<-c(0.25,0.5,1.0)
market_price_mult<-c(-0.25,0.05,0.5)
general_cancel_rate<-rep(.02,length(persistency_classes))
persist_models<-data.table(cbind(persistency_classes,general_cancel_rate,rate_change_mult,market_price_mult))

#' going to join everything but the class_names (loss_classes)
add_persist_names<-setdiff(names(persist_models),'persistency_classes') 

rm(persistency_classes,general_cancel_rate,rate_change_mult,market_price_mult)


#' Now we can set up our initial table of insureds with associated class parameters.
insured_data<-data.table(Insured=paste0(LETTERS,sample(1000000:9999999,size=market_size,replace = FALSE))
                         ,Loss_Class=sample(loss_classes,size=market_size,replace = TRUE)
                         ,Persistency_Class=sample(persistency_classes,size=market_size,replace = TRUE)
                        )

insured_data[,(add_loss_names):=loss_models[insured_data[,.(Loss_Class)]
                                        ,.SD
                                        ,.SDcols=add_loss_names
                                        ,on=.(loss_classes=Loss_Class)]
             ]

insured_data[,(add_persist_names):=persist_models[insured_data[,.(Persistency_Class)]
                                        ,.SD
                                        ,.SDcols=add_persist_names
                                        ,on=.(persistency_classes=Persistency_Class)]
             ]


#'AT THIS POINT WE WOULD HAVE A DATABASE OF INSUREDS
#'