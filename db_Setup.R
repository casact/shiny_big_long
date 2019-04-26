#' Purpose is to whip up our required data base for user needs

library(RSQLite) #using sqlite for now
library(DBI) #usually use RODBCext, but it doesn't seem to like SQLite
library(data.table) #Efficient data set up 

#' GENERATE INSURED CLASSES, INSURED LIST, AND STARTER EXPERIENCE
#' Basically just a 1M or so records strewn between loss and persistency class names.
market_size<-500000

#' I'm going to set up a set of freq x severity models for which the paramters will be set
#' Originally wanted to go with Tweedie, but terribly slow simulations with rtweedie.
#' I'm going for a gamma binomial mix at this point (keep it to one claim per insured)
#' All the fixed values are 100% arbitrary as of this point.
loss_classes<-c('A','B','C','D','E')
loss_means_sev<-seq(800,by=200,length.out = length(loss_classes))+runif(n=length(loss_classes))*100
loss_means_freq<-seq(0,by=.01,length.out = length(loss_classes))+runif(n=length(loss_classes))/50

#We could add a trend column here so that when we simulation we do the mean*trend^round

loss_models<-data.table(Loss_Class=loss_classes
                        ,loss_means_sev=loss_means_sev
                        ,loss_means_freq=loss_means_freq
                        )

#' going to join everything but the class_names (loss_classes)
add_loss_names<-setdiff(names(loss_models),'Loss_Class') 

#' I'm going to set up a set of binomials models for which the mean will depend on rate change and 
#' distance to market minimum price. There is an additional parameter for 'time with company' controlling
#' rate of departure simply due to duration of compnay.
persistency_classes<-c('loayalist','moderate','mover')
rate_change_mult<-c(0.25,0.5,1.0)
market_price_mult<-c(-0.25,0.05,0.5)
general_cancel_rate<-rep(.02,length(persistency_classes))
persist_models<-data.table(Persistency_Class=persistency_classes
                           ,general_cancel_rate
                           ,rate_change_mult
                           ,market_price_mult)

#' going to join everything but the class_names (loss_classes)
add_persist_names<-setdiff(names(persist_models),'Persistency_Class') 


#' Now we can set up our initial table of insureds with associated class parameters.
insured_data<-data.table(Insured=paste0(LETTERS,sample(1000000:9999999,size=market_size,replace = FALSE))
                         ,Loss_Class=sample(loss_classes,size=market_size,replace = TRUE)
                         ,Persistency_Class=sample(persistency_classes,size=market_size,replace = TRUE)
                        )

insured_data[,(add_loss_names):=loss_models[insured_data[,.(Loss_Class)]
                                        ,.SD
                                        ,.SDcols=add_loss_names
                                        ,on=.(Loss_Class)]
             ]

insured_data[,(add_persist_names):=persist_models[insured_data[,.(Persistency_Class)]
                                        ,.SD
                                        ,.SDcols=add_persist_names
                                        ,on=.(Persistency_Class)]
             ]

insured_data[,`:=`(Premium=50
                   ,Loss=0
                   ,Lapse=0
                   ,Insurer="STATE"
                  )
             ]

#' Here we will duplicate the insured information so that we can have our loss history pre-built
#' Not sure if this is better done here and then tie in premium, or to create each round incrementally?
#' It comes down to the overhead of data.base calls each round vs joining data into a large table each time.
#' In any case, and initial set of rounds will be needed.
rounds<-10

insured_data<-insured_data[rep(1:.N,rounds)][order(Insured),As_of_Round:=0:(rounds-1)][order(Insured)]

insured_data[,Loss:=rbinom(n=1:nrow(insured_data),1,loss_means_freq)*
                    rgamma(n=1:nrow(insured_data),shape = loss_means_sev,rate = 1)
             ]
rm(list=setdiff(ls(),'insured_data'))

#' copy current to backup, delete current, and recreate the data base
#' This does obliterate the current backup though, so keep that in mind if we need to retain backups
#' Alternatively, we could just write tables and overwrite and not care about backups. Saves a step.
if(file.exists('./CAS_LTPAWP.sqlite')) file.rename('./CAS_LTPAWP.sqlite','./CAS_LTPAWP.sqlite.bak')
con<-dbConnect(SQLite(),"CAS_LTPAWP.sqlite") #This creates the data base

#' INSURED DATA
#' These will be unique insureds in various classes.
#' We will house the premium and loss results here as well as who holds the policy for the round.
#' This will be the main table to updated in between rounds.
#' Loss is the losses as a random draw from the distribution
#' Lapse is if the insured moves to a new company. The Insured should always move to lowest premium, with ties settled by random draw.
#' This updating should be coded in the Shiny dashboard and reactive by a special moderator button.
dbWriteTable(conn=con
             ,name = 'Insured_File'
             ,value = insured_data
)


#' TABLE TO HOLD THE PREMIUM MOVES OVER TIME.
#' This would be what we read from mid-round to show the market moves and what we write to with a submit button.
#' Users will need to provide:
#' a) If they are privy to persistency classes, the premium by both classes
#' b) If they have 'standard' exhibits, we must write to all persistency classes for a given loss class.
#' 
dbWriteTable(conn=con
             ,name = 'Market_Premiums'
             ,value = data.frame(Insurer='unique',As_of_Round=0,Loss_Class='unique',Persistency_Class='unique',Premium=0)
            )

dbDisconnect(con)
rm(con)

#'AT THIS POINT WE HAVE OUR STARTING DATABASE AND DATA

