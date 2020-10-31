#********************
#
# empty the workspace
#
#********************
rm(list=ls())

#*******************
#
# set directory path
#
#*******************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
CODE.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/"

Year=2020
Countries=c(
  "england",
  "spain",
  "italy",
  "netherlands",
  "germany",
  "china",
  "japan",
  "turkey"
  )
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
# italy : "serie-a"
# netherlands : "eerste-divisie"
# germany : "3-liga"
# china : "super-league"
# japan : "j1-league", "j2-league"
# turkey : "super-lig
Leagues=c(
  "premier-league",
  "championship",
  "league-one",
  "league-two",
  "laliga",
  "serie-a",
  "eerste-divisie",
  "3-liga",
  "super-league",
  "j1-league",
  "j2-league",
  "super-lig"
  )
source(paste0(CODE.dir.1, "Functions.R"))
source(paste0(CODE.dir.2, "SA_Functions.R"))

lapply(c("data.table",
         "rvest",
         "stringr",
         "dplyr",
         "magrittr",
         "RSelenium",
         
         "profvis",
         "ggplot2",
         "readr", # readr::parse_number
         "reshape2"), # melt() and dcast()
       checkpackages)

#*******************************
#
# Extract_Data_Game_Results ----
#
#*******************************
Years=2020
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
lapply(c("data.table",
         "rvest"), checkpackages)
source(paste0(CODE.dir.2, "Extract_Data_Game_Results.R"))


#********************************
#
# Over_Under_Score_Algorithm ----
#
#********************************
# load
#*****
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Uni-Dist.Rdata")
load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Ind-Dist.Rdata")
Optimal_Settings

Optimal_Pars="No" # use optimal parameters based on Optimal_Settings
Prob_Estimate="Exact"
Chosen_Profit_Criteria=1
Coef=1/5.8

Countries=c(
  "england",
  "spain",
  "italy",
  "netherlands",
  "germany",
  "china",
  "japan",
  "turkey"
)
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
# italy : "serie-a"
# netherlands : "eerste-divisie"
# germany : "3-liga"
# china : "super-league"
# japan : "j1-league", "j2-league"
# turkey : "super-lig
Leagues=c(
  "premier-league",
  "championship",
  "league-one",
  "league-two",
  "laliga",
  "serie-a",
  "eerste-divisie",
  "3-liga",
  "super-league",
  "j1-league",
  "j2-league",
  "super-lig"
)

data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
# data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
log.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Log/Over_under_score_odds/"

source(paste0(CODE.dir.1, "Functions.R"))
source(paste0(CODE.dir.2, "SA_Functions.R"))

#*********
# save log
#*********
setwd(paste0(log.dir))
#setwd(paste0(log.dir,  Country, "/", League, "/"))
Loop=0
while(Loop==0){
  # generate log file
  New_Log=0
  Log_N=1
  while(New_Log==0){
    Log_File_Lists=list.files(log.dir)
    File_Name=paste0(Sys.Date(), "-", Log_N, ".txt")
    if(sum(Log_File_Lists%in%File_Name)>0){
      Log_N=Log_N+1
    }else{
      my_log=file(File_Name)
      New_Log=1
    }
  }
  sink(my_log, append = TRUE, type = "output") # Writing console output to log file
  sink(my_log, append = TRUE, type = "message")
  
  # In docker, run the following line first 
  #docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
  system_sleep=3
  source(paste0(CODE.dir.2, "Extract_Over_under_score_odds.R"))
  
  # Close connection to log file
  closeAllConnections()
  
  # loop every 3 hours
  Sys.sleep(6*60*60)
}

#*************************************************************
#
# Calculate_Balance by League (based on a chosen setting) ----
#
#*************************************************************
t_1=system.time({
  Capital=10000
  Years=2020
  data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
  output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
  Betting_Amount=10   # Amount*Proportion (input any amount)
  # Kelly
  Simulation="Yes"       # No = calculate balance based on actual bettings
  Optimal_Pars="No"      # don't use optimal parameters based on Optimal_Settings
  
  #***********
  # parameters
  #***********
  # england : "premier-league", "championship", "league-one", "league-two"
  # spain : "laliga"
  # italy : "serie-a"
  # netherlands : "eerste-divisie"
  # germany : "3-liga"
  # china : "super-league"
  # japan : "j1-league", "j2-league"
  # turkey : "super-lig
  All_Leagues=c(
    "premier-league",
    "championship",
    "league-one",
    "league-two",
    "laliga",
    "serie-a",
    "eerste-divisie",
    "3-liga"
    # "super-league",
    # "j1-league",
    # "j2-league",
    # "super-lig"
  )
  Prob_Estimates=c("Poisson", "Exact", "Negative_Binom", "Implied")
  Chosen_Profit_Criterias=c(1, 2, 3)
  Coefs=1/seq(1, 16, by=0.1)
  # Prob_Estimates=c("Exact")
  # Chosen_Profit_Criterias=c(1)
  # Coefs=1/5.8
  Output=expand.grid(All_Leagues, 
                     Prob_Estimates, 
                     Chosen_Profit_Criterias,
                     Coefs)
  colnames(Output)=c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")
  
  #******************
  # calculate balance
  #******************
  source(paste0(CODE.dir.2, "SA_Functions.R"))
  for(Ind in 1:nrow(Output)){
    Leagues=as.character(Output[Ind, "League"])
    Prob_Estimate=as.character(Output[Ind, "Prob_Est"])
    Chosen_Profit_Criteria=Output[Ind, "Chosen_Profit_Criterion"]
    Coef=Output[Ind, "Coef"]
    
    # Ind=3679
    # Leagues=as.character(Output[Ind, ]$League)
    # Prob_Estimate=as.character(Output[Ind, ]$Prob_Est)
    # Chosen_Profit_Criteria=Output[Ind, ]$Chosen_Profit_Criterion
    # Coef=Output[Ind, ]$Coef
    
    # Country
    if(Leagues%in%c("premier-league",
                    "championship",
                    "league-one",
                    "league-two")){
      Countries="england"
    }else if(Leagues%in%c("laliga")){
      Countries="spain"
    }else if(Leagues%in%c("serie-a")){
      Countries="italy"
    }else if(Leagues%in%c("eerste-divisie")){
      Countries="netherlands"
    }else if(Leagues%in%c("3-liga")){
      Countries="germany"
    }else if(Leagues%in%c("super-league")){
      Countries="china"
    }else if(Leagues%in%c("j1-league",
                          "j2-league")){
      Countries="japan"
    }else if(Leagues%in%c("super-lig")){
      Countries="turkey"
    }else if(Leagues%in%c("")){
      Countries=""
    }
    
    source(paste0(CODE.dir.2, "Calculate_Balance.R"))
    
    if(length(Final_Combined_Over_Under_Score_Odds)!=0){
      # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
      if(Betting_Amount=="Kelly"){
        Output[Ind, "Ind"]=Ind
        
        Output[Ind, "Min_Balance"]=Final_Combined_Over_Under_Score_Odds[which.min(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
        Output[Ind, "Balance"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Daily_Balance")]
        Output[Ind, "Max_Balance"]=Final_Combined_Over_Under_Score_Odds[which.max(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
        Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
        Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
        Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
        Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
        Output[Ind, "Daily_Balance_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Daily_Balance)
        
        Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
        Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
        Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
          nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
        Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
          prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
        
      }else{
        Output[Ind, "Ind"]=Ind
        
        Output[Ind, "Min_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.min(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
        Output[Ind, "Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Cumulative_Profit")]
        Output[Ind, "Max_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.max(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
        Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
        Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
        Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
        Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
        Output[Ind, "Cum_Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)
        
        Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
        Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
        Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
          nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
        Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
          prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
        
      }
    }
    
    Final_Combined_Over_Under_Score_Odds=c()
    print(paste0(Ind, " out of ", nrow(Output)))
  }
  Output=as.data.table(Output)
  Output[, Efficient:=Cum_Profit/Total_Bet]
})

#********************************************
#
# calculate balance with optimal setting ----
#
#********************************************
Capital=10000
Betting_Amount=10   # Amount*Proportion (input any amount)
# Kelly
Simulation="Yes"       # No = calculate balance based on actual bettings
Years=2020

# find the best setting for each league
Leagues=c(
  "premier-league",
  "championship",
  "league-one",
  "league-two",
  "laliga",
  "serie-a",
  "eerste-divisie",
  "3-liga",
  "super-league",
  "j1-league",
  "j2-league",
  "super-lig"
)

#
Optimal_Settings=c()
for(League_Ind in 1:length(Leagues)){
  #League_Ind=1
  Selected_Output=Output[League==Leagues[League_Ind] &
                           Prob_Est=="Exact" &
                           #Chosen_Profit_Criterion==3 &
                           Efficient>0 &
                           #Efficient<2 &
                           Cum_Profit>1000 &
                           #Bet_Std<1000 &
                           #Profit_Std<1000 &
                           #Cum_Profit_Std<1000 &
                           #Total_Bet<100000000 &
                           Min_Cum_Profit>0 &
                           Fixed_Bet_Prof_Ind>0 &
                           Kelly_Bet_Prof_Ind>0, ] %>% 
    filter(Efficient==max(Efficient))
  
  Optimal_Settings=rbind(Optimal_Settings, Selected_Output)
}
#Optimal_Settings_Old<-Optimal_Settings
# Optimal_Settings_Old[, c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")]
Optimal_Settings[, c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")]
Optimal_Settings[, Cum_Profit] %>% sum
1/Optimal_Settings_Old$Coef
1/Optimal_Settings$Coef

#******************
# calculate balance
Optimal_Pars="Yes" # use optimal parameters based on Optimal_Settings
Simul_Result=list()
Leagues=as.character(Optimal_Settings$League)
source(paste0(CODE.dir.2, "SA_Functions.R"))
source(paste0(CODE.dir.2, "Calculate_Balance.R"))
# if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
if(Betting_Amount=="Kelly"){
  Simul_Result=list(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Daily_Balance")],
                    data.table(
                      Date=unique(Final_Combined_Over_Under_Score_Odds$Date),
                      Final_Combined_Over_Under_Score_Odds %>% 
                        group_by(Date) %>% 
                        summarise(N=n(), Bet=sum(Bet), Profit=sum(Profit)) %>% 
                        dplyr::select(N, Bet, Profit) %>% 
                        as.data.table,
                      Final_Combined_Over_Under_Score_Odds %>% 
                        dplyr::select(Date, Daily_Balance) %>% 
                        unique %>% 
                        dplyr::select(Daily_Balance) %>% 
                        as.data.table
                    )
  )
}else{
  Simul_Result=list(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Cumulative_Profit")],
                    data.table(
                      Date=unique(Final_Combined_Over_Under_Score_Odds$Date),
                      Final_Combined_Over_Under_Score_Odds %>% 
                        group_by(Date) %>% 
                        summarise(N=n(), Bet=sum(Bet), Profit=sum(Profit)) %>% 
                        dplyr::select(N, Bet, Profit) %>% 
                        as.data.table,
                      Final_Combined_Over_Under_Score_Odds %>% 
                        dplyr::select(Date, Cumulative_Profit) %>% 
                        unique %>% 
                        dplyr::select(Cumulative_Profit) %>% 
                        as.data.table
                    ),
                    paste0("Total betting : ", sum(Final_Combined_Over_Under_Score_Odds[, Bet])))
  
}

Simul_Result[[1]]
Combined_Over_Under_Score_Odds[Date=="2020-10-17", ]
Kelly_Criterion(0, 1.87, 0.5)

Simul_Result[[1]][Date=="2020-10-17", ] %>% 
  group_by(Date) %>% 
  summarise(Profit=sum(Profit)) %>% 
  select(Profit) %>% sum

Simul_Result[[1]][, ] %>% 
  group_by(Date, League) %>% 
  summarise(Profit=sum(Profit), Bet=sum(Bet), N=n()) %>% 
  as.data.table

Simul_Result[[1]][Date>="2020-10-17", ] %>% 
  group_by(League) %>% 
  summarise(Profit=sum(Profit), Bet=sum(Bet), N=n()) %>% 
  as.data.table

Simul_Result[[1]][Profit>0, ] %>% 
  # filter(Profit>0) %>% 
  group_by(Date) %>% 
  summarise(Profit_Bet=sum(Profit, Bet), N=n()) %>% 
  as.data.table

#**************
# save and load
#**************
#save.image("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Uni-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-11-Ind-Dist.Rdata")

