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
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Uni-Dist.Rdata")
# load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Ind-Dist.Rdata")
# Optimal_Settings
Optimal_Pars="No" # use optimal parameters based on Optimal_Settings
Prob_Estimate="Exact"
Chosen_Profit_Criteria=1
Coef=1/5.8

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

#******************************
#
# Optimal parameter values ----
#
#******************************
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
Capital=10000
Years=2020
Betting_Amount=10   # Amount*Proportion (input any amount)
# Kelly
Simulation="Yes"       # No = calculate balance based on actual bettings
Optimal_Pars="No"      # don't use optimal parameters based on Optimal_Settings
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
Prob_Estimates=c("Poisson", "Exact", "Negative_Binom", "Implied") # "Poisson", "Exact", "Negative_Binom", "Implied"
Chosen_Profit_Criterias=c(1) # choose the option to bet by : 
                             # 1 : maximum profit
                             # 2 : minimum profit
                             # 3 : a larger profit between the lowest option of "Over"s and the highest option of "Under"s
Coefs=1/seq(1, 16, by=0.1)
Last_Date="2020-10-04" # the last date in training set
#
t_1=system.time({
  source(paste0(CODE.dir.2, "Optimal_Parameter_Grid_Search.R"))
})


#********************************************
#
# calculate balance with optimal setting ----
#
#********************************************
Capital=10000
Betting_Amount=10   # Amount*Proportion (input any amount)
# Kelly
Simulation="Yes"    # No = calculate balance based on actual bettings
Years=2020
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
First_Date="2020-10-15"
Last_Date=Sys.Date()+1 # last date to parse

#******************
# calculate balance
Optimal_Pars="Yes" # use optimal parameters based on Optimal_Settings
Simul_Result=list()
Leagues=as.character(Optimal_Settings$League)
source(paste0(CODE.dir.2, "SA_Functions.R"))
source(paste0(CODE.dir.2, "Calculate_Balance_1.R"))
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date>=First_Date, ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date<=Last_Date, ]
source(paste0(CODE.dir.2, "Calculate_Balance_2.R"))

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
Simul_Result

Simul_Result[[1]][Date>=First_Date, ]

Simul_Result[[1]][Date>=First_Date, ] %>% 
  group_by(Date) %>% 
  summarise(Profit=sum(Profit)) %>% 
  select(Profit) %>% sum

Simul_Result[[1]][Date>=First_Date, ] %>% 
  group_by(Date, League) %>% 
  summarise(Profit=sum(Profit), Bet=sum(Bet), N=n()) %>% 
  as.data.table

Simul_Result[[1]][Date>=First_Date, ] %>% 
  group_by(League) %>% 
  summarise(Profit=sum(Profit), Bet=sum(Bet), N=n()) %>% 
  as.data.table

Simul_Result[[1]][Date>=First_Date&Profit>0, ] %>% 
  # filter(Profit>0) %>% 
  group_by(Date) %>% 
  summarise(Profit_Bet=sum(Profit, Bet), N=n()) %>% 
  as.data.table

#**************
# save and load
#**************
#save.image("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-20-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-20-2-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-20-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-19-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-11-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-04-Ind-Dist.Rdata")



