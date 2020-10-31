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
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-24-Uni-Dist.Rdata")
load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-24-Ind-Dist.Rdata")
Optimal_Settings
Optimal_Pars="Yes" # use optimal parameters based on Optimal_Settings
# Prob_Estimate="Exact"
# Chosen_Profit_Criteria=1
# Coef=1/5.8

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
  "3-liga",
  "super-league",
  "j1-league",
  "j2-league",
  "super-lig"
)
Prob_Estimates=c("Poisson", "Exact", "Negative_Binom", "Implied") # "Poisson", "Exact", "Negative_Binom", "Implied"
Chosen_Profit_Criterias=c(1, 2, 3) # choose the option to bet by : 
                             # 1 : maximum profit
                             # 2 : minimum profit
                             # 3 : a larger profit between the lowest option of "Over"s and the highest option of "Under"s
Coefs=1/seq(1, 16, by=0.1)
Last_Date="2020-10-24" # the last date in training set
#
t_1=system.time({
  source(paste0(CODE.dir.2, "Optimal_Parameter_Grid_Search.R"))
})

Training_Output[, ] %>% head
Test_Output[, ] %>% head

#*****************
# optimal settings
#*****************
Output=Training_Output %>% 
  left_join(Test_Output, by=c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef"))
Optimal_Settings=c()
All_Leagues=c(
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
for(League_Ind in 1:length(All_Leagues)){
  #League_Ind=1
  Selected_Training_Output=Training_Output[League==All_Leagues[League_Ind]&
                                             Prob_Est=="Exact"
                                             # Chosen_Profit_Criterion==1 &
                                             # Efficient>0
                                             # Efficient<2 &
                                             # Cum_Profit>1000 &
                                             # Bet_Std<1000 &
                                             # Profit_Std<500
                                             # Cum_Profit_Std<1000 &
                                             #Total_Bet<100000000 &
                                             #Min_Cum_Profit>0 &
                                             #Fixed_Bet_Prof_Ind>0 &
                                             #Kelly_Bet_Prof_Ind>0
                                           , ]%>%
    filter(Cum_Profit==max(Cum_Profit))
  
  # Selected_Training_Output=Output[League==All_Leagues[League_Ind]&
  #                                   Prob_Est=="Exact"&
  #                                   Efficient.x>0 & Efficient.y>0 &
  #                                   #Efficient.x<2 & Efficient.y<2 &
  #                                   # Fixed_Bet_Prof_Ind.x>0 & Fixed_Bet_Prof_Ind.y>0 &
  #                                   # Kelly_Bet_Prof_Ind.x>0 & Kelly_Bet_Prof_Ind.y>0
  #                                   Cum_Profit.x>100 & Cum_Profit.y>100
  #                                 , ] %>%
  #   filter(Cum_Profit.x==max(Cum_Profit.x))
  
  Optimal_Settings=rbind(Optimal_Settings, Selected_Training_Output)
}
Optimal_Settings %>% 
  left_join(Test_Output, by=c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")) %>% 
  filter(Fixed_Bet_Prof_Ind.y>10)
Optimal_Settings
data.table(Optimal_Settings$League,
           1/Optimal_Settings$Coef)

# #############################################
# Training=rbind(Training_Output) %>%
#   group_by(Prob_Est, Chosen_Profit_Criterion, Coef) %>%
#   summarise(Total_Profit=sum(Cum_Profit, na.rm=T)) %>%
#   as.data.table
# Training[order(Total_Profit, decreasing = T),] %>% head(20)
# Test=rbind(Test_Output) %>%
#   group_by(Prob_Est, Chosen_Profit_Criterion, Coef) %>%
#   summarise(Total_Profit=sum(Cum_Profit, na.rm=T)) %>%
#   as.data.table
# Test[order(Total_Profit, decreasing = T),] %>% head(20)
# 
# Comb=Training %>%
#   left_join(Test[, c("Prob_Est", "Chosen_Profit_Criterion", "Coef", "Total_Profit")], by=c("Prob_Est", "Chosen_Profit_Criterion", "Coef"))
# Comb=Comb[order(Total_Profit.y, decreasing = T),]
# Comb[Total_Profit.x>1000,]
# Comb[, Total_Profit:=Total_Profit.x+Total_Profit.y]
# Comb=Comb[order(Total_Profit, decreasing = T),]
# 
# #
# 1/0.188
# Optimal_Settings$Prob_Est="Exact"
# Optimal_Settings$Coef=1/14.1

# 
# #Optimal_Settings_Old<-Optimal_Settings
# # Optimal_Settings_Old[, c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")]
# Optimal_Settings[, c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")]
# Optimal_Settings[, Cum_Profit] %>% sum
# # 1/Optimal_Settings_Old$Coef
# 1/Optimal_Settings$Coef
# #############################################

#********************************************
#
# calculate balance with optimal setting ----
#
#********************************************
Capital=10000
Betting_Amount=1   # Amount*Proportion (input any amount)
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
First_Date="2020-01-12"
Last_Date=Sys.Date()+1 # last date to parse

#******************
# calculate balance
# Optimal_Pars="No" # use optimal parameters based on Optimal_Settings
# Prob_Estimate="Poisson"
# Chosen_Profit_Criteria=1
# Coef=1
Optimal_Pars="Yes" # use optimal parameters based on Optimal_Settings
Simul_Result=list()
Leagues=as.character(Optimal_Settings$League)
# Leagues="premier-league"
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
#save.image("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-24-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-24-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-11-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-04-Ind-Dist.Rdata")

