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

Year=2021
Countries=c("england")
Leagues=c("premier-league", "championship", "league-one", "league-two") # "premier-league", "championship", "league-one", "league-two", "national-league"


#***************************
#
# Extract_Data_Game_Results
#
#***************************
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
source(paste0(CODE.dir.2, "Extract_Data_Game_Results.R"))


#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
# data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
log.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Log/Over_under_score_odds/"
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
         "readr"), # readr::parse_number
       checkpackages)

# settings to calculate kelly scores
Kelly_Method="Exact_Dist"    # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=1     # 1 (by maximum profit)
                             # 2 (by minimum profit)
                             # 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)

#*********
# save log
#*********
setwd(paste0(log.dir))
#setwd(paste0(log.dir,  Country, "/", League, "/"))
my_log=file("2020-09-23.txt")
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")
#closeAllConnections() # Close connection to log file

Loop=0
while(Loop==0){
  system_sleep=3
  
  # In docker, run the following line first 
  #docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
  source(paste0(CODE.dir.2, "Extract_Over_under_score_odds.R"))
  
  Sys.sleep(3*60*60) # loop every 3 hours
}


#*********************************************
#
# Calculate_Balance (based on chosen bettings)
#
#*********************************************
Leagues=c("premier-league", "championship", "league-one", "league-two") # "premier-league", "championship", "league-one", "league-two", "national-league"

Capital=10000
Years=2020:2021
# data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"

Betting_Amount=10000   # Fixed (input any amount)
                       # Kelly
Simulation="No"       # No = calculate balance based on actual bettings
#****************************************************
# change two values below only given Simulation="Yes"
#****************************************************
Kelly_Method="Exact_Dist" # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=1  # 1 (by maximum profit)
# 2 (by minimum profit)
# 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)
#****************************************************
source(paste0(CODE.dir.2, "Calculate_Balance.R"))

Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "N", "Result","Kelly", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Daily_Balance")] %>% tail(10)
unique(Final_Combined_Over_Under_Score_Odds$Daily_Balance)

# if >0, profitable
(mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])-1)*(sum(Final_Combined_Over_Under_Score_Odds[, Result])/nrow(Final_Combined_Over_Under_Score_Odds))-(1-(sum(Final_Combined_Over_Under_Score_Odds[, Result])/nrow(Final_Combined_Over_Under_Score_Odds)))


