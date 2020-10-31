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
Country="england"
Leagues=c("premier-league", "championship", "league-one", "league-two") # "premier-league", "championship", "league-one", "league-two", "national-league"

#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
system_sleep=3
data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"
source(paste0(CODE.dir.1, "Functions.R"))
lapply(c("data.table",
         "rvest",
         "stringr",
         "dplyr",
         "magrittr",
         "RSelenium"), checkpackages)

# In docker, run the following line first 
#docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
source(paste0(CODE.dir.2, "Extract_Over_under_score_odds.R"))

#***************************
#
# Extract_Data_Game_Results
#
#***************************
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds_data/"
source(paste0(CODE.dir.2, "Extract_Data_Game_Results.R"))

#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
source(paste0(CODE.dir.1, "Functions.R"))
source(paste0(CODE.dir.2, "SA_Functions.R"))
lapply(c("dplyr", 
         "data.table",
         "profvis",
         "magrittr",
         
         "ggplot2",
         "readr" # readr::parse_number
         
), 
checkpackages)
Results_Manipulation=Over_Under_Score_Odds_Manipulation=Over_Under_Score_Algorithm=""
Kelly_Method="Poisson" # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=3     # 1 (by maximum profit)
                             # 2 (by minimum profit)
                             # 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)
Years=2020:2021
source(paste0(CODE.dir.2, "Over_Under_Score_Algorithm.R"))


#*********************************************
#
# Calculate_Balance (based on chosen bettings)
#
#*********************************************
Capital=10000
source(paste0(CODE.dir.2, "Calculate_Balance.R"))


Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "Bet", "Profit", "Daily_Balance")]

# Chosen_Option_Prob



