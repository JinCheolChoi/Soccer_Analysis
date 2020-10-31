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
Countries=c("england", "spain")
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
Leagues=c("premier-league", "championship", "league-one", "league-two", "laliga")
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

#***************************
#
# Extract_Data_Game_Results
#
#***************************
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
lapply(c("data.table",
         "rvest"), checkpackages)
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
my_log=file("2020-10-01-1.txt")
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")
#closeAllConnections() # Close connection to log file

Loop=0
while(Loop==0){
  
  # In docker, run the following line first 
  #docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
  system_sleep=3
  source(paste0(CODE.dir.2, "Extract_Over_under_score_odds.R"))
  
  Sys.sleep(3*60*60) # loop every 3 hours
}

#*********************************************
#
# Calculate_Balance (based on chosen bettings)
#
#*********************************************
Countries=c("england")
# england : "premier-league", "championship", "league-one", "league-two", "national-league"
# spain : "laliga"
Leagues=c("premier-league", "championship", "league-one", "league-two")


Capital=100000
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"

Betting_Amount="Kelly"   # Amount*Proportion (input any amount)
                       # Kelly
                       
Simulation="Yes"       # No = calculate balance based on actual bettings
#****************************************************
# change two values below only given Simulation="Yes"
#****************************************************
Kelly_Method="Exact_Dist" # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=1  # when choosing a kelly score
                          # 1 (by maximum profit)
                          # 2 (by minimum profit)
                          # 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)
#****************************************************
Simul_Result=list()
Simul_Result_Ind=1
source(paste0(CODE.dir.2, "SA_Functions.R"))
for(Kelly_Method in c("Exact_Dist", "Poisson", "Negative_Binom")){
  for(Chosen_Profit_Criteria in c(1, 2, 3)){
    source(paste0(CODE.dir.2, "Calculate_Balance.R"))
    # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
    if(Betting_Amount=="Kelly"){
      Simul_Result[[Simul_Result_Ind]]=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Daily_Balance")]),
                                            print(unique(Final_Combined_Over_Under_Score_Odds$Daily_Balance)))
    }else{
      Simul_Result[[Simul_Result_Ind]]=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Cumulative_Profit")]),
                                            print(unique(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)),
                                            print(paste0("Total betting : ", sum(Final_Combined_Over_Under_Score_Odds[, Bet]))))
      
    }
    
    Simul_Result_Ind=Simul_Result_Ind+1
  }
}
Simul_Result[[1]]
Simul_Result[[2]]
Simul_Result[[3]]
Simul_Result[[4]]
Simul_Result[[5]]
Simul_Result[[6]]
Simul_Result[[7]]
Simul_Result[[8]]
Simul_Result[[9]]

# winning rate
for(i in 1:9){
  print(mean(Simul_Result[[i]][[1]][, Result]))
}

# avg odds
for(i in 1:9){
  print(mean(Simul_Result[[i]][[1]][, Chosen_Odds]))
}

# if >0, profitable
for(i in 1:9){
  print(sum(Simul_Result[[i]][[1]][Result==1, Chosen_Odds]-1)-nrow(Simul_Result[[i]][[1]][Result==0, ]))
}

# if >0, profitable (Kelly)
for(i in 1:9){
  print(
    prod((1+Simul_Result[[i]][[1]][, Kelly]*(Simul_Result[[i]][[1]][, Chosen_Odds]-1))^Simul_Result[[i]][[1]][, Result])*
      prod((1-Simul_Result[[i]][[1]][, Kelly])^(1-Simul_Result[[i]][[1]][, Result]))-1
    
  )
}

##################################################################
Kelly_Method="Exact_Dist" # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=1
source(paste0(CODE.dir.2, "SA_Functions.R"))
source(paste0(CODE.dir.2, "Calculate_Balance.R"))
# Combined_Long_Table
Combined_Long_Table=c()
for(League_Text in Leagues){
  Combined_Long_Table=rbind(Combined_Long_Table, 
                            Long_Table_Generator(Combined_Over_Under_Score_Odds[League==League_Text, ], 
                                                 Combined_Results_Data, 
                                                 League_Text, 
                                                 Kelly_Method))
}
Combined_Long_Table[Result==1, c("Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                                   min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                            ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                                   min(30, max(Combined_Long_Table$Kelly, na.rm=T))))

Combined_Long_Table[Result==0, c("Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                                   min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                            ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                                   min(30, max(Combined_Long_Table$Kelly, na.rm=T))))
Combined_Long_Table[, c("Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                          min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                   ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                          min(30, max(Combined_Long_Table$Kelly, na.rm=T))))

Final_Combined_Over_Under_Score_Odds[Result==1, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                                                           min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                                                    ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                                                           min(30, max(Combined_Long_Table$Kelly, na.rm=T))))


Final_Combined_Over_Under_Score_Odds[Result==0, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                                                           min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                                                    ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                                                           min(30, max(Combined_Long_Table$Kelly, na.rm=T))))

Final_Combined_Over_Under_Score_Odds[, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Combined_Long_Table$Odds, na.rm=T)),
                                                                                  min(30, max(Combined_Long_Table$Odds, na.rm=T))),
                                                                           ylim=c(max(-30, min(Combined_Long_Table$Kelly, na.rm=T)), 
                                                                                  min(30, max(Combined_Long_Table$Kelly, na.rm=T))))





Final_Combined_Over_Under_Score_Odds[Result==1, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T)),
                                                                                           min(30, max(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T))),
                                                                                    ylim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T)), 
                                                                                           min(30, max(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T))))


Final_Combined_Over_Under_Score_Odds[Result==0, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T)),
                                                                                           min(30, max(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T))),
                                                                                    ylim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T)), 
                                                                                           min(30, max(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T))))

Final_Combined_Over_Under_Score_Odds[, c("Chosen_Odds", "Kelly")] %>% plot(xlim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T)),
                                                                                  min(30, max(Final_Combined_Over_Under_Score_Odds$Chosen_Odds, na.rm=T))),
                                                                           ylim=c(max(-30, min(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T)), 
                                                                                  min(30, max(Final_Combined_Over_Under_Score_Odds$Kelly, na.rm=T))))



#
# 
# 
# #
# colnames(Final_Combined_Over_Under_Score_Odds)
# 
# for(Col in c("Chosen_Odds",
#              "Chosen_Option_Empirical_Prob",
#              "N",
#              "Chosen_Option_Std",
#              "Kelly",
#              "Expected_Profit_Ind")){
#   assign(paste0("Cont_Result_vs._", Col),
#        Contingency_Table_Generator_Conti_X(Data=Final_Combined_Over_Under_Score_Odds,
#                                            Row_Var=Col,
#                                            Col_Var="Result",
#                                            Missing="Not_Include"))
# }
# Cont_Result_vs._Chosen_Odds
# Cont_Result_vs._Chosen_Option_Empirical_Prob
# Cont_Result_vs._Chosen_Option_Std
# Cont_Result_vs._Expected_Profit_Ind
# Cont_Result_vs._Kelly
# Cont_Result_vs._N
# 
# Chosen_Odds<=4
# Chosen_Option_Empirical_Prob>=0.6
# Chosen_Option_Std<0=.2
# Kelly>=0.07
# N<=6




