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
Countries=c("england", "spain", "italy", "netherlands", "germany")
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
# italy : "serie-a"
# netherlands : "eerste-divisie"
# germany : "3-liga
Leagues=c("premier-league",
          "championship",
          "league-one",
          "league-two",
          "laliga",
          "serie-a",
          "eerste-divisie",
          "3-liga")
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
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
lapply(c("data.table",
         "rvest"), checkpackages)
All_Leagues=c("3-liga")
source(paste0(CODE.dir.2, "Extract_Data_Game_Results.R"))


#********************************
#
# Over_Under_Score_Algorithm ----
#
#********************************
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
# data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
log.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Log/Over_under_score_odds/"

# settings to calculate kelly scores
Prob_Estimate="Exact"    # Poisson, Exact, Negative_Binom, Implied
Chosen_Profit_Criteria=1     # 1 (by maximum profit)
                             # 2 (by minimum profit)
                             # 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)
Coef=1/6
#*********
# save log
#*********
setwd(paste0(log.dir))
#setwd(paste0(log.dir,  Country, "/", League, "/"))
my_log=file(paste0(Sys.Date(), ".txt"))
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

#*************************************************************
#
# Calculate_Balance by League (based on a chosen setting) ----
#
#*************************************************************
Capital=10000
Years=2021
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/"
Betting_Amount=10000   # Amount*Proportion (input any amount)
                       # Kelly
Simulation="Yes"       # No = calculate balance based on actual bettings

#***********
# parameters
#***********
Countries=c("england", "spain", "italy", "netherlands", "germany")
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
# italy : "serie-a"
# netherlands : "eerste-divisie"
All_Leagues=c("premier-league", "championship", "league-one", "league-two", 
              "laliga", 
              "serie-a",
              "eerste-divisie",
              "3-liga"
              )
Prob_Estimates=c("Poisson", "Exact", "Negative_Binom", "Implied")
Chosen_Profit_Criterias=c(1, 2, 3)
Coefs=1/seq(1, 16, by=0.1)
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
  if(Leagues%in%c("premier-league", "championship", "league-one", "league-two")){
    Country="england"
  }else if(Leagues%in%c("laliga")){
    Country="spain"
  }else if(Leagues%in%c("serie-a")){
    Country="italy"
  }else if(Leagues%in%c("eerste-divisie")){
    Country="netherlands"
  }else if(Leagues%in%c("3-liga")){
    Country="germany"
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


#********************************************
#
# calculate balance with optimal setting ----
#
#********************************************
# find the best setting for each league
All_Leagues=c("premier-league", "championship", "league-one", "league-two", 
              "laliga", 
              "serie-a",
              "eerste-divisie",
              "3-liga")

#
Optimal_Settings=c()
for(League_Ind in 1:length(All_Leagues)){
  Selected_Output=Output[League==All_Leagues[League_Ind] &
                           Efficient>0 &
                           #Efficient<2 &
                           Cum_Profit>5000 &
                           Bet_Std < 4000 &
                           Cum_Profit_Std<4000 &
                           Min_Cum_Profit>0 &
                           Fixed_Bet_Prof_Ind>0 &
                           Kelly_Bet_Prof_Ind>0, ] %>% 
    filter(Efficient==max(Efficient))
  
  Optimal_Settings=rbind(Optimal_Settings, Selected_Output)
}
Optimal_Settings
Optimal_Settings[, Cum_Profit] %>% sum

#******************
# calculate balance
Simul_Result=list()
Leagues=as.character(Optimal_Settings$League)
source(paste0(CODE.dir.2, "SA_Functions.R"))
source(paste0(CODE.dir.2, "Calculate_Balance.R"))
# if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
if(Betting_Amount=="Kelly"){
  Simul_Result=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Daily_Balance")]),
                    print(unique(Final_Combined_Over_Under_Score_Odds$Daily_Balance)))
}else{
  Simul_Result=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Cumulative_Profit")]),
                    print(unique(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)),
                    print(paste0("Total betting : ", sum(Final_Combined_Over_Under_Score_Odds[, Bet]))))
  
}
Simul_Result[[1]][League=="league-two", Profit] %>% sum
Simul_Result[[1]][League=="league-two", ]




#********************************************************
#********************************************************
#
Combined_Output=Output %>% 
  group_by(Prob_Est, Chosen_Profit_Criterion, Coef) %>% 
  summarise(Cum_P=sum(Cum_Profit), Total_B=sum(Total_Bet)) %>% 
  as.data.table
Combined_Output[, Efficient:=Cum_P/Total_B]
Combined_Output[Cum_P<1000000 & 
                  Cum_P>0, ] %>% 
  dplyr::filter(Cum_P==max(Cum_P))
Combined_Output[Cum_P<1000000 & 
                  Cum_P>0 &
                  Chosen_Profit_Criterion==1 &
                  Coef<=1/5.8+0.0000001 &
                  Coef>=1/5.8-0.0000001, ]
Odds_vs_Cum_Profit=c()
for(i in 1:length(Coefs)){
  Odds_vs_Cum_Profit$Odds[i]=1/Coefs[i]
  
  Odds_vs_Cum_Profit$Cum_Profit[i]=(Output[
    Prob_Est=="Exact" & 
      Chosen_Profit_Criterion==1 & 
      Coef<=Coefs[i]+0.0000001&
      Coef>=Coefs[i]-0.0000001,
    Cum_Profit] %>% sum)
}

Target_Ind=which(Odds_vs_Cum_Profit$Cum_Profit<1000000 &
                   Odds_vs_Cum_Profit$Cum_Profit>-1000000 &
                   Odds_vs_Cum_Profit$Cum_Profit>0)
plot(Odds_vs_Cum_Profit$Odds[Target_Ind], Odds_vs_Cum_Profit$Cum_Profit[Target_Ind])

#********************************************************
#********************************************************

Simul_Result=list()
Simul_Result_Ind=1
# Ind=3601
# Coef=Output[Ind, ]$Coef
# Leagues=as.character(Output[Ind, ]$League)
# Prob_Estimates=as.character(Output[Ind, ]$Prob_Est)
# Chosen_Profit_Criterias=Output[Ind, ]$Chosen_Profit_Criterion
Ind=3601
Coef=1/5.8
Leagues=c("premier-league", "championship", "league-one", "league-two", 
          "laliga", 
          "serie-a",
          "eerste-divisie",
          "3-liga")
Prob_Estimates=c("Exact")
Chosen_Profit_Criterias=1

source(paste0(CODE.dir.2, "SA_Functions.R"))
for(Prob_Estimate in Prob_Estimates){
  for(Chosen_Profit_Criteria in Chosen_Profit_Criterias){
    source(paste0(CODE.dir.2, "Calculate_Balance.R"))
    # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
    if(Betting_Amount=="Kelly"){
      Simul_Result[[Simul_Result_Ind]]=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Daily_Balance")]),
                                            print(unique(Final_Combined_Over_Under_Score_Odds$Daily_Balance)))
    }else{
      Simul_Result[[Simul_Result_Ind]]=list(print(Final_Combined_Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team", "N", "Result","Kelly", "Chosen_Option", "Chosen_Odds", "Expected_Profit_Ind", "Bet", "Profit", "Cumulative_Profit")]),
                                            print(unique(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)),
                                            print(paste0("Total betting : ", sum(Final_Combined_Over_Under_Score_Odds[, Bet]))))
      
    }
    
    Simul_Result_Ind=Simul_Result_Ind+1
  }
}

Output[League=="premier-league" &
         Prob_Est=="Implied" &
         Chosen_Profit_Criterion==1 &
         Coef>=(1/5.8)-0.000001 &
         Coef<=(1/5.8)+0.000001, ]

plot(Simul_Result[[1]][[1]][, c("Date", "Cumulative_Profit")], type='o')
Simul_Result[[1]][[1]][League=="premier-league", Bet] %>% sum
Simul_Result[[1]][[1]][League=="premier-league", Profit] %>% sum
Simul_Result[[2]]
Simul_Result[[3]]
Simul_Result[[4]]
Simul_Result[[5]]
Simul_Result[[6]]
Simul_Result[[7]]
Simul_Result[[8]]
Simul_Result[[9]]
Simul_Result[[10]]
Simul_Result[[11]]
Simul_Result[[12]]

#********************************************************
#********************************************************
Prob_Estimate="Exact" # Poisson, Negative_Binom, Exact, Implied
Simulation="Yes"
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
                                                 Prob_Estimate))
}


Leagues=c("premier-league", "championship", "league-one", "league-two", "laliga", "serie-a")
Odds_List=seq(0, 15, by=0.1)
Out=matrix(NA, nrow=length(Leagues), ncol=length(Odds_List))
for(League_Text_Ind in 1:length(Leagues)){
  for(Odds_Ind in 1:length(Odds_List)){
    
    Target_Data=Combined_Long_Table[!is.na(Odds) & 
                                      !is.infinite(Kelly) &
                                      !is.na(Kelly) &
                                      League==Leagues[League_Text_Ind] &
                                      Odds>=Odds_List[Odds_Ind] &
                                      Odds<Odds_List[Odds_Ind]+0.01, ]
    if(nrow(Target_Data)>0){
      Out[League_Text_Ind, Odds_Ind]=prod((1+Target_Data[, Kelly]*
                                             (Target_Data[, Odds]-1))^Target_Data[, Result])*
        prod((1-Target_Data[, Kelly])^(1-Target_Data[, Result]))-1
    }
    
  }
  
}

Posi_Profit=which(Out>0, arr.ind=T) %>% as.data.table
Odds_List[Posi_Profit[row==1, col]]
Odds_List[Posi_Profit[row==2, col]]
Odds_List[Posi_Profit[row==3, col]]
Odds_List[Posi_Profit[row==4, col]]
Odds_List[Posi_Profit[row==5, col]]

which.max(Posi_Profit[row==1, col])
which.max(Posi_Profit[row==2, col])
which.max(Posi_Profit[row==3, col])
which.max(Posi_Profit[row==4, col])
which.max(Posi_Profit[row==5, col])

Combined_Long_Table[!is.na(Odds) & 
                      !is.infinite(Kelly), Odds] %>% summary

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


#**************
# save and load
#**************
#save.image("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Ind-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Uni-Dist.Rdata")
#load("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/2020-10-14-Ind-Dist.Rdata")

