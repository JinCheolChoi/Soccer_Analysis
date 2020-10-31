#***********
# parameters
#***********
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

#*****************
# optimal settings
#*****************
Optimal_Settings=c()
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
for(League_Ind in 1:length(All_Leagues)){
  #League_Ind=1
  Selected_Output=Output[League==All_Leagues[League_Ind] &
                           Prob_Est=="Exact" &
                           #Chosen_Profit_Criterion==3 &
                           Efficient>0 &
                           # Efficient<2 &
                           #Cum_Profit>1000 &
                           # Bet_Std<1000 &
                           # Profit_Std<1000 &
                           #Cum_Profit_Std<1000 &
                           #Total_Bet<100000000 &
                           Min_Cum_Profit>0 &
                           Fixed_Bet_Prof_Ind>0 &
                           Kelly_Bet_Prof_Ind>0
                         , ] %>% 
    filter(Cum_Profit==max(Cum_Profit))
  
  Optimal_Settings=rbind(Optimal_Settings, Selected_Output)
}
Optimal_Settings

