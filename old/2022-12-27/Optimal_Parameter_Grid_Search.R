#***********
#
# parameters
#
#***********
Training_Output=expand.grid(All_Leagues, 
                            Prob_Estimates, 
                            Chosen_Profit_Criterias,
                            Coefs)
colnames(Training_Output)=c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")
Test_Output=expand.grid(All_Leagues, 
                        Prob_Estimates, 
                        Chosen_Profit_Criterias,
                        Coefs)
colnames(Test_Output)=c("League", "Prob_Est", "Chosen_Profit_Criterion", "Coef")

#******************
#
# calculate balance
#
#******************
source(paste0(CODE.dir.2, "SA_Functions.R"))
for(Ind in 1:nrow(Training_Output)){
  Leagues=as.character(Training_Output[Ind, "League"])
  Prob_Estimate=as.character(Training_Output[Ind, "Prob_Est"])
  Chosen_Profit_Criteria=Training_Output[Ind, "Chosen_Profit_Criterion"]
  Coef=Training_Output[Ind, "Coef"]
  
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
  
  # calculate balance
  source(paste0(CODE.dir.2, "Calculate_Balance_1.R"))
  Full_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds
  
  #*************
  # Training set
  #*************
  Combined_Over_Under_Score_Odds=Full_Combined_Over_Under_Score_Odds[Date<=Training_Last_Date, ]
  source(paste0(CODE.dir.2, "Calculate_Balance_2.R"))
  Combined_Over_Under_Score_Odds
  
  # Training_Output
  if(length(Final_Combined_Over_Under_Score_Odds)!=0){
    # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
    if(Betting_Amount=="Kelly"){
      Training_Output[Ind, "Ind"]=Ind
      
      Training_Output[Ind, "Min_Balance"]=Final_Combined_Over_Under_Score_Odds[which.min(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
      Training_Output[Ind, "Balance"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Daily_Balance")]
      Training_Output[Ind, "Max_Balance"]=Final_Combined_Over_Under_Score_Odds[which.max(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
      Training_Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
      Training_Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
      Training_Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
      Training_Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
      Training_Output[Ind, "Daily_Balance_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Daily_Balance)
      
      Training_Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
      Training_Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
      Training_Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
        nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
      Training_Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
        prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
      
    }else{
      Training_Output[Ind, "Ind"]=Ind
      
      Training_Output[Ind, "Min_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.min(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
      Training_Output[Ind, "Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Cumulative_Profit")]
      Training_Output[Ind, "Max_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.max(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
      Training_Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
      Training_Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
      Training_Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
      Training_Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
      Training_Output[Ind, "Cum_Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)
      
      Training_Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
      Training_Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
      Training_Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
        nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
      Training_Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
        prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
      
    }
  }
  Final_Combined_Over_Under_Score_Odds=c()
  
  
  #*********
  # Test set
  #*********
  Combined_Over_Under_Score_Odds=Full_Combined_Over_Under_Score_Odds[Date>Training_Last_Date &
                                                                       Date<=Test_Last_Date, ]
  source(paste0(CODE.dir.2, "Calculate_Balance_2.R"))
  # Test_Output
  if(length(Final_Combined_Over_Under_Score_Odds)!=0){
    # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
    if(Betting_Amount=="Kelly"){
      Test_Output[Ind, "Ind"]=Ind
      
      Test_Output[Ind, "Min_Balance"]=Final_Combined_Over_Under_Score_Odds[which.min(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
      Test_Output[Ind, "Balance"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Daily_Balance")]
      Test_Output[Ind, "Max_Balance"]=Final_Combined_Over_Under_Score_Odds[which.max(Daily_Balance), .SD, .SDcols=c("Daily_Balance")]
      Test_Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
      Test_Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
      Test_Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
      Test_Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
      Test_Output[Ind, "Daily_Balance_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Daily_Balance)
      
      Test_Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
      Test_Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
      Test_Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
        nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
      Test_Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
        prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
      
    }else{
      Test_Output[Ind, "Ind"]=Ind
      
      Test_Output[Ind, "Min_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.min(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
      Test_Output[Ind, "Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[nrow(Final_Combined_Over_Under_Score_Odds), .SD, .SDcols=c("Cumulative_Profit")]
      Test_Output[Ind, "Max_Cum_Profit"]=Final_Combined_Over_Under_Score_Odds[which.max(Cumulative_Profit), .SD, .SDcols=c("Cumulative_Profit")]
      Test_Output[Ind, "N_of_Bet"]=nrow(Final_Combined_Over_Under_Score_Odds)
      Test_Output[Ind, "Total_Bet"]=sum(Final_Combined_Over_Under_Score_Odds$Bet)
      Test_Output[Ind, "Bet_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Bet)
      Test_Output[Ind, "Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Profit)
      Test_Output[Ind, "Cum_Profit_Std"]=sd(Final_Combined_Over_Under_Score_Odds$Cumulative_Profit)
      
      Test_Output[Ind, "Avg_Winning_Rate"]=mean(Final_Combined_Over_Under_Score_Odds[, Result])
      Test_Output[Ind, "Avg_Chosen_Odds"]=mean(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds])
      Test_Output[Ind, "Fixed_Bet_Prof_Ind"]=sum(Final_Combined_Over_Under_Score_Odds[Result==1, Chosen_Odds]-1)-
        nrow(Final_Combined_Over_Under_Score_Odds[Result==0, ])
      Test_Output[Ind, "Kelly_Bet_Prof_Ind"]=prod((1+Final_Combined_Over_Under_Score_Odds[, Kelly]*(Final_Combined_Over_Under_Score_Odds[, Chosen_Odds]-1))^Final_Combined_Over_Under_Score_Odds[, Result])*
        prod((1-Final_Combined_Over_Under_Score_Odds[, Kelly])^(1-Final_Combined_Over_Under_Score_Odds[, Result]))-1
      
    }
  }
  Final_Combined_Over_Under_Score_Odds=c()
  
  print(paste0(Ind, " out of ", nrow(Training_Output)))
}
Training_Output=as.data.table(Training_Output)
Training_Output[, Efficient:=Cum_Profit/Total_Bet]
Test_Output=as.data.table(Test_Output)
Test_Output[, Efficient:=Cum_Profit/Total_Bet]


