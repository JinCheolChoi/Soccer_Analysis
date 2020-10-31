
# Trainig_Data_set=Data_set[Date<"2000-01-01", ]
# Data_set=Data_set[Date>="2000-01-01", ]

# 
# Impirical_Probability=unique(Trainig_Data_set[,
#                                               .SD,
#                                               .SDcols=c("Home_Team", "Away_Team")])
# 
# # system.time({
# #   for(i in 1:nrow(Impirical_Probability)){
# #     Target_Training_Data_set=Trainig_Data_set[Home_Team==Impirical_Probability[i, Home_Team] &
# #                                                 Away_Team==Impirical_Probability[i, Away_Team], ]
# #     Home_Score=Target_Training_Data_set[, Home_Score]
# #     Away_Score=Target_Training_Data_set[, Away_Score]
# #     Impirical_Probability[i, `:=`(Home_Mean_Score=mean(Home_Score),
# #                                   Home_Var_Score=var(Home_Score),
# #                                   Away_Mean_Score=mean(Away_Score),
# #                                   Away_Var_Score=var(Away_Score))]
# #     
# #   }
# # })
# 
# #
# Distribution_Function=function(x){
#   Target_Training_Data_set=Trainig_Data_set[Home_Team==x[1] &
#                                               Away_Team==x[2], ]
#   Home_Score=Target_Training_Data_set[, Home_Score]
#   Away_Score=Target_Training_Data_set[, Away_Score]
#   
#   return(c(mean(Home_Score),
#            var(Home_Score),
#            mean(Away_Score),
#            var(Away_Score)))
# }
# 
# 
# system.time({
#   Empirical_Distribution=data.table(t(Impirical_Probability)) %>% 
#     sapply(function(x) Distribution_Function(x))
#   Impirical_Probability[, `:=`(Home_Mean_Score=Empirical_Distribution[1, ],
#                                Home_Var_Score=Empirical_Distribution[2, ],
#                                Away_Mean_Score=Empirical_Distribution[3, ],
#                                Away_Var_Score=Empirical_Distribution[4, ])]
# })



#  function 1
Empirical_Probability_Calculator=function(x, nsim=1000000){
  # Temp_Impirical_Probability=Impirical_Probability[Home_Team==x[2] &
  #                                                    Away_Team==x[3], ]
  #x=unlist(data.table(t(Target_Data_set))[, 5])
  
  
  if(x[2]%in%Data_set[Date<x[1], Home_Team] &
     x[3]%in%Data_set[Date<x[1], Away_Team]){
    
    Home_Score=Data_set[Date<x[1] &
                          Home_Team==x[2] &
                          Away_Team==x[3], Home_Score]
    Away_Score=Data_set[Date<x[1] &
                          Home_Team==x[2] &
                          Away_Team==x[3], Away_Score]
    
    if(length(Home_Score)==0 & length(Away_Score)==0){
      Home_Score=Data_set[Date<x[1] &
                            Home_Team==x[2], Home_Score]
      Away_Score=Data_set[Date<x[1] &
                            Away_Team==x[3], Away_Score]
      Mean_Home_Score=mean(Home_Score)
      Mean_Away_Score=mean(Away_Score)
      poisson_sample_1=rpois(nsim, lambda=Mean_Home_Score)
      poisson_sample_2=rpois(nsim, lambda=Mean_Away_Score)
    }else if(length(Home_Score)==0 & length(Away_Score)!=0){
      if(length(Home_Score)==0 & length(Away_Score)==0){
        Home_Score=Data_set[Date<x[1] &
                              Home_Team==x[2], Home_Score]
        Mean_Home_Score=mean(Home_Score)
        Mean_Away_Score=mean(Away_Score)
        poisson_sample_1=rpois(nsim, lambda=Mean_Home_Score)
        poisson_sample_2=rpois(nsim, lambda=Mean_Away_Score)
      }
    }else if(length(Home_Score)!=0 & length(Away_Score)==0){
      if(length(Home_Score)==0 & length(Away_Score)==0){
        Away_Score=Data_set[Date<x[1] &
                              Away_Team==x[3], Away_Score]
        Mean_Home_Score=mean(Home_Score)
        Mean_Away_Score=mean(Away_Score)
        poisson_sample_1=rpois(nsim, lambda=Mean_Home_Score)
        poisson_sample_2=rpois(nsim, lambda=Mean_Away_Score)
      }
    }else{
      Mean_Home_Score=mean(Home_Score)
      Mean_Away_Score=mean(Away_Score)
      poisson_sample_1=rpois(nsim, lambda=Mean_Home_Score)
      poisson_sample_2=rpois(nsim, lambda=Mean_Away_Score)
    }
    
    return(
      c(sum(poisson_sample_1==poisson_sample_2)/nsim,
        sum(poisson_sample_1>poisson_sample_2)/nsim,
        sum(poisson_sample_1<poisson_sample_2)/nsim)
    )
  }else{
    return(
      c(1/3, 1/3, 1/3)
    )
  }
  
}

# function 2
Kelly_Criterion=function(p, b){p-(1-p)/(b-1)}


#*************************
#
# compute all combinations
#
#*************************
nsim=10000

Unique_Betting_Dates=unique(Data_set[, Date])
Bet_from_Data_set=c()
Run_Algorithm=0
Betting_Date=min(Data_set[, Date])
#Betting_Date=as.Date("2020-02-09")
while(Run_Algorithm==0){
  
  # target test data
  Target_Data_set=Data_set[Date>=Betting_Date &
                             Date<=Betting_Date+3, ]
  
  if(nrow(Target_Data_set)>1){
    # compute impirical probability (1-3 row : H_p, D_p, and A_p)
    # Empirical_Probability=data.table(t(Target_Data_set)) %>% 
    #   sapply(function(x) Empirical_Probability_Calculator(x, nsim=nsim))
    # Target_Data_set[,
    #                      `:=`(
    #                        H_p=Empirical_Probability[1, ],
    #                        D_p=Empirical_Probability[2, ],
    #                        A_p=Empirical_Probability[3, ]
    #                      )]
    Target_Data_set[,
                    `:=`(
                      H_p=1/`Home_Odds`,
                      D_p=1/`Draw_Odds`,
                      A_p=1/`Away_Odds`
                    )]
    
    Target_Data_set_Comb=combn(c(1:nrow(Target_Data_set)), 2)
    Bet_from_Target_Data_set_Temp_1=c()
    
    for(i in 1:ncol(Target_Data_set_Comb)){
      #i=10
      Pick_1=Target_Data_set_Comb[1, i]
      Pick_2=Target_Data_set_Comb[2, i]
      Odds_Comb=t(as.matrix(Target_Data_set[Pick_1,
                                            .SD,
                                            .SDcols=c("Home_Odds", "Draw_Odds", "Away_Odds")]))%*%
        as.matrix(Target_Data_set[Pick_2,
                                  .SD,
                                  .SDcols=c("Home_Odds", "Draw_Odds", "Away_Odds")])
      
      Prob_Comb=t(as.matrix(Target_Data_set[Pick_1,
                                            .SD,
                                            .SDcols=c("H_p", "D_p", "A_p")]))%*%
        as.matrix(Target_Data_set[Pick_2,
                                  .SD,
                                  .SDcols=c("H_p", "D_p", "A_p")])
      
      Kellies_Estimated=Kelly_Criterion(p=Prob_Comb, b=Odds_Comb)
      
      
      # 
      #Positive_Kellies_Ind=which(Kellies_Estimated>0, arr.ind=T)
      #Positive_Kellies_Ind=which(Kellies_Estimated>=-Inf, arr.ind=T)
      #Positive_Kellies_Ind=which(Odds_Comb<=2.5, arr.ind=T)
      Positive_Kellies_Ind=which(Prob_Comb<=1, arr.ind=T) # basically choose every combination
      
      Bet_from_Target_Data_set_Temp_2=c()
      
      if(length(Positive_Kellies_Ind)==0){
        
        if(Betting_Date==max(Data_set[, Date])){
          Run_Algorithm=1
        }
        
        Bet_from_Target_Data_set_Temp_1=c()
        
        # update betting date
        Betting_Date=min(Data_set[Date>Betting_Date, Date])
      }else{
        for(j in 1:nrow(Positive_Kellies_Ind)){
          #j=1
          Pick_1_Pick_Ind=Positive_Kellies_Ind[j, 1]
          Pick_2_Pick_Ind=Positive_Kellies_Ind[j, 2]
          
          Bet_from_Target_Data_set_Temp_2=rbind(Bet_from_Target_Data_set_Temp_2,
                                                data.table(
                                                  Pick_1_Date=Target_Data_set[Pick_1, Date],
                                                  Pick_1_Home_Team=Target_Data_set[Pick_1, Home_Team],
                                                  Pick_1_Away_Team=Target_Data_set[Pick_1, Away_Team],
                                                  Pick_1=ifelse(Pick_1_Pick_Ind==1, "H", ifelse(Pick_1_Pick_Ind==2, "D", "A")),
                                                  Pick_1_Odds=unlist(Target_Data_set[Pick_1,
                                                                                     .SD,
                                                                                     .SDcols=c("Home_Odds", "Draw_Odds", "Away_Odds")])[Pick_1_Pick_Ind],
                                                  Pick_1_Probability=unlist(Target_Data_set[Pick_1,
                                                                                            .SD,
                                                                                            .SDcols=c("H_p", "D_p", "A_p")])[Pick_1_Pick_Ind],
                                                  Pick_1_F_Result=Data_set[Date==Target_Data_set[Pick_1, Date] &
                                                                             Home_Team==Target_Data_set[Pick_1, Home_Team] &
                                                                             Away_Team==Target_Data_set[Pick_1, Away_Team], F_Result],
                                                  
                                                  
                                                  Pick_2_Date=Target_Data_set[Pick_2, Date],
                                                  Pick_2_Home_Team=Target_Data_set[Pick_2, Home_Team],
                                                  Pick_2_Away_Team=Target_Data_set[Pick_2, Away_Team],
                                                  Pick_2=ifelse(Pick_2_Pick_Ind==1, "H", ifelse(Pick_2_Pick_Ind==2, "D", "A")),
                                                  Pick_2_Odds=unlist(Target_Data_set[Pick_2,
                                                                                     .SD,
                                                                                     .SDcols=c("Home_Odds", "Draw_Odds", "Away_Odds")])[Pick_2_Pick_Ind],
                                                  Pick_2_Probability=unlist(Target_Data_set[Pick_2,
                                                                                            .SD,
                                                                                            .SDcols=c("H_p", "D_p", "A_p")])[Pick_2_Pick_Ind],
                                                  
                                                  Pick_2_F_Result=Data_set[Date==Target_Data_set[Pick_2, Date] &
                                                                             Home_Team==Target_Data_set[Pick_2, Home_Team] &
                                                                             Away_Team==Target_Data_set[Pick_2, Away_Team], F_Result],
                                                  
                                                  Odds_Comb=Odds_Comb[Pick_1_Pick_Ind, Pick_2_Pick_Ind],
                                                  Prob_Comb=Prob_Comb[Pick_1_Pick_Ind, Pick_2_Pick_Ind],
                                                  Kelly=Kellies_Estimated[Pick_1_Pick_Ind, Pick_2_Pick_Ind]
                                                ))
          
        }
        Bet_from_Target_Data_set_Temp_1=rbind(Bet_from_Target_Data_set_Temp_1,
                                              Bet_from_Target_Data_set_Temp_2)
        
        # update betting date
        if(Betting_Date==max(Data_set[, Date])){
          Run_Algorithm=1
        }else{
          Betting_Date=Unique_Betting_Dates[min(which(Unique_Betting_Dates>max(Bet_from_Target_Data_set_Temp_1[, Pick_1_Date])))] 
        }
      }
    }
    
  }else{
    
    if(Betting_Date==max(Data_set[, Date])){
      Run_Algorithm=1
    }
    
    Bet_from_Target_Data_set_Temp_1=c()
    
    # update betting date
    Betting_Date=min(Data_set[Date>Betting_Date, Date])
  }
  
  # record
  Bet_from_Data_set=rbind(Bet_from_Data_set,
                          Bet_from_Target_Data_set_Temp_1)
  
  # print process
  print(Betting_Date)
}

# function 3
Profit_Calculator=function(x){
  #x=Output_Table_2[1, ]
  x=as.character(x)
  
  Conditioned_Bet_from_Data_set=Bet_from_Data_set[Game_Year==as.numeric(x[1]) &
                                                    Pick_1==x[2] & 
                                                    Pick_2==x[3] &
                                                    ((as.numeric(x[4])-0.01)<Prob_Comb & Prob_Comb<=as.numeric(x[4])), ]
  return(
    data.table(
      Lower_Prob_C=as.numeric(x[4])-0.01,
      number_of_picks=nrow(Conditioned_Bet_from_Data_set),
      number_of_wins=nrow(Conditioned_Bet_from_Data_set[Win=="Yes", ]),
      expected_net_odds=mean(Conditioned_Bet_from_Data_set[Win=="Yes", Odds_Comb])-
        (1/(nrow(Conditioned_Bet_from_Data_set[Win=="Yes", ])/nrow(Conditioned_Bet_from_Data_set))),
      net_profit_per_fixed_dollar_bet=sum(Conditioned_Bet_from_Data_set[Win=="Yes", Odds_Comb])-nrow(Conditioned_Bet_from_Data_set)
    )
  )
}


#****************************************
#
# Compute the best settings (odds & prob)
#
#****************************************
# game result match
Bet_from_Data_set[, Win:="No"]
Bet_from_Data_set[Pick_1==Pick_1_F_Result &
                    Pick_2==Pick_2_F_Result, Win:="Yes"]

# date
Bet_from_Data_set %<>% 
  left_join(Data_set[, 1:4], by=c("Pick_1_Date"="Date", "Pick_1_Home_Team"="Home_Team", "Pick_1_Away_Team"="Away_Team")) %>% 
  as.data.table

#
Bet_from_Data_set[, Actual_Year:=format(Pick_1_Date, "%Y")]


#
Years=2000:2020
P_1s=c("H", "D", "A")
P_2s=c("H", "D", "A")
#Odds_Cs=seq(1, 30, by=0.1)
Prob_Cs=seq(0, 0.99, by=0.01)

#*********
# Parallel
library(parallel)
Time_parApply=system.time({
  Output_Table=as.data.table(expand.grid(Years, P_1s, P_2s, Prob_Cs))
  colnames(Output_Table)=c("Game_Year", "Pick_1", "Pick_2", "Upper_Prob_C")
  
  cl <- makeCluster(detectCores()) # use 6 threads
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, c("Profit_Calculator", "Bet_from_Data_set"))
  Output_Table=cbind(Output_Table,
                     do.call(rbind.data.frame, parApply(cl, Output_Table, 1, function(x) Profit_Calculator(x))))
  stopCluster(cl)
})

# Time_mclapply=system.time({
#   Output_Table_Test=as.data.table(expand.grid(Years, P_1s, P_2s, Prob_Cs))
#   colnames(Output_Table_Test)=c("Game_Year", "Pick_1", "Pick_2", "Upper_Prob_C")
#   
#   Output_Table_Test=cbind(Output_Table_Test,
#                           do.call(rbind.data.frame, 
#                                   mclapply.hack(data.frame(t(Output_Table_Test)), function(x) Profit_Calculator(x))))
# })
# 
# all.equal( Output_Table, Output_Table_Test )
#
Res_Var="net_profit_per_fixed_dollar_bet"
Pred_Vars=c(
  "Game_Year",
  "Pick_1",
  "Pick_2",
  "Upper_Prob_C",
  "number_of_picks"
)
Data_to_use=Output_Table[number_of_picks>0,
                         .SD,
                         .SDcols=c(Res_Var, Pred_Vars)] %>% as.data.frame
vector.OF.classes.num.fact=ifelse(unlist(lapply(Data_to_use[, Pred_Vars], class))=="integer", "num", "fact")
vector.OF.classes.num.fact[which(Pred_Vars%in%c("Upper_Prob_C", "number_of_picks"))]="num"
levels.of.fact=rep("NA", length(vector.OF.classes.num.fact))
levels.of.fact[which(Pred_Vars%in%c("Pick_1", "Pick_2"))]="D"
Data_to_use=Format_Columns(Data_to_use,
                           Res_Var=Res_Var,
                           Pred_Vars,
                           vector.OF.classes.num.fact,
                           levels.of.fact) %>% as.data.table

Data_to_use[, Upper_Prob_C] %>% min
Data_to_use[, Upper_Prob_C] %>% max
for(Prob in seq(1, 0, by=-0.1)){
  Data_to_use[Upper_Prob_C<=Prob, Prob_C:=Prob]
}
model_fit=lm(net_profit_per_fixed_dollar_bet~Game_Year+Prob_C+number_of_picks,
             data=Data_to_use)

Data_to_use[, Profit:=sum(net_profit_per_fixed_dollar_bet), by=c("Game_Year", "Prob_C", "Pick_1", "Pick_2")]

Data_to_use[, Game_Year:=as.numeric(as.character(Game_Year))]
Data_to_use[, Game_Year:=as.factor(Game_Year)]

ggplot(Data_to_use, aes(x=Prob_C, y=Profit))+
  geom_point(aes(col=Game_Year), size=3)+
  geom_line(aes(col=Game_Year))


# library(rgl)
# for(Y_Ind in 1:length(Years)){
#   #Y_Ind=18
#   unique(which(Output_Matrix[[Y_Ind]]>0, arr.ind=T))
#   
#   Output_Matrix[[Y_Ind]]=as.data.frame(Output_Matrix[[Y_Ind]])
#   Output_Matrix[[Y_Ind]][is.na(Output_Matrix[[Y_Ind]])]=0
#   Output_Matrix[[Y_Ind]][Output_Matrix[[Y_Ind]]==-Inf]=0
#   Output_Matrix[[Y_Ind]][Output_Matrix[[Y_Ind]]==Inf]=0
#   Output_Matrix[[Y_Ind]]=as.matrix(Output_Matrix[[Y_Ind]])
#   
#   #persp(Odds_Cs, Prob_Cs, output[[Y_Ind]])
#   
#   #open3d()
#   persp3d(x = Odds_Cs, y = Prob_Cs,
#           z = Output_Matrix[[Y_Ind]], col= "orange")
# }



#**************
# save and load
#**************
#save.image(paste0(rdata.dir, "2020-02-17.Rdata"))
#load(paste0(rdata.dir, "2020-02-17.Rdata"))

