#********************
#
# empty the workspace
#
#********************
#rm(list=ls())

#*******************
#
# set directory path
#
#*******************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
# CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/"

#***************************
#
# import functions & library
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

#**************
#
# run algorithm
#
#**************
# Country="england"
# Leagues=c("premier-league", "championship") # premier-league, championship, league-one, league-two, national-league
Combined_Over_Under_Score_Odds=c()
for(League in Leagues){
  #League="premier-league"
  #League="championship"
  
  #*******************
  #
  # set directory path
  #
  #*******************
  data.dir=paste0(CODE.dir.2, "Data/Game_results/", Country, "/", League, "/")
  data.dir_2=paste0(CODE.dir.2, "Data/Over_under_score_odds/", Country, "/")
  
  #************
  #
  # import data
  #
  #************
  # 1. Over_Under_Score_Odds
  #*************************
  Over_Under_Score_Odds=fread(paste0(data.dir_2, "/", League, ".csv"))
  Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
  
  #***********************************************************
  # 2. results data to extract historical data for Kelly score
  #***********************************************************
  File_Lists=list.files(data.dir)
  Results_Data=c()
  for(File_to_Open in File_Lists){
    Results_Data=rbind(fread(paste0(data.dir, File_to_Open)), Results_Data)
  }
  
  #******************
  #
  # Data manipulation
  #
  #******************
  Over_Under_Score_Odds_Manipulation="Yes"
  Results_Manipulation="Yes"
  source(paste0(CODE.dir.2, "Data_Manipulation.R"))
  
  # check -> there shouldn't any output
  setdiff(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])), unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])))
  setdiff(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])), unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])))
  unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team]))
  unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))
  
  #****************
  #
  # Compute indices
  #
  #****************
  # Kelly score
  #Kelly_Method="Poisson" # Poisson, Exact_Dist, Negative_Binom
  if(Kelly_Method=="Poisson"){
    Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Poisson(x, Results_Data)))
  }else if(Kelly_Method=="Exact_Dist"){
    Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Exact_Dist(x, Results_Data)))
  }else if(Kelly_Method=="Negative_Binom"){
    Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Negative_Binom(x, Results_Data)))
  }
  
  # Over_Under_Score_Exact_Prob (proportion)
  Over_Under_Score_Exact_Prob=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Exact_Prob(x, Results_Data)))
  
  # Number of games
  Game_Ns=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Game_Numbers(x, Results_Data)))
  
  # Over_Under_Score_Profit
  Over_Under_Categories=c("Over 0.5", "Under 0.5", 
                          "Over 1.5", "Under 1.5", 
                          "Over 2.5", "Under 2.5", 
                          "Over 3.5", "Under 3.5", 
                          "Over 4.5", "Under 4.5", 
                          "Over 5.5", "Under 5.5")
  Over_Under_Score_Profit=cbind(Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
                                Over_Under_Score_Odds[, .SD, .SDcols=Over_Under_Categories]*
                                  Over_Under_Score_Kelly[, .SD, .SDcols=Over_Under_Categories]-
                                  Over_Under_Score_Kelly[, .SD, .SDcols=Over_Under_Categories])
  
  # Over_Under_Score_Var (Variance of distribution of Over_Under_Score_Exact_Prob at each Over_Under_Category)
  Over_Under_Score_Var=Over_Under_Score_Exact_Prob %>% 
    left_join(Game_Ns, by=c("Date", "League", "Home_Team", "Away_Team")) %>% 
    mutate(`Over 0.5 Var`=`Over 0.5`*(1-`Over 0.5`)/N,
           `Under 0.5 Var`=`Over 0.5`*(1-`Over 0.5`)/N,
           
           `Over 1.5 Var`=`Over 1.5`*(1-`Over 1.5`)/N,
           `Under 1.5 Var`=`Under 1.5`*(1-`Under 1.5`)/N,
           
           `Over 2.5 Var`=`Over 2.5`*(1-`Over 2.5`)/N,
           `Under 2.5 Var`=`Under 2.5`*(1-`Under 2.5`)/N,
           
           `Over 3.5 Var`=`Over 3.5`*(1-`Over 3.5`)/N,
           `Under 3.5 Var`=`Under 3.5`*(1-`Under 3.5`)/N,
           
           `Over 4.5 Var`=`Over 4.5`*(1-`Over 4.5`)/N,
           `Under 4.5 Var`=`Under 4.5`*(1-`Under 4.5`)/N,
           
           `Over 5.5 Var`=`Over 5.5`*(1-`Over 5.5`)/N,
           `Under 5.5 Var`=`Under 5.5`*(1-`Under 5.5`)/N) %>% 
    dplyr::select(Date, League, Home_Team, Away_Team, 
                  `Over 0.5 Var`, `Under 0.5 Var`, `Over 1.5 Var`, `Under 1.5 Var`,
                  `Over 2.5 Var`, `Under 2.5 Var`, `Over 3.5 Var`, `Under 3.5 Var`,
                  `Over 4.5 Var`, `Under 4.5 Var`, `Over 5.5 Var`, `Under 5.5 Var`)
  colnames(Over_Under_Score_Var)=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)
  
  # verify values
  Over_Under_Score_Odds
  Over_Under_Score_Kelly
  Over_Under_Score_Profit
  Over_Under_Score_Exact_Prob
  Over_Under_Score_Var
  
  # algorithm
  for(i in 1:nrow(Over_Under_Score_Profit)){
    if(Chosen_Profit_Criteria==1){
      #**************************************************
      # Chosen_Profit Criterion - (1) : by maximum profit
      Chosen_Profit=unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)][which.max(unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)])]
    }else if(Chosen_Profit_Criteria==2){
      #**************************************************
      # Chosen_Profit Criterion - (2) : by minimum profit
      Chosen_Profit=unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)][which.min(unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)])]
    }else if(Chosen_Profit_Criteria==3){
      #***************************************************************************************************************************
      # Chosen_Profit Criterion - (3) : by a larger profit between the lowest option of "Over"s and the highest option of "Under"s
      Temp=unlist(Over_Under_Score_Profit[i, .SD, .SDcols=Over_Under_Categories])[which(Over_Under_Score_Profit[i, .SD, .SDcols=Over_Under_Categories]>0)] # consider only upto Under 5.5
      Over_Selected_Temp=grep("Over", names(Temp))
      Under_Selected_Temp=grep("Under", names(Temp))
      
      if(length(Over_Selected_Temp)+length(Under_Selected_Temp)>0){ # bet only if there is positive expected profit
        if(length(Over_Selected_Temp)>0){
          Over_Selected=c()
          Under_Selected=-100
          Over_Selected=Temp[min(Over_Selected_Temp)] # select the lowest Over options
        }
        if(length(Under_Selected_Temp)>0){
          Under_Selected=c()
          Over_Selected=-100
          Under_Selected=Temp[max(Under_Selected_Temp)] # select the highest Under options
        }
        Chosen_Profit=ifelse(Over_Selected>=Under_Selected, Over_Selected, Under_Selected)
        
      }else{ # if there is no positive expected profit
        Chosen_Profit=NULL
      }
    }
    
    
    #
    if(length(Chosen_Profit)!=0){
      #
      Over_Under_Score_Odds[i, `Chosen_Option`:=names(Chosen_Profit)]
      Over_Under_Score_Odds[i, `Chosen_Odds`:=Over_Under_Score_Odds[i, .SD, .SDcols=names(Chosen_Profit)]]
      #Over_Under_Score_Odds[i, Result:=Over_Under_Score_Result[i, .SD, .SDcols=names(Chosen_Profit)]]
      Over_Under_Score_Odds[i, Chosen_Option_Prob:=Over_Under_Score_Exact_Prob[i, 
                                                                               .SD,
                                                                               .SDcols=Over_Under_Score_Odds[i, Chosen_Option]]]
      Over_Under_Score_Odds[i, N:=Game_Ns[i, .SD, .SDcols=c("N")]]
      Over_Under_Score_Odds[i, Chosen_Option_Std:=sqrt(Over_Under_Score_Var[i,
                                                                            .SD,
                                                                            .SDcols=Over_Under_Score_Odds[i, Chosen_Option]])]
      Over_Under_Score_Odds[i, Kelly:=Over_Under_Score_Kelly[i, .SD, .SDcols=names(Chosen_Profit)]]
    }else{
      #
      Over_Under_Score_Odds[i, `Chosen_Option`:=""]
      Over_Under_Score_Odds[i, `Chosen_Odds`:=as.numeric("")]
      #Over_Under_Score_Odds[i, Result:=""]
      Over_Under_Score_Odds[i, Chosen_Option_Prob:=as.numeric("")]
      Over_Under_Score_Odds[i, N:=as.numeric("")]
      Over_Under_Score_Odds[i, Chosen_Option_Std:=as.numeric("")]
      Over_Under_Score_Odds[i, Kelly:=as.numeric("")]
    }
    
  }
  
  # combine all Over_Under_Score_Oddss
  Combined_Over_Under_Score_Odds=rbind(Combined_Over_Under_Score_Odds, Over_Under_Score_Odds)
}
# export
fwrite(Combined_Over_Under_Score_Odds,
       paste0(CODE.dir.2, "Output/Over_under_score_odds/Combined_Over_Under_Score_Odds.csv"),
       row.names=FALSE, col.names=TRUE)



#**************
# save and load
#**************
#save.image(paste0(CODE.dir.2, "/Rdata/GEE_Analysis.Rdata"))
#load(paste0(CODE.dir.2, "/Rdata/GEE_Analysis.Rdata"))


