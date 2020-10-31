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


#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
Country="england"
Leagues=c("premier-league", "championship") # premier-league, championship, league-one, league-two, national-league
Results_Manipulation=
Over_Under_Score_Odds_Manipulation=
Over_Under_Score_Algorithm=""

Kelly_Method="Poisson" # Poisson, Exact_Dist, Negative_Binom
Chosen_Profit_Criteria=3     # 1 (by maximum profit), 2 (by minimum profit), 3 (by a larger profit between the lowest option of "Over"s and the highest option of "Under"s)
source(paste0(CODE.dir.2, "Over_Under_Score_Algorithm.R"))


#*******************************************
#
# Calculate balance based on chosen bettings
#
#*******************************************
Capital=10000

# Over_Under_Score_Algorithm bettings
Combined_Over_Under_Score_Odds=fread(paste0("C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Output/Over_under_score_odds/", "Combined_Over_Under_Score_Odds.csv"))
Combined_Over_Under_Score_Odds[, Date:=as.Date(Date)]

# obtain the results of bettings
Combined_Results_Data=c()
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
  
  #********************
  # import results data
  #********************
  File_Lists=list.files(data.dir)
  Results_Data=c()
  for(File_to_Open in File_Lists){
    Results_Data=rbind(fread(paste0(data.dir, File_to_Open)), Results_Data)
  }
  Results_Manipulation="Yes"
  source(paste0(CODE.dir.2, "Data_Manipulation.R"))
  
  # combine all results data
  Combined_Results_Data=rbind(Combined_Results_Data, Results_Data)
}
Over_Under_Score_Algorithm="Yes"
source(paste0(CODE.dir.2, "Data_Manipulation.R"))


#********
# filters
#********
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[sample(1:nrow(Combined_Over_Under_Score_Odds)), ]
#Combined_Over_Under_Score_Odds[Kelly==1, Kelly:=0.2]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date>="2020-05-01",]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds>=1.1, ] # nominal odds is better to be larger than a certain threshold
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly!=1, ] # if Kelly==1, no bet! (too risky)
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Kelly), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Chosen_Odds), ]
Combined_Over_Under_Score_Odds[, Expected_Profit_Ind:=(Chosen_Odds-1)*Kelly] # calculate expected profit
Unique_Dates=sort(unique(Combined_Over_Under_Score_Odds[, Date]))

#
for(Dat_Ind in 1:length(Unique_Dates)){
  #Dat_Ind=2
  Dat=Unique_Dates[Dat_Ind]
  Temp_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date==Dat, ]
  
  #
  setorderv(Temp_Combined_Over_Under_Score_Odds, "Expected_Profit_Ind", -1) # order rows by Kelly
  #setorderv(Temp_Combined_Over_Under_Score_Odds, "Chosen_Odds", -1) # order rows by Chosen_Odds
  
  #
  if(Dat_Ind==1){
    for(i in 1:nrow(Temp_Combined_Over_Under_Score_Odds)){
      if(i==1){
        Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Capital*Kelly)]
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Capital-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }
    }
    Total_Profit=sum(Temp_Combined_Over_Under_Score_Odds[, Profit]) # profit
    Temp_Combined_Over_Under_Score_Odds[, Daily_Balance:=Capital+Total_Profit] # balance
    
    # Final_Combined_Over_Under_Score_Odds
    Final_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date==Dat, ] %>% 
      left_join(Temp_Combined_Over_Under_Score_Odds[,
                                                    .SD,
                                                    .SDcols=c("Date", "Home_Team", "Away_Team", "Bet", "Profit", "Daily_Balance")],
                by=c("Date", "Home_Team", "Away_Team"))
  }else{ # if Data!=min(Unique_Dates)
    for(i in 1:nrow(Temp_Combined_Over_Under_Score_Odds)){
      if(i==1){
        # balance from the previous betting date
        Prev_Daily_Balance=unique(Final_Combined_Over_Under_Score_Odds[Date==Unique_Dates[Dat_Ind-1], Daily_Balance])
        Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Prev_Daily_Balance*Kelly)]
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        # balance from the previous betting date
        Prev_Daily_Balance=unique(Final_Combined_Over_Under_Score_Odds[Date==Unique_Dates[Dat_Ind-1], Daily_Balance])
        Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Prev_Daily_Balance-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }
    }
    Total_Profit=sum(Temp_Combined_Over_Under_Score_Odds[, Profit]) # profit
    Temp_Combined_Over_Under_Score_Odds[, Daily_Balance:=Prev_Daily_Balance+Total_Profit] # balance
    
    # Final_Combined_Over_Under_Score_Odds
    Final_Combined_Over_Under_Score_Odds=rbind(Final_Combined_Over_Under_Score_Odds, Combined_Over_Under_Score_Odds[Date==Dat, ] %>% 
                                                 left_join(Temp_Combined_Over_Under_Score_Odds[,
                                                                                               .SD,
                                                                                               .SDcols=c("Date", "Home_Team", "Away_Team", "Bet", "Profit", "Daily_Balance")],
                                                           by=c("Date", "Home_Team", "Away_Team")))
  }
}
Final_Combined_Over_Under_Score_Odds




