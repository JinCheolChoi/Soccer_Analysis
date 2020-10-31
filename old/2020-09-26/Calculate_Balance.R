#*******************************
#
# Combined_Over_Under_Score_Odds
#
#*******************************
if(Simulation=="No"){
  # combined Over_Under_Score_Algorithm bettings
  Combined_Over_Under_Score_Odds=c()
  for(Country in Countries){
    for(League in Leagues){
      for(Year in Years){
        #
        if(file.exists(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))){ # if Over_Under_Score_Odds exists (start) ----
          Over_Under_Score_Odds=fread(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
          if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%Y-%m-%d"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
          }else if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%m/%d/%Y"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
          }
          
          Combined_Over_Under_Score_Odds=rbind(Combined_Over_Under_Score_Odds, Over_Under_Score_Odds, fill=T)
        }
        
      }
    }
  }
}else if(Simulation=="Yes"){ # simulation settings (different kelly scores)
  # combined Over_Under_Score_Algorithm bettings
  Combined_Over_Under_Score_Odds=c()
  for(Country in Countries){ # Country (start) ----
    # Country="england"
    for(League in Leagues){
      # League="premier-league"
      #*******************
      #
      # set directory path
      #
      #*******************
      data.dir_1=paste0(data.dir.1, Country, "/", League, "/")
      data.dir_2=paste0(output.dir, Country, "/", League, "/")
      
      for(Year in Years){ # Year (start) ----
        #Year=2021
        #************
        #
        # import data
        #
        #************
        # 1. Over_Under_Score_Odds
        #*************************
        if(file.exists(paste0(data.dir_2, Year, ".csv"))){ # if Over_Under_Score_Odds exists (start) ----
          Over_Under_Score_Odds=fread(paste0(data.dir_2, Year, ".csv"))
          if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%Y-%m-%d"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
          }else if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%m/%d/%Y"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
          }
          
          
          #***********************************************************
          # 2. results data to extract historical data for Kelly score
          #***********************************************************
          File_Lists=list.files(data.dir_1)
          Results_Data=c()
          for(File_to_Open in File_Lists){
            Results_Data=rbind(fread(paste0(data.dir_1, File_to_Open)), Results_Data)
          }
          
          # League column
          Results_Data[, League:=League]
          
          #******************
          #
          # Data manipulation
          #
          #******************
          Over_Under_Score_Odds=Over_Under_Score_Odds_Manipulation(Over_Under_Score_Odds)
          Results_Data=Results_Manipulation(Results_Data)
          
          # check -> there shouldn't any output
          sort(setdiff(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])), unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))))
          sort(setdiff(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])), unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team]))))
          sort(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])))
          sort(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])))
          
          #****************
          #
          # Compute indices
          #
          #****************
          # Kelly score
          #Kelly_Method="Poisson" # Poisson, Exact_Dist, Negative_Binom
          if(Kelly_Method=="Poisson"){
            Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Poisson(x, Results_Data[League==League, ])))
          }else if(Kelly_Method=="Exact_Dist"){
            Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Exact_Dist(x, Results_Data[League==League, ])))
          }else if(Kelly_Method=="Negative_Binom"){
            Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Negative_Binom(x, Results_Data[League==League, ])))
          }
          
          # Over_Under_Score_Exact_Prob (proportion)
          Over_Under_Score_Exact_Prob=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Exact_Prob(x, Results_Data[League==League, ])))
          
          # Number of games
          Game_Ns=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Game_Numbers(x, Results_Data[League==League, ])))
          
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
              Chosen_Profit=unlist(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)]>0)][which.max(unlist(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)]>0)])]
            }else if(Chosen_Profit_Criteria==2){
              #**************************************************
              # Chosen_Profit Criterion - (2) : by minimum profit
              Chosen_Profit=unlist(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)]>0)][which.min(unlist(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols=c(Over_Under_Categories)]>0)])]
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
              Over_Under_Score_Odds[i, `Chosen_Odds`:=as.numeric(Over_Under_Score_Odds[i, .SD, .SDcols=names(Chosen_Profit)])]
              #Over_Under_Score_Odds[i, Result:=Over_Under_Score_Result[i, .SD, .SDcols=names(Chosen_Profit)]]
              Over_Under_Score_Odds[i, Chosen_Option_Empirical_Prob:=as.numeric(Over_Under_Score_Exact_Prob[i, 
                                                                                                            .SD,
                                                                                                            .SDcols=Over_Under_Score_Odds[i, Chosen_Option]])]
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
              Over_Under_Score_Odds[i, Chosen_Option_Empirical_Prob:=as.numeric("")]
              Over_Under_Score_Odds[i, N:=as.numeric("")]
              Over_Under_Score_Odds[i, Chosen_Option_Std:=as.numeric("")]
              Over_Under_Score_Odds[i, Kelly:=as.numeric("")]
            }
            
          }
          
          Combined_Over_Under_Score_Odds=rbind(Combined_Over_Under_Score_Odds, Over_Under_Score_Odds, fill=T)
          
        }else{ # if Over_Under_Score_Odds exists (end) ----
          print(paste0("Country : [", Country, "], League : [", League, "], Year : ", Year, " file doesn't exist."))
        } 
      } # Year (end) ----
      
      
    }
    
  } # Country (end) ----
}


#*******************************
#
# obtain the results of bettings
#
#*******************************
Combined_Results_Data=c()
for(Country in Countries){
  for(League in Leagues){
    #League="premier-league"
    #League="championship"
    
    #*******************
    #
    # set directory path
    #
    #*******************
    data.dir_1=paste0(data.dir.1, Country, "/", League, "/")
    
    #********************
    # import results data
    #********************
    File_Lists=list.files(data.dir_1)
    Results_Data=c()
    for(File_to_Open in File_Lists){
      Results_Data=rbind(fread(paste0(data.dir_1, File_to_Open)), Results_Data)
    }
    
    # League column
    Results_Data[, League:=League]
    Results_Data=Results_Manipulation(Results_Data)
    
    # combine all results data
    Combined_Results_Data=rbind(Combined_Results_Data, Results_Data)
  }
}
Combined_Over_Under_Score_Odds=Over_Under_Score_Algorithm(Combined_Over_Under_Score_Odds, Combined_Results_Data)


#********
#
# filters
#
#********
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[sample(1:nrow(Combined_Over_Under_Score_Odds)), ]
#Combined_Over_Under_Score_Odds[Kelly==1, Kelly:=0.2]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date>="2020-09-26",]
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds<=1.4, ] # nominal odds is better to be larger than a certain threshold
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly!=1, ] # if Kelly==1, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[N>=10, ] # if Kelly==1, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>=0.10, Kelly:=0.10] # if Kelly is too high, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[, Kelly:=Kelly*0.1] # if Kelly is too high, no bet! (too risky)

#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds<=4, ] # 
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Option_Empirical_Prob>=0.6, ] # 
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Option_Std<=0.3, ] #
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>=0.05, ] #
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[N<=6, ] # 


Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Kelly), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Chosen_Odds), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Total_Goal), ] # games that haven't been held yet

Combined_Over_Under_Score_Odds[, Expected_Profit_Ind:=(Chosen_Odds-1)*Kelly] # calculate expected profit
Unique_Dates=sort(unique(Combined_Over_Under_Score_Odds[, Date]))

#******************
#
# calculate balance
#
#******************
if(nrow(Combined_Over_Under_Score_Odds)!=0){
  for(Dat_Ind in 1:length(Unique_Dates)){
    #Dat_Ind=2
    Dat=Unique_Dates[Dat_Ind]
    Temp_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date==Dat, ]
    
    #
    setorderv(Temp_Combined_Over_Under_Score_Odds, "Expected_Profit_Ind", -1) # order rows by Kelly
    #setorderv(Temp_Combined_Over_Under_Score_Odds, "Chosen_Odds", -1) # order rows by Chosen_Odds
    #setorderv(Temp_Combined_Over_Under_Score_Odds, "Kelly", -1) # order rows by Chosen_Odds
    
    #
    if(Dat_Ind==1){
      for(i in 1:nrow(Temp_Combined_Over_Under_Score_Odds)){
        #i=1
        if(i==1){
          if(Betting_Amount=="Kelly"){
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Capital*Kelly)]
          }else{ # if betting amount doesn't depend on kelly score
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
          }
          Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
        }else if(i>1){
          if(Betting_Amount=="Kelly"){
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Capital-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
          }else{ # if betting amount doesn't depend on kelly score
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
          }
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
          if(Betting_Amount=="Kelly"){
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Prev_Daily_Balance*Kelly)]
          }else{
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
          }
          Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
        }else if(i>1){
          # balance from the previous betting date
          Prev_Daily_Balance=unique(Final_Combined_Over_Under_Score_Odds[Date==Unique_Dates[Dat_Ind-1], Daily_Balance])
          if(Betting_Amount=="Kelly"){
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Prev_Daily_Balance-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
          }else{
            Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
          }
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
}else{
  print("There is no game to bet.")
}

# if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
if(Betting_Amount=="Kelly"){
  # empty (do nothing)
}else{
  Final_Combined_Over_Under_Score_Odds[, Cumulative_Profit:=Daily_Balance-Capital]
  Final_Combined_Over_Under_Score_Odds[, Daily_Balance:=NULL]
}

# remove URL column
Final_Combined_Over_Under_Score_Odds[, URL:=NULL]

