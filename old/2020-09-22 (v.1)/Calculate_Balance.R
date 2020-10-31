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

# obtain the results of bettings
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
    
    Over_Under_Score_Odds_Manipulation="No"
    Over_Under_Score_Algorithm="No"
    Results_Manipulation="Yes"
    source(paste0(CODE.dir.2, "Data_Manipulation.R"))
    
    # combine all results data
    Combined_Results_Data=rbind(Combined_Results_Data, Results_Data)
  }
}
Over_Under_Score_Algorithm="Yes"
source(paste0(CODE.dir.2, "Data_Manipulation.R"))

#********
# filters
#********
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[sample(1:nrow(Combined_Over_Under_Score_Odds)), ]
#Combined_Over_Under_Score_Odds[Kelly==1, Kelly:=0.2]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date>="2020-08-01",]
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds<=1.4, ] # nominal odds is better to be larger than a certain threshold
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly!=1, ] # if Kelly==1, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[N>=10, ] # if Kelly==1, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>=0.10, Kelly:=0.10] # if Kelly is too high, no bet! (too risky)
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[, Kelly:=Kelly*0.2] # if Kelly is too high, no bet! (too risky)
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>0.05, ] # 
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Kelly), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Chosen_Odds), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Total_Goal), ] # games that haven't been held yet
Combined_Over_Under_Score_Odds[, Expected_Profit_Ind:=(Chosen_Odds-1)*Kelly] # calculate expected profit
Unique_Dates=sort(unique(Combined_Over_Under_Score_Odds[, Date]))

#
for(Dat_Ind in 1:length(Unique_Dates)){
  #Dat_Ind=1
  Dat=Unique_Dates[Dat_Ind]
  Temp_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date==Dat, ]
  
  #
  setorderv(Temp_Combined_Over_Under_Score_Odds, "Expected_Profit_Ind", -1) # order rows by Kelly
  #setorderv(Temp_Combined_Over_Under_Score_Odds, "Chosen_Odds", -1) # order rows by Chosen_Odds
  #setorderv(Temp_Combined_Over_Under_Score_Odds, "Kelly", -1) # order rows by Chosen_Odds
  
  #
  if(Dat_Ind==1){
    for(i in 1:nrow(Temp_Combined_Over_Under_Score_Odds)){
      if(i==1){
        if(Betting_Amount=="Kelly"){
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Capital*Kelly)]
        }else{ # if betting amount doesn't depend on kelly score
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=Betting_Amount*Kelly]
        }
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        if(Betting_Amount=="Kelly"){
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Capital-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
        }else{ # if betting amount doesn't depend on kelly score
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=Betting_Amount*Kelly]
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
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=Betting_Amount*Kelly]
        }
        Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        # balance from the previous betting date
        Prev_Daily_Balance=unique(Final_Combined_Over_Under_Score_Odds[Date==Unique_Dates[Dat_Ind-1], Daily_Balance])
        if(Betting_Amount=="Kelly"){
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor((Prev_Daily_Balance-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet]))*Kelly)]
        }else{
          Temp_Combined_Over_Under_Score_Odds[i, Bet:=Betting_Amount*Kelly]
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
# remove URL column
Final_Combined_Over_Under_Score_Odds[, URL:=NULL]

