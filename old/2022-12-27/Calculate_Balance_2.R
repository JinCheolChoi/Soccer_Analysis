#********
#
# filters
#
#********
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[sample(1:nrow(Combined_Over_Under_Score_Odds)), ]
#Combined_Over_Under_Score_Odds[Kelly==1, Kelly:=0.2]
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date>="2020-09-26",]
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds<5, ] # nominal odds is better to be larger than a certain threshold
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly!=1, ] # if Kelly==1, no bet! (too risky)
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[N>=1, ] # if Kelly==1, no bet! (too risky)
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>=1, Kelly:=1] # if Kelly is too high, no bet! (too risky)
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds>12, ]
Combined_Over_Under_Score_Odds[, Expected_Profit_Ind:=(Chosen_Odds-1)*Kelly] # calculate expected profit
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Expected_Profit_Ind>0, ] # keep games with positive expected profit
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Expected_Profit_Ind>=0, ] # calculate expected profit
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds<=3, ] # 
#Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Option_Empirical_Prob>=0.8, ] # 
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Option_Std<=0.3, ] #
# Combined_Over_Under_Score_Odds[Kelly>30, ]$Chosen_Odds
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[, Kelly:=Kelly/150] # if Kelly is too high, no bet! (too risky)
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Kelly>0, ]
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!Kelly>150, ] #
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!Kelly<=10, ] #

# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Chosen_Odds>=12, ] #
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!Kelly>1, ] #
# Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[N<=6, ] # 
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Kelly), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Chosen_Odds), ]
Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[!is.na(Total_Goal), ] # games that haven't been held yet

Unique_Dates=sort(unique(Combined_Over_Under_Score_Odds[, Date]))


#******************
#
# calculate balance
#
#******************
Final_Combined_Over_Under_Score_Odds=c()
if(nrow(Combined_Over_Under_Score_Odds)!=0){
  for(Dat_Ind in 1:length(Unique_Dates)){
    #Dat_Ind=1
    Dat=Unique_Dates[Dat_Ind]
    Temp_Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds[Date==Dat, ]
    
    #
    setorderv(Temp_Combined_Over_Under_Score_Odds, "Expected_Profit_Ind", -1) # order rows by Expected_Profit_Ind
    # setorderv(Temp_Combined_Over_Under_Score_Odds, "Chosen_Odds", -1) # order rows by Chosen_Odds
    # setorderv(Temp_Combined_Over_Under_Score_Odds, "Kelly", 1) # order rows by Kelly
    
    #
    
    if(nrow(Temp_Combined_Over_Under_Score_Odds)>0){
      if(Dat_Ind==1){
        for(i in 1:nrow(Temp_Combined_Over_Under_Score_Odds)){
          #i=2
          if(i==1){
            if(Betting_Amount=="Kelly"){
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=max(0, min(floor(Capital*Kelly), Capital))]
            }else{ # if betting amount doesn't depend on kelly score
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
            }
            Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
          }else if(i>1){
            if(Betting_Amount=="Kelly"){
              Prev_Balance=Capital-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet])
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=max(0, min(Prev_Balance, floor((Prev_Balance)*Kelly)))]
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
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=max(0, min(floor(Prev_Daily_Balance*Kelly), Prev_Daily_Balance))]
            }else{
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=floor(Betting_Amount*Kelly)]
            }
            Temp_Combined_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
          }else if(i>1){
            # balance from the previous betting date
            Prev_Daily_Balance=unique(Final_Combined_Over_Under_Score_Odds[Date==Unique_Dates[Dat_Ind-1], Daily_Balance])
            if(Betting_Amount=="Kelly"){
              Prev_Balance=Prev_Daily_Balance-sum(Temp_Combined_Over_Under_Score_Odds[1:(i-1), Bet])
              Temp_Combined_Over_Under_Score_Odds[i, Bet:=max(0, min(Prev_Balance, floor((Prev_Balance)*Kelly)))]
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
                                                               by=c("Date", "Home_Team", "Away_Team")), fill=T)
      }
    }else{
      # blank
    }
    
  }
  
  # if Betting_Amount is given as an amount, the outcome of interest is Cumulative_Profit, not Daily_Balance
  if(Betting_Amount=="Kelly"){
    # empty (do nothing)
  }else{
    Final_Combined_Over_Under_Score_Odds=as.data.table(Final_Combined_Over_Under_Score_Odds)
    Final_Combined_Over_Under_Score_Odds[, Cumulative_Profit:=Daily_Balance-Capital]
    Final_Combined_Over_Under_Score_Odds[, Daily_Balance:=NULL]
  }
}else{
  print("There is no game to bet.")
}
# remove URL column
if(length(Final_Combined_Over_Under_Score_Odds)!=0){
  Final_Combined_Over_Under_Score_Odds[, URL:=NULL]
}



