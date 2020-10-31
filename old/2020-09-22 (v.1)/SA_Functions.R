#***************
#
# Temp Functions
#
#***************
Kelly_Criterion=function(p, b){p-(1-p)/(b-1)}


Kelly_Criterion_Calculator_Negative_Binom=function(x, Results_Data){
  
  Mean=Results_Data[Home_Team==x["Home_Team"]&
                      Away_Team==x["Away_Team"]&
                      Exact_Date<=x["Date"], Total_Goal] %>% mean # Date is important to take into account only data prior to the game
  Var=Results_Data[Home_Team==x["Home_Team"]&
                     Away_Team==x["Away_Team"]&
                     Exact_Date<=x["Date"], Total_Goal] %>% var
  
  # Mean=Results_Data[, Total_Goal] %>% mean
  # Var=Results_Data[, Total_Goal] %>% var
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=Kelly_Criterion((1-pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 0.5"])),
      `Under 0.5`=Kelly_Criterion((pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 0.5"])),
      
      `Over 1.5`=Kelly_Criterion((1-pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 1.5"])),
      `Under 1.5`=Kelly_Criterion((pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 1.5"])),
      
      `Over 2.5`=Kelly_Criterion((1-pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 2.5"])),
      `Under 2.5`=Kelly_Criterion((pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 2.5"])),
      
      `Over 3.5`=Kelly_Criterion((1-pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 3.5"])),
      `Under 3.5`=Kelly_Criterion((pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 3.5"])),
      
      `Over 4.5`=Kelly_Criterion((1-pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 4.5"])),
      `Under 4.5`=Kelly_Criterion((pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 4.5"])),
      
      `Over 5.5`=Kelly_Criterion((1-pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 5.5"])),
      `Under 5.5`= Kelly_Criterion((pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 5.5"]))
    )
  )
}


Exact_Prob=function(x, Results_Data){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<0.5)), # Date is important to take into account only data prior to the game
      `Under 0.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<0.5),
      
      `Over 1.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<1.5)),
      `Under 1.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<1.5),
      
      `Over 2.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<2.5)),
      `Under 2.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<2.5),
      
      `Over 3.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<3.5)),
      `Under 3.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<3.5),
      
      `Over 4.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<4.5)),
      `Under 4.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<4.5),
      
      `Over 5.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<=x["Date"], Total_Goal]<5.5)),
      `Under 5.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<=x["Date"], Total_Goal]<5.5)
      
      # `Over 0.5`=(1-mean(Results_Data[, Total_Goal]<0.5)),
      # `Under 0.5`=mean(Results_Data[, Total_Goal]<0.5),
      # 
      # `Over 1.5`=(1-mean(Results_Data[, Total_Goal]<1.5)),
      # `Under 1.5`=mean(Results_Data[, Total_Goal]<1.5),
      # 
      # `Over 2.5`=(1-mean(Results_Data[, Total_Goal]<2.5)),
      # `Under 2.5`=mean(Results_Data[, Total_Goal]<2.5),
      # 
      # `Over 3.5`=(1-mean(Results_Data[, Total_Goal]<3.5)),
      # `Under 3.5`=mean(Results_Data[, Total_Goal]<3.5),
      # 
      # `Over 4.5`=(1-mean(Results_Data[, Total_Goal]<4.5)),
      # `Under 4.5`=mean(Results_Data[, Total_Goal]<4.5),
      # 
      # `Over 5.5`=(1-mean(Results_Data[, Total_Goal]<5.5)),
      # `Under 5.5`=mean(Results_Data[, Total_Goal]<5.5)
    )
  )
}

Game_Numbers=function(x, Results_Data){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      N=length(Results_Data[Home_Team==x["Home_Team"]&
                              Away_Team==x["Away_Team"]&
                              Exact_Date<=x["Date"], Total_Goal])
      
      #N=length(Results_Data[, Total_Goal])
    )
  )
}

Kelly_Criterion_Calculator_Exact_Dist=function(x, Results_Data){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                                        Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<0.5)), as.numeric(x["Over 0.5"])), # Date is important to take into account only data prior to the game
      `Under 0.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&
                                                      Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<0.5), as.numeric(x["Under 0.5"])),
      
      `Over 1.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<1.5)), as.numeric(x["Over 1.5"])),
      `Under 1.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<1.5), as.numeric(x["Under 1.5"])),
      
      `Over 2.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                                        Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<2.5)), as.numeric(x["Over 2.5"])),
      `Under 2.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&
                                                      Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<2.5), as.numeric(x["Under 2.5"])),
      
      `Over 3.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                                        Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<3.5)), as.numeric(x["Over 3.5"])),
      `Under 3.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&
                                                      Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<3.5), as.numeric(x["Under 3.5"])),
      
      `Over 4.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                                        Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<4.5)), as.numeric(x["Over 4.5"])),
      `Under 4.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&
                                                      Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<4.5), as.numeric(x["Under 4.5"])),
      
      `Over 5.5`=Kelly_Criterion((1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                                        Away_Team==x["Away_Team"]&
                                                        Exact_Date<=x["Date"], Total_Goal]<5.5)), as.numeric(x["Over 5.5"])),
      `Under 5.5`=Kelly_Criterion(mean(Results_Data[Home_Team==x["Home_Team"]&
                                                      Away_Team==x["Away_Team"]&
                                                      Exact_Date<=x["Date"], Total_Goal]<5.5), as.numeric(x["Under 5.5"]))
      
      # `Over 0.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<0.5)), as.numeric(x["Over 0.5"])),
      # `Under 0.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<0.5), as.numeric(x["Under 0.5"])),
      # 
      # `Over 1.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<1.5)), as.numeric(x["Over 1.5"])),
      # `Under 1.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<1.5), as.numeric(x["Under 1.5"])),
      # 
      # `Over 2.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<2.5)), as.numeric(x["Over 2.5"])),
      # `Under 2.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<2.5), as.numeric(x["Under 2.5"])),
      # 
      # `Over 3.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<3.5)), as.numeric(x["Over 3.5"])),
      # `Under 3.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<3.5), as.numeric(x["Under 3.5"])),
      # 
      # `Over 4.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<4.5)), as.numeric(x["Over 4.5"])),
      # `Under 4.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<4.5), as.numeric(x["Under 4.5"])),
      # 
      # `Over 5.5`=Kelly_Criterion((1-mean(Results_Data[, Total_Goal]<5.5)), as.numeric(x["Over 5.5"])),
      # `Under 5.5`=Kelly_Criterion(mean(Results_Data[, Total_Goal]<5.5), as.numeric(x["Under 5.5"]))
    )
  )
}

Kelly_Criterion_Calculator_Poisson=function(x, Results_Data){
  
  Mean=Results_Data[Home_Team==x["Home_Team"]&
                      Away_Team==x["Away_Team"]&
                      Exact_Date<=x["Date"], Total_Goal] %>% mean # Date is important to take into account only data prior to the game
  Var=Results_Data[Home_Team==x["Home_Team"]&
                     Away_Team==x["Away_Team"]&
                     Exact_Date<=x["Date"], Total_Goal] %>% var
  
  # Mean=Results_Data[, Total_Goal] %>% mean
  # Var=Results_Data[, Total_Goal] %>% var
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=Kelly_Criterion((1-ppois(0.5, lambda=Mean)), as.numeric(x["Over 0.5"])),
      `Under 0.5`=Kelly_Criterion((ppois(0.5, lambda=Mean)), as.numeric(x["Under 0.5"])),
      
      `Over 1.5`=Kelly_Criterion((1-ppois(1.5, lambda=Mean)), as.numeric(x["Over 1.5"])),
      `Under 1.5`=Kelly_Criterion((ppois(1.5, lambda=Mean)), as.numeric(x["Under 1.5"])),
      
      `Over 2.5`=Kelly_Criterion((1-ppois(2.5, lambda=Mean)), as.numeric(x["Over 2.5"])),
      `Under 2.5`=Kelly_Criterion((ppois(2.5, lambda=Mean)), as.numeric(x["Under 2.5"])),
      
      `Over 3.5`=Kelly_Criterion((1-ppois(3.5, lambda=Mean)), as.numeric(x["Over 3.5"])),
      `Under 3.5`=Kelly_Criterion((ppois(3.5, lambda=Mean)), as.numeric(x["Under 3.5"])),
      
      `Over 4.5`=Kelly_Criterion((1-ppois(4.5, lambda=Mean)), as.numeric(x["Over 4.5"])),
      `Under 4.5`=Kelly_Criterion((ppois(4.5, lambda=Mean)), as.numeric(x["Under 4.5"])),
      
      `Over 5.5`=Kelly_Criterion((1-ppois(5.5, lambda=Mean)), as.numeric(x["Over 5.5"])),
      `Under 5.5`=Kelly_Criterion((ppois(5.5, lambda=Mean)), as.numeric(x["Under 5.5"]))
    )
  )
}
