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



#***********************************
#
# Over_Under_Score_Odds_Manipulation
#
#***********************************
Over_Under_Score_Odds_Manipulation=function(Over_Under_Score_Odds){
  #********************
  # premier-league ----
  #********************
  Over_Under_Score_Odds[Home_Team=="Arsenal FC", Home_Team:="Arsenal"]
  Over_Under_Score_Odds[Away_Team=="Arsenal FC", Away_Team:="Arsenal"]
  
  Over_Under_Score_Odds[Home_Team=="Aston Villa FC", Home_Team:="Aston Villa"]
  Over_Under_Score_Odds[Away_Team=="Aston Villa FC", Away_Team:="Aston Villa"]
  
  Over_Under_Score_Odds[Home_Team=="Brighton & Hove Albion FC", Home_Team:="Brighton"]
  Over_Under_Score_Odds[Away_Team=="Brighton & Hove Albion FC", Away_Team:="Brighton"]
  
  
  Over_Under_Score_Odds[Home_Team=="Burnley FC", Home_Team:="Burnley"]
  Over_Under_Score_Odds[Away_Team=="Burnley FC", Away_Team:="Burnley"]
  
  Over_Under_Score_Odds[Home_Team=="Chelsea FC", Home_Team:="Chelsea"]
  Over_Under_Score_Odds[Away_Team=="Chelsea FC", Away_Team:="Chelsea"]
  
  Over_Under_Score_Odds[Home_Team=="Crystal Palace FC", Home_Team:="Crystal Palace"]
  Over_Under_Score_Odds[Away_Team=="Crystal Palace FC", Away_Team:="Crystal Palace"]
  
  Over_Under_Score_Odds[Home_Team=="Everton FC", Home_Team:="Everton"]
  Over_Under_Score_Odds[Away_Team=="Everton FC", Away_Team:="Everton"]
  
  Over_Under_Score_Odds[Home_Team=="Fulham FC", Home_Team:="Fulham"]
  Over_Under_Score_Odds[Away_Team=="Fulham FC", Away_Team:="Fulham"]
  
  Over_Under_Score_Odds[Home_Team=="Leeds United", Home_Team:="Leeds"]
  Over_Under_Score_Odds[Away_Team=="Leeds United", Away_Team:="Leeds"]
  
  Over_Under_Score_Odds[Home_Team=="Leicester City FC", Home_Team:="Leicester"]
  Over_Under_Score_Odds[Away_Team=="Leicester City FC", Away_Team:="Leicester"]
  
  Over_Under_Score_Odds[Home_Team=="Liverpool FC", Home_Team:="Liverpool"]
  Over_Under_Score_Odds[Away_Team=="Liverpool FC", Away_Team:="Liverpool"]
  
  Over_Under_Score_Odds[Home_Team=="Man City", Home_Team:="Manchester City"]
  Over_Under_Score_Odds[Away_Team=="Man City", Away_Team:="Manchester City"]
  Over_Under_Score_Odds[Home_Team=="Man Utd", Home_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Away_Team=="Man Utd", Away_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Home_Team=="Manchester City FC", Home_Team:="Manchester City"]
  Over_Under_Score_Odds[Away_Team=="Manchester City FC", Away_Team:="Manchester City"]
  
  Over_Under_Score_Odds[Home_Team=="Manchester United FC", Home_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Away_Team=="Manchester United FC", Away_Team:="Manchester Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Newcastle United FC", Home_Team:="Newcastle"]
  Over_Under_Score_Odds[Away_Team=="Newcastle United FC", Away_Team:="Newcastle"]
  
  Over_Under_Score_Odds[Home_Team=="Nottm Forest", Home_Team:="Nottingham"]
  Over_Under_Score_Odds[Away_Team=="Nottm Forest", Away_Team:="Nottingham"]
  
  Over_Under_Score_Odds[Home_Team=="Sheff Utd", Home_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Away_Team=="Sheff Utd", Away_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Home_Team=="Sheffield United FC", Home_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Away_Team=="Sheffield United FC", Away_Team:="Sheffield Utd"]
  
  
  Over_Under_Score_Odds[Home_Team=="Sheff Wed", Home_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Away_Team=="Sheff Wed", Away_Team:="Sheffield Wed"]
  
  Over_Under_Score_Odds[Home_Team=="Southampton FC", Home_Team:="Southampton"]
  Over_Under_Score_Odds[Away_Team=="Southampton FC", Away_Team:="Southampton"]
  
  Over_Under_Score_Odds[Home_Team=="Tottenham Hotspur FC", Home_Team:="Tottenham"]
  Over_Under_Score_Odds[Away_Team=="Tottenham Hotspur FC", Away_Team:="Tottenham"]
  
  Over_Under_Score_Odds[Home_Team=="West Bromwich Albion", Home_Team:="West Brom"]
  Over_Under_Score_Odds[Away_Team=="West Bromwich Albion", Away_Team:="West Brom"]
  
  Over_Under_Score_Odds[Home_Team=="West Ham United FC", Home_Team:="West Ham"]
  Over_Under_Score_Odds[Away_Team=="West Ham United FC", Away_Team:="West Ham"]
  
  Over_Under_Score_Odds[Home_Team=="Wolverhampton Wanderers F", Home_Team:="Wolves"]
  Over_Under_Score_Odds[Away_Team=="Wolverhampton Wanderers F", Away_Team:="Wolves"]
  Over_Under_Score_Odds[Home_Team=="Wolverhampton Wanderers FC", Home_Team:="Wolves"]
  Over_Under_Score_Odds[Away_Team=="Wolverhampton Wanderers FC", Away_Team:="Wolves"]
  Over_Under_Score_Odds[Home_Team=="Wolverhampton Wanderers", Home_Team:="Wolves"]
  Over_Under_Score_Odds[Away_Team=="Wolverhampton Wanderers", Away_Team:="Wolves"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #******************
  # championship ----
  #******************
  Over_Under_Score_Odds[Home_Team=="AFC Bournemouth", Home_Team:="Bournemouth"]
  Over_Under_Score_Odds[Away_Team=="AFC Bournemouth", Away_Team:="Bournemouth"]
  
  Over_Under_Score_Odds[Home_Team=="Birmingham City", Home_Team:="Birmingham"]
  Over_Under_Score_Odds[Away_Team=="Birmingham City", Away_Team:="Birmingham"]
  
  Over_Under_Score_Odds[Home_Team=="Blackburn Rovers", Home_Team:="Blackburn"]
  Over_Under_Score_Odds[Away_Team=="Blackburn Rovers", Away_Team:="Blackburn"]
  
  Over_Under_Score_Odds[Home_Team=="Brentford FC", Home_Team:="Brentford"]
  Over_Under_Score_Odds[Away_Team=="Brentford FC", Away_Team:="Brentford"]
  
  Over_Under_Score_Odds[Home_Team=="Cardiff City", Home_Team:="Cardiff"]
  Over_Under_Score_Odds[Away_Team=="Cardiff City", Away_Team:="Cardiff"]
  
  Over_Under_Score_Odds[Home_Team=="Coventry City", Home_Team:="Coventry"]
  Over_Under_Score_Odds[Away_Team=="Coventry City", Away_Team:="Coventry"]
  
  Over_Under_Score_Odds[Home_Team=="Derby County", Home_Team:="Derby"]
  Over_Under_Score_Odds[Away_Team=="Derby County", Away_Team:="Derby"]
  
  Over_Under_Score_Odds[Home_Team=="Huddersfield Town", Home_Team:="Huddersfield"]
  Over_Under_Score_Odds[Away_Team=="Huddersfield Town", Away_Team:="Huddersfield"]
  
  Over_Under_Score_Odds[Home_Team=="Luton Town", Home_Team:="Luton"]
  Over_Under_Score_Odds[Away_Team=="Luton Town", Away_Team:="Luton"]
  
  Over_Under_Score_Odds[Home_Team=="Middlesbrough FC", Home_Team:="Middlesbrough"]
  Over_Under_Score_Odds[Away_Team=="Middlesbrough FC", Away_Team:="Middlesbrough"]
  
  Over_Under_Score_Odds[Home_Team=="Millwall FC", Home_Team:="Millwall"]
  Over_Under_Score_Odds[Away_Team=="Millwall FC", Away_Team:="Millwall"]
  
  Over_Under_Score_Odds[Home_Team=="Norwich City FC", Home_Team:="Norwich"]
  Over_Under_Score_Odds[Away_Team=="Norwich City FC", Away_Team:="Norwich"]
  
  Over_Under_Score_Odds[Home_Team=="Nottingham Forest", Home_Team:="Nottingham"]
  Over_Under_Score_Odds[Away_Team=="Nottingham Forest", Away_Team:="Nottingham"]
  
  Over_Under_Score_Odds[Home_Team=="Preston North End", Home_Team:="Preston"]
  Over_Under_Score_Odds[Away_Team=="Preston North End", Away_Team:="Preston"]
  
  Over_Under_Score_Odds[Home_Team=="Queens Park Rangers", Home_Team:="QPR"]
  Over_Under_Score_Odds[Away_Team=="Queens Park Rangers", Away_Team:="QPR"]
  
  Over_Under_Score_Odds[Home_Team=="Reading FC", Home_Team:="Reading"]
  Over_Under_Score_Odds[Away_Team=="Reading FC", Away_Team:="Reading"]
  
  Over_Under_Score_Odds[Home_Team=="Rotherham United", Home_Team:="Rotherham"]
  Over_Under_Score_Odds[Away_Team=="Rotherham United", Away_Team:="Rotherham"]
  
  Over_Under_Score_Odds[Home_Team=="Sheffield Wednesday", Home_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Away_Team=="Sheffield Wednesday", Away_Team:="Sheffield Wed"]
  
  Over_Under_Score_Odds[Home_Team=="Stoke City", Home_Team:="Stoke"]
  Over_Under_Score_Odds[Away_Team=="Stoke City", Away_Team:="Stoke"]
  
  Over_Under_Score_Odds[Home_Team=="Swansea City", Home_Team:="Swansea"]
  Over_Under_Score_Odds[Away_Team=="Swansea City", Away_Team:="Swansea"]
  
  Over_Under_Score_Odds[Home_Team=="Watford FC", Home_Team:="Watford"]
  Over_Under_Score_Odds[Away_Team=="Watford FC", Away_Team:="Watford"]
  
  Over_Under_Score_Odds[Home_Team=="Wycombe Wanderers", Home_Team:="Wycombe"]
  Over_Under_Score_Odds[Away_Team=="Wycombe Wanderers", Away_Team:="Wycombe"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #****************
  # league-one ----
  #****************
  Over_Under_Score_Odds[Home_Team=="Accrington Stanley", Home_Team:="Accrington"]
  Over_Under_Score_Odds[Away_Team=="Accrington Stanley", Away_Team:="Accrington"]
  
  Over_Under_Score_Odds[Home_Team=="Burton Albion", Home_Team:="Burton"]
  Over_Under_Score_Odds[Away_Team=="Burton Albion", Away_Team:="Burton"]
  
  Over_Under_Score_Odds[Home_Team=="Charlton Athletic", Home_Team:="Charlton"]
  Over_Under_Score_Odds[Away_Team=="Charlton Athletic", Away_Team:="Charlton"]
  
  Over_Under_Score_Odds[Home_Team=="Crewe Alexandra", Home_Team:="Crewe"]
  Over_Under_Score_Odds[Away_Team=="Crewe Alexandra", Away_Team:="Crewe"]
  
  Over_Under_Score_Odds[Home_Team=="Doncaster Rovers", Home_Team:="Doncaster"]
  Over_Under_Score_Odds[Away_Team=="Doncaster Rovers", Away_Team:="Doncaster"]
  
  Over_Under_Score_Odds[Home_Team=="Fleetwood Town", Home_Team:="Fleetwood"]
  Over_Under_Score_Odds[Away_Team=="Fleetwood Town", Away_Team:="Fleetwood"]
  
  Over_Under_Score_Odds[Home_Team=="Gillingham FC", Home_Team:="Gillingham"]
  Over_Under_Score_Odds[Away_Team=="Gillingham FC", Away_Team:="Gillingham"]
  
  Over_Under_Score_Odds[Home_Team=="Hull City", Home_Team:="Hull"]
  Over_Under_Score_Odds[Away_Team=="Hull City", Away_Team:="Hull"]
  
  Over_Under_Score_Odds[Home_Team=="Ipswich Town", Home_Team:="Ipswich"]
  Over_Under_Score_Odds[Away_Team=="Ipswich Town", Away_Team:="Ipswich"]
  
  Over_Under_Score_Odds[Home_Team=="Lincoln City", Home_Team:="Lincoln"]
  Over_Under_Score_Odds[Away_Team=="Lincoln City", Away_Team:="Lincoln"]
  
  Over_Under_Score_Odds[Home_Team=="Milton Keynes Dons", Home_Team:="MK Dons"]
  Over_Under_Score_Odds[Away_Team=="Milton Keynes Dons", Away_Team:="MK Dons"]
  
  Over_Under_Score_Odds[Home_Team=="Northampton Town", Home_Team:="Northampton"]
  Over_Under_Score_Odds[Away_Team=="Northampton Town", Away_Team:="Northampton"]
  
  Over_Under_Score_Odds[Home_Team=="Oxford United", Home_Team:="Oxford Utd"]
  Over_Under_Score_Odds[Away_Team=="Oxford United", Away_Team:="Oxford Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Peterborough United", Home_Team:="Peterborough"]
  Over_Under_Score_Odds[Away_Team=="Peterborough United", Away_Team:="Peterborough"]
  
  Over_Under_Score_Odds[Home_Team=="Plymouth Argyle", Home_Team:="Plymouth"]
  Over_Under_Score_Odds[Away_Team=="Plymouth Argyle", Away_Team:="Plymouth"]
  
  Over_Under_Score_Odds[Home_Team=="Portsmouth FC", Home_Team:="Portsmouth"]
  Over_Under_Score_Odds[Away_Team=="Portsmouth FC", Away_Team:="Portsmouth"]
  
  Over_Under_Score_Odds[Home_Team=="Rochdale AFC", Home_Team:="Rochdale"]
  Over_Under_Score_Odds[Away_Team=="Rochdale AFC", Away_Team:="Rochdale"]
  
  Over_Under_Score_Odds[Home_Team=="Shrewsbury Town", Home_Team:="Shrewsbury"]
  Over_Under_Score_Odds[Away_Team=="Shrewsbury Town", Away_Team:="Shrewsbury"]
  
  Over_Under_Score_Odds[Home_Team=="Sunderland AFC", Home_Team:="Sunderland"]
  Over_Under_Score_Odds[Away_Team=="Sunderland AFC", Away_Team:="Sunderland"]
  
  Over_Under_Score_Odds[Home_Team=="Swindon Town", Home_Team:="Swindon"]
  Over_Under_Score_Odds[Away_Team=="Swindon Town", Away_Team:="Swindon"]
  
  Over_Under_Score_Odds[Home_Team=="Wigan Athletic", Home_Team:="Wigan"]
  Over_Under_Score_Odds[Away_Team=="Wigan Athletic", Away_Team:="Wigan"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #****************
  # league-two ----
  #****************
  Over_Under_Score_Odds[Home_Team=="Barrow AFC", Home_Team:="Barrow"]
  Over_Under_Score_Odds[Away_Team=="Barrow AFC", Away_Team:="Barrow"]
  
  Over_Under_Score_Odds[Home_Team=="Bolton Wanderers", Home_Team:="Bolton"]
  Over_Under_Score_Odds[Away_Team=="Bolton Wanderers", Away_Team:="Bolton"]
  
  Over_Under_Score_Odds[Home_Team=="Cambridge United", Home_Team:="Cambridge Utd"]
  Over_Under_Score_Odds[Away_Team=="Cambridge United", Away_Team:="Cambridge Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Carlisle United", Home_Team:="Carlisle"]
  Over_Under_Score_Odds[Away_Team=="Carlisle United", Away_Team:="Carlisle"]
  
  Over_Under_Score_Odds[Home_Team=="Cheltenham Town", Home_Team:="Cheltenham"]
  Over_Under_Score_Odds[Away_Team=="Cheltenham Town", Away_Team:="Cheltenham"]
  
  Over_Under_Score_Odds[Home_Team=="Colchester United", Home_Team:="Colchester"]
  Over_Under_Score_Odds[Away_Team=="Colchester United", Away_Team:="Colchester"]
  
  Over_Under_Score_Odds[Home_Team=="Crawley Town", Home_Team:="Crawley"]
  Over_Under_Score_Odds[Away_Team=="Crawley Town", Away_Team:="Crawley"]
  
  Over_Under_Score_Odds[Home_Team=="Exeter City", Home_Team:="Exeter"]
  Over_Under_Score_Odds[Away_Team=="Exeter City", Away_Team:="Exeter"]
  
  Over_Under_Score_Odds[Home_Team=="Forest Green Rovers", Home_Team:="Forest Green"]
  Over_Under_Score_Odds[Away_Team=="Forest Green Rovers", Away_Team:="Forest Green"]
  
  Over_Under_Score_Odds[Home_Team=="Grimsby Town", Home_Team:="Grimsby"]
  Over_Under_Score_Odds[Away_Team=="Grimsby Town", Away_Team:="Grimsby"]
  
  Over_Under_Score_Odds[Home_Team=="Harrogate Town FC", Home_Team:="Harrogate"]
  Over_Under_Score_Odds[Away_Team=="Harrogate Town FC", Away_Team:="Harrogate"]
  
  Over_Under_Score_Odds[Home_Team=="Mansfield Town", Home_Team:="Mansfield"]
  Over_Under_Score_Odds[Away_Team=="Mansfield Town", Away_Team:="Mansfield"]
  
  Over_Under_Score_Odds[Home_Team=="Morecambe FC", Home_Team:="Morecambe"]
  Over_Under_Score_Odds[Away_Team=="Morecambe FC", Away_Team:="Morecambe"]
  
  Over_Under_Score_Odds[Home_Team=="Newport County AFC", Home_Team:="Newport"]
  Over_Under_Score_Odds[Away_Team=="Newport County AFC", Away_Team:="Newport"]
  
  Over_Under_Score_Odds[Home_Team=="Oldham Athletic", Home_Team:="Oldham"]
  Over_Under_Score_Odds[Away_Team=="Oldham Athletic", Away_Team:="Oldham"]
  
  Over_Under_Score_Odds[Home_Team=="Salford City FC", Home_Team:="Salford"]
  Over_Under_Score_Odds[Away_Team=="Salford City FC", Away_Team:="Salford"]
  
  Over_Under_Score_Odds[Home_Team=="Scunthorpe United", Home_Team:="Scunthorpe"]
  Over_Under_Score_Odds[Away_Team=="Scunthorpe United", Away_Team:="Scunthorpe"]
  
  Over_Under_Score_Odds[Home_Team=="Southend United", Home_Team:="Southend"]
  Over_Under_Score_Odds[Away_Team=="Southend United", Away_Team:="Southend"]
  
  Over_Under_Score_Odds[Home_Team=="Stevenage FC", Home_Team:="Stevenage"]
  Over_Under_Score_Odds[Away_Team=="Stevenage FC", Away_Team:="Stevenage"]
  
  Over_Under_Score_Odds[Home_Team=="Tranmere Rovers", Home_Team:="Tranmere"]
  Over_Under_Score_Odds[Away_Team=="Tranmere Rovers", Away_Team:="Tranmere"]
  
  Over_Under_Score_Odds[Home_Team=="Walsall FC", Home_Team:="Walsall"]
  Over_Under_Score_Odds[Away_Team=="Walsall FC", Away_Team:="Walsall"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  return(Over_Under_Score_Odds)
}


#*********************
#
# Results_Manipulation
#
#*********************
Results_Manipulation=function(Results_Data){
  # Date
  Results_Data[, Exact_Date:=as.Date(Date, format="%d.%m.%Y")]
  Results_Data[is.na(Exact_Date), Exact_Date:=as.Date(paste0(Date, year(Sys.Date())), format="%d.%m.%Y")]
  Results_Data[Date=="Today", Exact_Date:=as.Date(format(Sys.Date(),"%Y-%m-%d"))]
  Results_Data[Date=="Yesterday", Exact_Date:=as.Date(format(Sys.Date()-1,"%Y-%m-%d"))]
  
  # Total_Goal
  Results_Data[, Home_Goal:=as.numeric(Home_Goal)]
  Results_Data[, Away_Goal:=as.numeric(Away_Goal)]
  
  Results_Data[, Total_Goal:=Home_Goal+Away_Goal]
  Results_Data[, Total_Goal] %>% mean
  Results_Data[, Total_Goal] %>% var
  
  return(Results_Data)
}


#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
Over_Under_Score_Algorithm=function(Combined_Over_Under_Score_Odds, Combined_Results_Data){
  # total goal
  Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds %>% 
    left_join(Combined_Results_Data, by=c("Date"="Exact_Date", "League", "Home_Team", "Away_Team")) %>% 
    dplyr::select(-Date.y) %>% 
    as.data.table
  
  #
  Combined_Over_Under_Score_Odds[, Result:=0]
  
  Combined_Over_Under_Score_Odds[Total_Goal<readr::parse_number(Chosen_Option)& # if the total goal is under the number of goals of the "Under" category chosen, result=1
                                   sub("^([[:alpha:]]*).*", "\\1", Chosen_Option)=="Under", Result:=1]
  Combined_Over_Under_Score_Odds[Total_Goal>readr::parse_number(Chosen_Option)& # if the total goal is over the number of goals of the "Over" category chosen, result=1
                                   sub("^([[:alpha:]]*).*", "\\1", Chosen_Option)=="Over", Result:=1]
  
  return(Combined_Over_Under_Score_Odds)
  
}


#********************
#
# Extract_Total_Goals
#
#********************
Total_Goals_Kelly_Calculator=function(Game_URL, Country, League, ...){
  # game URL
  #Game_URL=Over_under_score_odds[Game_Ind, URL]
  
  #*****************************
  # 1. Over_Under_Score_Odds
  #*********
  # headings
  Target_Options_N=0
  print(paste0("Country : [", Country, "], League : [", League, "], Target_Options_N"))
  while(Target_Options_N==0){
    Headings=c()
    print(paste0("Country : [", Country, "], League : [", League, "], Headings"))
    while(length(Headings)==0){
      print(paste0("Country : [", Country, "], League : [", League, "], navigate to the game"))
      
      # navigate to the game page
      remDr$navigate(Game_URL)
      
      Sys.sleep(system_sleep)
      
      Headings=remDr$findElements(using='css selector', value='.event-panel__heading__market-name')
      Sys.sleep(1)
    }
    Headings_Text=unlist(sapply(Headings, function(x){x$getElementText()}))
    
    #*****
    # Date
    #*****
    GameTime=remDr$findElements(using='class name', value='event-card__event-time__date-time')
    GameTime_Text=unlist(sapply(GameTime, function(x){x$getElementText()}))
    
    Months_Text=c("Jan", "Feb", "Mar", "Apr",
                  "May", "Jun", "Jul", "Aug",
                  "Sep", "Oct", "Nov", "Dec")
    Days_Text=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    Time_Texts=unique(na.omit(unlist(strsplit(unlist(GameTime_Text), "[^a-zA-Z]+"))))
    Time_Texts=Time_Texts[Time_Texts!=""]
    Time_Nums=unique(na.omit(unlist(strsplit(unlist(GameTime_Text), "\\D"))))
    
    if(Time_Texts[1]=="Today"){
      #Month_Text=month(Sys.Date()+1)
      Day_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%d")
      Year_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%Y")
      Month_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%m")
      
    }else if(Time_Texts[1]=="Tomorrow"){
      #Month_Text=month(Sys.Date()+1)
      Day_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%d")
      Year_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%Y")
      Month_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%m")
      
    }else if(Time_Texts[1]%in%Days_Text){
      Month_Text=which(Months_Text==Time_Texts[2])
      Day_Text=Time_Nums[2]
      Year_Text=Time_Nums[3]
    }else{ # if only time is shown, the game is going to be held today
      Day_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%d")
      Year_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%Y")
      Month_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%m")
    }
    #
    Date_Text=as.Date(paste0(Month_Text, "/", Day_Text, "/", Year_Text), format="%m/%d/%Y")
    
    #*******
    # League
    #*******
    League
    
    #***************************
    # Team names (Home vs. Away)
    #***************************
    Teams=remDr$findElements(using='class name', value='scoreboard__row__cell--competitor')
    Teams_Text=unlist(sapply(Teams, function(x){x$getElementText()}))
    
    # count target options
    Target_Options=c("Total Goals (0.5)",
                     "Total Goals (1.5)", 
                     "Total Goals (2.5)", 
                     "Total Goals (3.5)", 
                     "Total Goals (4.5)", 
                     "Total Goals (5.5)")
    Target_Options_N=sum(Headings_Text%in%Target_Options)
  }
  
  # close all headings (the first 5 options are open by default)
  for(i in 1:5){
    #i=6
    Headings[[i]]$clickElement()
    Sys.sleep(1)
  }
  
  # open headings chosen
  for(i in which(Headings_Text%in%Target_Options)){
    #i=6
    Headings[[i]]$clickElement()
    Sys.sleep(1)
  }
  
  # over & under texts on the page (if the game is ongoing, this doesn't work)
  # Titles=remDr$findElements(using='css selector', value='.button--outcome__text')
  # Titles_Text=unlist(sapply(Titles, function(x){x$getElementText()}))
  #Sys.sleep(system_sleep)
  
  # odds of over & under on the page
  Odds=remDr$findElements(using='css selector', value='span.button--outcome__price')
  Odds_Text=unlist(sapply(Odds, function(x){x$getElementText()}))
  Sys.sleep(1)
  
  # target options available
  Target_Options_Available=Target_Options[Target_Options%in%Headings_Text[which(Headings_Text%in%Target_Options)]]
  
  #*****************************
  # enter odds of target options
  Over_Under_Score_Odds=data.table(
    Date=Date_Text,
    League=League,
    Home_Team=Teams_Text[1],
    Away_Team=Teams_Text[2],
    URL=Game_URL,
    `Over 0.5`=0,
    `Under 0.5`=0,
    `Over 1.5`=0,
    `Under 1.5`=0,
    `Over 2.5`=0,
    `Under 2.5`=0,
    `Over 3.5`=0,
    `Under 3.5`=0,
    `Over 4.5`=0,
    `Under 4.5`=0,
    `Over 5.5`=0,
    `Under 5.5`=0
  )
  
  if("Total Goals (0.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 0.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (0.5)")*2-1]),
                               `Under 0.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (0.5)")*2]))]
  }
  if("Total Goals (1.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 1.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (1.5)")*2-1]),
                               `Under 1.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (1.5)")*2]))]
  }
  if("Total Goals (2.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 2.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (2.5)")*2-1]),
                               `Under 2.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (2.5)")*2]))]
  }
  if("Total Goals (3.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 3.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (3.5)")*2-1]),
                               `Under 3.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (3.5)")*2]))]
  }
  if("Total Goals (4.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 4.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (4.5)")*2-1]),
                               `Under 4.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (4.5)")*2]))]
  }
  if("Total Goals (5.5)"%in%Target_Options_Available){
    Over_Under_Score_Odds[,
                          `:=`(`Over 5.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (5.5)")*2-1]),
                               `Under 5.5`=as.numeric(Odds_Text[which(Target_Options_Available=="Total Goals (5.5)")*2]))]
  }
  
  #***********************************************************
  # 2. results data to extract historical data for Kelly score
  #***********************************************************
  data.dir_1=paste0(data.dir.1, Country, "/", League, "/")
  
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
  # Over_Under_Score_Odds
  Over_Under_Score_Odds=Over_Under_Score_Odds_Manipulation(Over_Under_Score_Odds)
  # Results_Data
  Results_Data=Results_Manipulation(Results_Data)
  
  # # check -> there shouldn't any output
  # sort(setdiff(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])), unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))))
  # sort(setdiff(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])), unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team]))))
  # sort(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])))
  # sort(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])))
  
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
  
  # Over_Under_Score_Var (Variance of distribution of Over_Under_Score_`Exact_Prob at each Over_Under_Category)
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
  
  return(Over_Under_Score_Odds)
}





