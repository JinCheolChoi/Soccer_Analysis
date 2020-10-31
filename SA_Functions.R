#****************
#
# Kelly_Criterion
#
#****************
Kelly_Criterion=function(p, b, Coef=1){
  Kelly=(p)-(1-p)/(b*Coef-1)       
  
  return(Kelly)
}


#************************
#
# Poisson_Prob_Calculator
#
#************************
Poisson_Prob_Calculator=function(x, Results_Data){
  
  Mean=Results_Data[Home_Team==x["Home_Team"]&
                      Away_Team==x["Away_Team"]&
                      Exact_Date<x["Date"], Total_Goal] %>% mean # Date is important to take into account only data prior to the game
  Var=Results_Data[Home_Team==x["Home_Team"]&
                     Away_Team==x["Away_Team"]&
                     Exact_Date<x["Date"], Total_Goal] %>% var
  
  # Mean=Results_Data[Exact_Date<x["Date"], Total_Goal] %>% mean
  # Var=Results_Data[Exact_Date<x["Date"], Total_Goal] %>% var
  
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=1-ppois(0.5, lambda=Mean),
      `Under 0.5`=ppois(0.5, lambda=Mean),
      
      `Over 1.5`=1-ppois(1.5, lambda=Mean),
      `Under 1.5`=ppois(1.5, lambda=Mean),
      
      `Over 2.5`=1-ppois(2.5, lambda=Mean),
      `Under 2.5`=ppois(2.5, lambda=Mean),
      
      `Over 3.5`=1-ppois(3.5, lambda=Mean),
      `Under 3.5`=ppois(3.5, lambda=Mean),
      
      `Over 4.5`=1-ppois(4.5, lambda=Mean),
      `Under 4.5`=ppois(4.5, lambda=Mean),
      
      `Over 5.5`=1-ppois(5.5, lambda=Mean),
      `Under 5.5`=ppois(5.5, lambda=Mean)
    )
  )
}



#*******************************
#
# Negative_Binom_Prob_Calculator
#
#*******************************
Negative_Binom_Prob_Calculator=function(x, Results_Data){
  
  Mean=Results_Data[Home_Team==x["Home_Team"]&
                      Away_Team==x["Away_Team"]&
                      Exact_Date<x["Date"], Total_Goal] %>% mean # Date is important to take into account only data prior to the game
  Var=Results_Data[Home_Team==x["Home_Team"]&
                     Away_Team==x["Away_Team"]&
                     Exact_Date<x["Date"], Total_Goal] %>% var
  
  # Mean=Results_Data[Exact_Date<x["Date"], Total_Goal] %>% mean
  # Var=Results_Data[Exact_Date<x["Date"], Total_Goal] %>% var
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=1-pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 0.5`=pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean),
      
      `Over 1.5`=1-pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 1.5`=pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean),
      
      `Over 2.5`=1-pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 2.5`=pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean),
      
      `Over 3.5`=1-pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 3.5`=pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean),
      
      `Over 4.5`=1-pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 4.5`=pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean),
      
      `Over 5.5`=1-pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean),
      `Under 5.5`=pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean)
    )
  )
}


#**********************
#
# Exact_Prob_Calculator
#
#**********************
Exact_Prob_Calculator=function(x, Results_Data){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<0.5)), # Date is important to take into account only data prior to the game
      `Under 0.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<0.5),

      `Over 1.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<1.5)),
      `Under 1.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<1.5),

      `Over 2.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<2.5)),
      `Under 2.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<2.5),

      `Over 3.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<3.5)),
      `Under 3.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<3.5),
      `Over 4.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<4.5)),
      `Under 4.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<4.5),

      `Over 5.5`=(1-mean(Results_Data[Home_Team==x["Home_Team"]&
                                        Away_Team==x["Away_Team"]&
                                        Exact_Date<x["Date"], Total_Goal]<5.5)),
      `Under 5.5`=mean(Results_Data[Home_Team==x["Home_Team"]&
                                      Away_Team==x["Away_Team"]&
                                      Exact_Date<x["Date"], Total_Goal]<5.5)
      
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


#************************
#
# Implied_Prob_Calculator
#
#************************
Implied_Prob_Calculator=function(x){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      `Over 0.5`=1/as.numeric(x["Over 0.5"]),
      `Under 0.5`=1/as.numeric(x["Under 0.5"]),
      
      `Over 1.5`=1/as.numeric(x["Over 1.5"]),
      `Under 1.5`=1/as.numeric(x["Under 1.5"]),
      
      `Over 2.5`=1/as.numeric(x["Over 2.5"]),
      `Under 2.5`=1/as.numeric(x["Under 2.5"]),
      
      `Over 3.5`=1/as.numeric(x["Over 3.5"]),
      `Under 3.5`=1/as.numeric(x["Under 3.5"]),
      
      `Over 4.5`=1/as.numeric(x["Over 4.5"]),
      `Under 4.5`=1/as.numeric(x["Under 4.5"]),
      
      `Over 5.5`=1/as.numeric(x["Over 5.5"]),
      `Under 5.5`=1/as.numeric(x["Under 5.5"])
    )
  )
}



#*************
#
# Game_Numbers
#
#*************
Game_Numbers=function(x, Results_Data){
  
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      
      N=length(Results_Data[Home_Team==x["Home_Team"]&
                              Away_Team==x["Away_Team"]&
                              Exact_Date<x["Date"], Total_Goal])
      
      #N=length(Results_Data[, Total_Goal])
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
  
  
  #************
  # laliga ----
  #************
  Over_Under_Score_Odds[Home_Team=="Athletic Bilbao", Home_Team:="Ath Bilbao"]
  Over_Under_Score_Odds[Away_Team=="Athletic Bilbao", Away_Team:="Ath Bilbao"]
  
  Over_Under_Score_Odds[Home_Team=="Atletico Madrid", Home_Team:="Atl. Madrid"]
  Over_Under_Score_Odds[Away_Team=="Atletico Madrid", Away_Team:="Atl. Madrid"]
  
  Over_Under_Score_Odds[Home_Team=="Deportivo Alaves", Home_Team:="Alaves"]
  Over_Under_Score_Odds[Away_Team=="Deportivo Alaves", Away_Team:="Alaves"]
  
  Over_Under_Score_Odds[Home_Team=="Elche CF", Home_Team:="Elche"]
  Over_Under_Score_Odds[Away_Team=="Elche CF", Away_Team:="Elche"]
  
  Over_Under_Score_Odds[Home_Team=="FC Barcelona", Home_Team:="Barcelona"]
  Over_Under_Score_Odds[Away_Team=="FC Barcelona", Away_Team:="Barcelona"]
  
  Over_Under_Score_Odds[Home_Team=="Levante UD", Home_Team:="Levante"]
  Over_Under_Score_Odds[Away_Team=="Levante UD", Away_Team:="Levante"]
  
  Over_Under_Score_Odds[Home_Team=="RC Celta de Vigo", Home_Team:="Celta Vigo"]
  Over_Under_Score_Odds[Away_Team=="RC Celta de Vigo", Away_Team:="Celta Vigo"]
  
  Over_Under_Score_Odds[Home_Team=="Real Valladolid", Home_Team:="Valladolid"]
  Over_Under_Score_Odds[Away_Team=="Real Valladolid", Away_Team:="Valladolid"]
  
  Over_Under_Score_Odds[Home_Team=="SD Eibar", Home_Team:="Eibar"]
  Over_Under_Score_Odds[Away_Team=="SD Eibar", Away_Team:="Eibar"]
  
  Over_Under_Score_Odds[Home_Team=="SD Huesca", Home_Team:="Huesca"]
  Over_Under_Score_Odds[Away_Team=="SD Huesca", Away_Team:="Huesca"]
  
  Over_Under_Score_Odds[Home_Team=="Sevilla FC", Home_Team:="Sevilla"]
  Over_Under_Score_Odds[Away_Team=="Sevilla FC", Away_Team:="Sevilla"]
  
  Over_Under_Score_Odds[Home_Team=="Villarreal CF", Home_Team:="Villarreal"]
  Over_Under_Score_Odds[Away_Team=="Villarreal CF", Away_Team:="Villarreal"]
  
  Over_Under_Score_Odds[Home_Team=="Getafe CF", Home_Team:="Getafe"]
  Over_Under_Score_Odds[Away_Team=="Getafe CF", Away_Team:="Getafe"]
  
  Over_Under_Score_Odds[Home_Team=="Real Betis Balompie", Home_Team:="Betis"]
  Over_Under_Score_Odds[Away_Team=="Real Betis Balompie", Away_Team:="Betis"]
  
  Over_Under_Score_Odds[Home_Team=="Valencia CF", Home_Team:="Valencia"]
  Over_Under_Score_Odds[Away_Team=="Valencia CF", Away_Team:="Valencia"]
  
  Over_Under_Score_Odds[Home_Team=="CA Osasuna", Home_Team:="Osasuna"]
  Over_Under_Score_Odds[Away_Team=="CA Osasuna", Away_Team:="Osasuna"]
  
  Over_Under_Score_Odds[Home_Team=="Fe Club de Futbol", Home_Team:="Getafe"] # same as Fe CF Getafe
  Over_Under_Score_Odds[Away_Team=="Fe Club de Futbol", Away_Team:="Getafe"]
  
  Over_Under_Score_Odds[Home_Team=="Fe CF Getafe", Home_Team:="Getafe"]
  Over_Under_Score_Odds[Away_Team=="Fe CF Getafe", Away_Team:="Getafe"]
  
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
  
  
  #*************
  # serie-a ----
  #*************
  Over_Under_Score_Odds[Home_Team=="Atalanta BC", Home_Team:="Atalanta"]
  Over_Under_Score_Odds[Away_Team=="Atalanta BC", Away_Team:="Atalanta"]
  
  Over_Under_Score_Odds[Home_Team=="Benevento Calcio", Home_Team:="Benevento"]
  Over_Under_Score_Odds[Away_Team=="Benevento Calcio", Away_Team:="Benevento"]
  
  Over_Under_Score_Odds[Home_Team=="Bologna FC", Home_Team:="Bologna"]
  Over_Under_Score_Odds[Away_Team=="Bologna FC", Away_Team:="Bologna"]
  
  Over_Under_Score_Odds[Home_Team=="Cagliari Calcio", Home_Team:="Cagliari"]
  Over_Under_Score_Odds[Away_Team=="Cagliari Calcio", Away_Team:="Cagliari"]
  
  Over_Under_Score_Odds[Home_Team=="Hellas Verona", Home_Team:="Verona"]
  Over_Under_Score_Odds[Away_Team=="Hellas Verona", Away_Team:="Verona"]
  
  Over_Under_Score_Odds[Home_Team=="Inter Milano", Home_Team:="Inter"]
  Over_Under_Score_Odds[Away_Team=="Inter Milano", Away_Team:="Inter"]
  
  Over_Under_Score_Odds[Home_Team=="Juventus Turin", Home_Team:="Juventus"]
  Over_Under_Score_Odds[Away_Team=="Juventus Turin", Away_Team:="Juventus"]
  
  Over_Under_Score_Odds[Home_Team=="Lazio Roma", Home_Team:="Lazio"]
  Over_Under_Score_Odds[Away_Team=="Lazio Roma", Away_Team:="Lazio"]
  
  Over_Under_Score_Odds[Home_Team=="Parma Calcio", Home_Team:="Parma"]
  Over_Under_Score_Odds[Away_Team=="Parma Calcio", Away_Team:="Parma"]
  
  Over_Under_Score_Odds[Home_Team=="Spezia Calcio", Home_Team:="Spezia"]
  Over_Under_Score_Odds[Away_Team=="Spezia Calcio", Away_Team:="Spezia"]
  
  Over_Under_Score_Odds[Home_Team=="SSC Napoli", Home_Team:="Napoli"]
  Over_Under_Score_Odds[Away_Team=="SSC Napoli", Away_Team:="Napoli"]
  
  Over_Under_Score_Odds[Home_Team=="Udinese Calcio", Home_Team:="Udinese"]
  Over_Under_Score_Odds[Away_Team=="Udinese Calcio", Away_Team:="Udinese"]
  
  Over_Under_Score_Odds[Home_Team=="Sampdoria Genoa", Home_Team:="Sampdoria"]
  Over_Under_Score_Odds[Away_Team=="Sampdoria Genoa", Away_Team:="Sampdoria"]
  
  Over_Under_Score_Odds[Home_Team=="ACF Fiorentina", Home_Team:="Fiorentina"]
  Over_Under_Score_Odds[Away_Team=="ACF Fiorentina", Away_Team:="Fiorentina"]
  
  Over_Under_Score_Odds[Home_Team=="FC Crotone", Home_Team:="Crotone"]
  Over_Under_Score_Odds[Away_Team=="FC Crotone", Away_Team:="Crotone"]
  
  Over_Under_Score_Odds[Home_Team=="Sassuolo Calcio", Home_Team:="Sassuolo"]
  Over_Under_Score_Odds[Away_Team=="Sassuolo Calcio", Away_Team:="Sassuolo"]
  
  Over_Under_Score_Odds[Home_Team=="Torino FC", Home_Team:="Torino"]
  Over_Under_Score_Odds[Away_Team=="Torino FC", Away_Team:="Torino"]
  
  Over_Under_Score_Odds[Home_Team=="Genoa CFC", Home_Team:="Genoa"]
  Over_Under_Score_Odds[Away_Team=="Genoa CFC", Away_Team:="Genoa"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #********************
  # eerste-divisie ----
  #********************
  Over_Under_Score_Odds[Home_Team=="Roda JC Kerkrade", Home_Team:="Roda"]
  Over_Under_Score_Odds[Away_Team=="Roda JC Kerkrade", Away_Team:="Roda"]
  
  Over_Under_Score_Odds[Home_Team=="De Graafschap", Home_Team:="Graafschap"]
  Over_Under_Score_Odds[Away_Team=="De Graafschap", Away_Team:="Graafschap"]
  
  Over_Under_Score_Odds[Home_Team=="FC Dordrecht", Home_Team:="Dordrecht"]
  Over_Under_Score_Odds[Away_Team=="FC Dordrecht", Away_Team:="Dordrecht"]
  
  Over_Under_Score_Odds[Home_Team=="SBV Excelsior", Home_Team:="Excelsior"]
  Over_Under_Score_Odds[Away_Team=="SBV Excelsior", Away_Team:="Excelsior"]
  
  Over_Under_Score_Odds[Home_Team=="TOP Oss", Home_Team:="Oss"]
  Over_Under_Score_Odds[Away_Team=="TOP Oss", Away_Team:="Oss"]
  
  Over_Under_Score_Odds[Home_Team=="Almere City FC", Home_Team:="Almere City"]
  Over_Under_Score_Odds[Away_Team=="Almere City FC", Away_Team:="Almere City"]
  
  Over_Under_Score_Odds[Home_Team=="FC Den Bosch", Home_Team:="Den Bosch"]
  Over_Under_Score_Odds[Away_Team=="FC Den Bosch", Away_Team:="Den Bosch"]
  
  Over_Under_Score_Odds[Home_Team=="Helmond Sport", Home_Team:="Helmond"]
  Over_Under_Score_Odds[Away_Team=="Helmond Sport", Away_Team:="Helmond"]
  
  Over_Under_Score_Odds[Home_Team=="SC Cambuur", Home_Team:="Cambuur"]
  Over_Under_Score_Odds[Away_Team=="SC Cambuur", Away_Team:="Cambuur"]
  
  Over_Under_Score_Odds[Home_Team=="FC Eindhoven", Home_Team:="Eindhoven FC"]
  Over_Under_Score_Odds[Away_Team=="FC Eindhoven", Away_Team:="Eindhoven FC"]
  
  Over_Under_Score_Odds[Home_Team=="Go Ahead Eagles", Home_Team:="G.A. Eagles"]
  Over_Under_Score_Odds[Away_Team=="Go Ahead Eagles", Away_Team:="G.A. Eagles"]
  
  Over_Under_Score_Odds[Home_Team=="MVV Maastricht", Home_Team:="Maastricht"]
  Over_Under_Score_Odds[Away_Team=="MVV Maastricht", Away_Team:="Maastricht"]
  
  Over_Under_Score_Odds[Home_Team=="SC Telstar", Home_Team:="Telstar"]
  Over_Under_Score_Odds[Away_Team=="SC Telstar", Away_Team:="Telstar"]
  
  Over_Under_Score_Odds[Home_Team=="Jong Ajax Amsterdam", Home_Team:="Jong Ajax"]
  Over_Under_Score_Odds[Away_Team=="Jong Ajax Amsterdam", Away_Team:="Jong Ajax"]
  
  Over_Under_Score_Odds[Home_Team=="Jong AZ Alkmaar", Home_Team:="Jong AZ"]
  Over_Under_Score_Odds[Away_Team=="Jong AZ Alkmaar", Away_Team:="Jong AZ"]
  
  Over_Under_Score_Odds[Home_Team=="Jong FC Utrecht", Home_Team:="Jong Utrecht"]
  Over_Under_Score_Odds[Away_Team=="Jong FC Utrecht", Away_Team:="Jong Utrecht"]
  
  Over_Under_Score_Odds[Home_Team=="Jong PSV Eindhoven", Home_Team:="Jong PSV"]
  Over_Under_Score_Odds[Away_Team=="Jong PSV Eindhoven", Away_Team:="Jong PSV"]
  
  Over_Under_Score_Odds[Home_Team=="NEC Nijmegen", Home_Team:="Nijmegen"]
  Over_Under_Score_Odds[Away_Team=="NEC Nijmegen", Away_Team:="Nijmegen"]
  
  Over_Under_Score_Odds[Home_Team=="NAC Breda", Home_Team:="Breda"]
  Over_Under_Score_Odds[Away_Team=="NAC Breda", Away_Team:="Breda"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #************
  # 3-liga ----
  #************
  Over_Under_Score_Odds[Home_Team=="1 FC Kaiserslautern", Home_Team:="Kaiserslautern"]
  Over_Under_Score_Odds[Away_Team=="1 FC Kaiserslautern", Away_Team:="Kaiserslautern"]
  
  Over_Under_Score_Odds[Home_Team=="FC Bayern Munich II", Home_Team:="Bayern II"]
  Over_Under_Score_Odds[Away_Team=="FC Bayern Munich II", Away_Team:="Bayern II"]
  
  Over_Under_Score_Odds[Home_Team=="FC Hansa Rostock", Home_Team:="Hansa Rostock"]
  Over_Under_Score_Odds[Away_Team=="FC Hansa Rostock", Away_Team:="Hansa Rostock"]
  
  Over_Under_Score_Odds[Home_Team=="Krefelder FC Uerdingen 05", Home_Team:="Uerdingen"]
  Over_Under_Score_Odds[Away_Team=="Krefelder FC Uerdingen 05", Away_Team:="Uerdingen"]
  
  Over_Under_Score_Odds[Home_Team=="SC Verl", Home_Team:="Verl"]
  Over_Under_Score_Odds[Away_Team=="SC Verl", Away_Team:="Verl"]
  
  Over_Under_Score_Odds[Home_Team=="SV Waldhof Mannheim 07", Home_Team:="Mannheim"]
  Over_Under_Score_Odds[Away_Team=="SV Waldhof Mannheim 07", Away_Team:="Mannheim"]
  
  Over_Under_Score_Odds[Home_Team=="1. FC Magdeburg", Home_Team:="Magdeburg"]
  Over_Under_Score_Odds[Away_Team=="1. FC Magdeburg", Away_Team:="Magdeburg"]
  
  Over_Under_Score_Odds[Home_Team=="SV Wehen Wiesbaden", Home_Team:="Wehen"]
  Over_Under_Score_Odds[Away_Team=="SV Wehen Wiesbaden", Away_Team:="Wehen"]
  
  Over_Under_Score_Odds[Home_Team=="TSV 1860 Munich", Home_Team:="Munich 1860"]
  Over_Under_Score_Odds[Away_Team=="TSV 1860 Munich", Away_Team:="Munich 1860"]
  
  Over_Under_Score_Odds[Home_Team=="VfB Lubeck", Home_Team:="Lubeck"]
  Over_Under_Score_Odds[Away_Team=="VfB Lubeck", Away_Team:="Lubeck"]
  
  Over_Under_Score_Odds[Home_Team=="FC Ingolstadt 04", Home_Team:="Ingolstadt"]
  Over_Under_Score_Odds[Away_Team=="FC Ingolstadt 04", Away_Team:="Ingolstadt"]
  
  Over_Under_Score_Odds[Home_Team=="FC Viktoria Koln", Home_Team:="Viktoria Koln"]
  Over_Under_Score_Odds[Away_Team=="FC Viktoria Koln", Away_Team:="Viktoria Koln"]
  
  Over_Under_Score_Odds[Home_Team=="SpVgg Unterhaching", Home_Team:="Unterhaching"]
  Over_Under_Score_Odds[Away_Team=="SpVgg Unterhaching", Away_Team:="Unterhaching"]
  
  Over_Under_Score_Odds[Home_Team=="SV Meppen 1912", Home_Team:="Meppen"]
  Over_Under_Score_Odds[Away_Team=="SV Meppen 1912", Away_Team:="Meppen"]
  
  Over_Under_Score_Odds[Home_Team=="FSV Zwickau", Home_Team:="Zwickau"]
  Over_Under_Score_Odds[Away_Team=="FSV Zwickau", Away_Team:="Zwickau"]
  
  Over_Under_Score_Odds[Home_Team=="Hallescher FC", Home_Team:="Hallescher"]
  Over_Under_Score_Odds[Away_Team=="Hallescher FC", Away_Team:="Hallescher"]
  
  Over_Under_Score_Odds[Home_Team=="1. FC Saarbrucken", Home_Team:="Saarbrucken"]
  Over_Under_Score_Odds[Away_Team=="1. FC Saarbrucken", Away_Team:="Saarbrucken"]
  
  Over_Under_Score_Odds[Home_Team=="MSV Duisburg", Home_Team:="Duisburg"]
  Over_Under_Score_Odds[Away_Team=="MSV Duisburg", Away_Team:="Duisburg"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #******************
  # super-league ----
  #******************
  Over_Under_Score_Odds[Home_Team=="Hebei China Fortune FC", Home_Team:="Hebei"]
  Over_Under_Score_Odds[Away_Team=="Hebei China Fortune FC", Away_Team:="Hebei"]
  
  Over_Under_Score_Odds[Home_Team=="Shenzhen FC", Home_Team:="Shenzhen"]
  Over_Under_Score_Odds[Away_Team=="Shenzhen FC", Away_Team:="Shenzhen"]
  
  Over_Under_Score_Odds[Home_Team=="Beijing Guoan FC", Home_Team:="Beijing Guoan"]
  Over_Under_Score_Odds[Away_Team=="Beijing Guoan FC", Away_Team:="Beijing Guoan"]
  
  Over_Under_Score_Odds[Home_Team=="Dalian Professional FC", Home_Team:="Dalian Pro"]
  Over_Under_Score_Odds[Away_Team=="Dalian Professional FC", Away_Team:="Dalian Pro"]
  
  Over_Under_Score_Odds[Home_Team=="Shijiazhuang Ever Bright FC", Home_Team:="Shijiazhuang"]
  Over_Under_Score_Odds[Away_Team=="Shijiazhuang Ever Bright FC", Away_Team:="Shijiazhuang"]
  
  Over_Under_Score_Odds[Home_Team=="Wuhan Zall FC", Home_Team:="Wuhan Zall"]
  Over_Under_Score_Odds[Away_Team=="Wuhan Zall FC", Away_Team:="Wuhan Zall"]
  
  Over_Under_Score_Odds[Home_Team=="Guangzhou R&F FC", Home_Team:="Guangzhou R&F"]
  Over_Under_Score_Odds[Away_Team=="Guangzhou R&F FC", Away_Team:="Guangzhou R&F"]
  
  Over_Under_Score_Odds[Home_Team=="Jiangsu Suning FC", Home_Team:="Jiangsu Suning"]
  Over_Under_Score_Odds[Away_Team=="Jiangsu Suning FC", Away_Team:="Jiangsu Suning"]
  
  # Over_Under_Score_Odds[Home_Team=="Qingdao Huanghai FC", Home_Team:=""]
  # Over_Under_Score_Odds[Away_Team=="Qingdao Huanghai FC", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #***************
  # j1-league ----
  #***************
  Over_Under_Score_Odds[Home_Team=="Kashima Antlers", Home_Team:="Kashiwa"]
  Over_Under_Score_Odds[Away_Team=="Kashima Antlers", Away_Team:="Kashiwa"]
  
  Over_Under_Score_Odds[Home_Team=="Nagoya Grampus", Home_Team:="Nagoya"]
  Over_Under_Score_Odds[Away_Team=="Nagoya Grampus", Away_Team:="Nagoya"]
  
  Over_Under_Score_Odds[Home_Team=="Shonan Bellmare", Home_Team:="Shonan"]
  Over_Under_Score_Odds[Away_Team=="Shonan Bellmare", Away_Team:="Shonan"]
  
  Over_Under_Score_Odds[Home_Team=="Vissel Kobe", Home_Team:="Kobe"]
  Over_Under_Score_Odds[Away_Team=="Vissel Kobe", Away_Team:="Kobe"]
  
  Over_Under_Score_Odds[Home_Team=="Yokohama F Marinos", Home_Team:="Yokohama M."]
  Over_Under_Score_Odds[Away_Team=="Yokohama F Marinos", Away_Team:="Yokohama M."]
  
  Over_Under_Score_Odds[Home_Team=="Hokkaido Consadole Sapporo", Home_Team:="Sapporo"]
  Over_Under_Score_Odds[Away_Team=="Hokkaido Consadole Sapporo", Away_Team:="Sapporo"]
  
  Over_Under_Score_Odds[Home_Team=="Sanfrecce Hiroshima", Home_Team:="Hiroshima"]
  Over_Under_Score_Odds[Away_Team=="Sanfrecce Hiroshima", Away_Team:="Hiroshima"]
  
  Over_Under_Score_Odds[Home_Team=="Yokohama FC", Home_Team:="Yokohama"]
  Over_Under_Score_Odds[Away_Team=="Yokohama FC", Away_Team:="Yokohama"]
  
  Over_Under_Score_Odds[Home_Team=="Cerezo Osaka", Home_Team:="C-Osaka"]
  Over_Under_Score_Odds[Away_Team=="Cerezo Osaka", Away_Team:="C-Osaka"]
  
  Over_Under_Score_Odds[Home_Team=="Gamba Osaka", Home_Team:="G-Osaka"]
  Over_Under_Score_Odds[Away_Team=="Gamba Osaka", Away_Team:="G-Osaka"]
  
  Over_Under_Score_Odds[Home_Team=="Kashiwa Reysol", Home_Team:="Kashima"]
  Over_Under_Score_Odds[Away_Team=="Kashiwa Reysol", Away_Team:="Kashima"]
  
  Over_Under_Score_Odds[Home_Team=="Urawa Red Diamonds", Home_Team:="Urawa"]
  Over_Under_Score_Odds[Away_Team=="Urawa Red Diamonds", Away_Team:="Urawa"]
  
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
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #***************
  # j2-league ----
  #***************
  Over_Under_Score_Odds[Home_Team=="Ehime FC", Home_Team:="Ehime"]
  Over_Under_Score_Odds[Away_Team=="Ehime FC", Away_Team:="Ehime"]
  
  Over_Under_Score_Odds[Home_Team=="Fagiano Okayama", Home_Team:="Okayama"]
  Over_Under_Score_Odds[Away_Team=="Fagiano Okayama", Away_Team:="Okayama"]
  
  Over_Under_Score_Odds[Home_Team=="FC Ryukyu", Home_Team:="Ryukyu"]
  Over_Under_Score_Odds[Away_Team=="FC Ryukyu", Away_Team:="Ryukyu"]
  
  Over_Under_Score_Odds[Home_Team=="JEF United Ichihara Chiba", Home_Team:="Chiba"]
  Over_Under_Score_Odds[Away_Team=="JEF United Ichihara Chiba", Away_Team:="Chiba"]
  
  Over_Under_Score_Odds[Home_Team=="Jubilo Iwata", Home_Team:="Iwata"]
  Over_Under_Score_Odds[Away_Team=="Jubilo Iwata", Away_Team:="Iwata"]
  
  Over_Under_Score_Odds[Home_Team=="Kyoto Sanga FC", Home_Team:="Kyoto"]
  Over_Under_Score_Odds[Away_Team=="Kyoto Sanga FC", Away_Team:="Kyoto"]
  
  Over_Under_Score_Odds[Home_Team=="Machida Zelvia", Home_Team:="Machida"]
  Over_Under_Score_Odds[Away_Team=="Machida Zelvia", Away_Team:="Machida"]
  
  Over_Under_Score_Odds[Home_Team=="Matsumoto Yamaga FC", Home_Team:="Yamaga"]
  Over_Under_Score_Odds[Away_Team=="Matsumoto Yamaga FC", Away_Team:="Yamaga"]
  
  Over_Under_Score_Odds[Home_Team=="Mito Hollyhock", Home_Team:="Mito"]
  Over_Under_Score_Odds[Away_Team=="Mito Hollyhock", Away_Team:="Mito"]
  
  Over_Under_Score_Odds[Home_Team=="Niigata Albirex", Home_Team:="Albirex Niigata"]
  Over_Under_Score_Odds[Away_Team=="Niigata Albirex", Away_Team:="Albirex Niigata"]
  
  Over_Under_Score_Odds[Home_Team=="Thespakusatsu", Home_Team:="Kusatsu"]
  Over_Under_Score_Odds[Away_Team=="Thespakusatsu", Away_Team:="Kusatsu"]
  
  Over_Under_Score_Odds[Home_Team=="Tochigi SC", Home_Team:="Tochigi"]
  Over_Under_Score_Odds[Away_Team=="Tochigi SC", Away_Team:="Tochigi"]
  
  Over_Under_Score_Odds[Home_Team=="Tokushima Vortis", Home_Team:="Tokushima"]
  Over_Under_Score_Odds[Away_Team=="Tokushima Vortis", Away_Team:="Tokushima"]
  
  Over_Under_Score_Odds[Home_Team=="Tokyo Verdy", Home_Team:="Verdy"]
  Over_Under_Score_Odds[Away_Team=="Tokyo Verdy", Away_Team:="Verdy"]
  
  Over_Under_Score_Odds[Home_Team=="Ventforet Kofu", Home_Team:="Kofu"]
  Over_Under_Score_Odds[Away_Team=="Ventforet Kofu", Away_Team:="Kofu"]
  
  Over_Under_Score_Odds[Home_Team=="Zweigen Kanazawa", Home_Team:="Kanazawa"]
  Over_Under_Score_Odds[Away_Team=="Zweigen Kanazawa", Away_Team:="Kanazawa"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #***************
  # super-lig ----
  #***************
  Over_Under_Score_Odds[Home_Team=="Goztepe SK", Home_Team:="Goztepe"]
  Over_Under_Score_Odds[Away_Team=="Goztepe SK", Away_Team:="Goztepe"]
  
  Over_Under_Score_Odds[Home_Team=="Antalyaspor AS", Home_Team:="Antalyaspor"]
  Over_Under_Score_Odds[Away_Team=="Antalyaspor AS", Away_Team:="Antalyaspor"]
  
  Over_Under_Score_Odds[Home_Team=="Buyuksehir Belediye Erzurumspor", Home_Team:="Erzurum BB"]
  Over_Under_Score_Odds[Away_Team=="Buyuksehir Belediye Erzurumspor", Away_Team:="Erzurum BB"]
  
  Over_Under_Score_Odds[Home_Team=="Galatasaray SK", Home_Team:="Galatasaray"]
  Over_Under_Score_Odds[Away_Team=="Galatasaray SK", Away_Team:="Galatasaray"]
  
  Over_Under_Score_Odds[Home_Team=="Gaziantep FK", Home_Team:="Gaziantep"]
  Over_Under_Score_Odds[Away_Team=="Gaziantep FK", Away_Team:="Gaziantep"]
  
  Over_Under_Score_Odds[Home_Team=="Istanbul Basaksehir FK", Home_Team:="Basaksehir"]
  Over_Under_Score_Odds[Away_Team=="Istanbul Basaksehir FK", Away_Team:="Basaksehir"]
  
  Over_Under_Score_Odds[Home_Team=="Konyaspor Club", Home_Team:="Konyaspor"]
  Over_Under_Score_Odds[Away_Team=="Konyaspor Club", Away_Team:="Konyaspor"]
  
  Over_Under_Score_Odds[Home_Team=="Fatih Karagumruk SK", Home_Team:="Karagumruk"]
  Over_Under_Score_Odds[Away_Team=="Fatih Karagumruk SK", Away_Team:="Karagumruk"]
  
  Over_Under_Score_Odds[Home_Team=="Genclerbirligi SK", Home_Team:="Genclerbirligi"]
  Over_Under_Score_Odds[Away_Team=="Genclerbirligi SK", Away_Team:="Genclerbirligi"]
  
  Over_Under_Score_Odds[Home_Team=="Besiktas JK", Home_Team:="Besiktas"]
  Over_Under_Score_Odds[Away_Team=="Besiktas JK", Away_Team:="Besiktas"]
  
  Over_Under_Score_Odds[Home_Team=="Caykur Rizespor", Home_Team:="Rizespor"]
  Over_Under_Score_Odds[Away_Team=="Caykur Rizespor", Away_Team:="Rizespor"]
  
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
  Results_Data[, Home_Goal:=as.numeric(gsub("([0-9]+).*$", "\\1", Home_Goal))]
  Results_Data[, Away_Goal:=as.numeric(gsub("([0-9]+).*$", "\\1", Away_Goal))]
  
  Results_Data[, Total_Goal:=Home_Goal+Away_Goal]
  # Results_Data[, Total_Goal] %>% mean
  # Results_Data[, Total_Goal] %>% var
  
  return(Results_Data)
}


#**************************
#
# Over_Under_Betting_Result
#
#**************************
Over_Under_Betting_Result=function(Combined_Over_Under_Score_Odds, Combined_Results_Data){
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
Total_Goals_Kelly_Calculator=function(Game_URL, Country, League, Prob_Estimate, Chosen_Profit_Criteria, Coef=1, ...){
  # game URL
  #Game_URL=Over_under_score_odds[Game_Ind, URL]
  
  #*************************
  # 1. Over_Under_Score_Odds
  #*********
  # headings
  Target_Options_N=0
  print(paste0("Country : [", Country, "], League : [", League, "], Target_Options_N"))
  N_Try=1
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
      
      # if N_Try==5, re-open the server
      if(N_Try==5){
        print(paste0("Country : [", Country, "], League : [", League, "], re-open the server"))
        # close the current server
        remDr$close()
        # re-open server
        remDr$open(silent=T)
        
        # reset N_Try
        N_Try=1
      }else{
        N_Try=N_Try+1
      }
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
    if(is.null(GameTime_Text)){GameTime_Text="Today"} # if the game is ongoing, GameTime_Text is null
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
  print(paste0("Country : [", Country, "], League : [", League, "], open heandings chosen"))
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
    Results_Data=rbind(fread(paste0(data.dir_1, File_to_Open)), Results_Data, fill=T)
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
  Over_Under_Score_Odds=Compute_Kelly_Scores(Over_Under_Score_Odds, Results_Data, League, Prob_Estimate, Chosen_Profit_Criteria, Coef)
  
  #
  Over_Under_Score_Odds[, Record_Time:=as.character(Sys.time())]
  
  print(Over_Under_Score_Odds)
  return(Over_Under_Score_Odds)
}


#*********************
#
# Compute_Kelly_Scores
#
#*********************
Compute_Kelly_Scores=function(Over_Under_Score_Odds, Results_Data, League_Text, Prob_Estimate, Chosen_Profit_Criteria, Coef=1, ...) {
  # Over_Under_Categories
  
  Over_Under_Categories = c(
    "Over 0.5",
    "Under 0.5",
    "Over 1.5",
    "Under 1.5",
    "Over 2.5",
    "Under 2.5",
    "Over 3.5",
    "Under 3.5",
    "Over 4.5",
    "Under 4.5",
    "Over 5.5",
    "Under 5.5"
  )
  # Kelly score
  #Prob_Estimate="Poisson" # Poisson, Exact_Dist, Negative_Binom
  if (Prob_Estimate=="Poisson") {
    Poisson_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Poisson_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Poisson_Prob[,
                                   .SD,
                                   .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
    
  } else if (Prob_Estimate=="Negative_Binom") {
    Negative_Binom_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Negative_Binom_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Negative_Binom_Prob[,
                                          .SD,
                                          .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  } else if (Prob_Estimate=="Exact") {
    Exact_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Exact_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Exact_Prob[,
                                 .SD,
                                 .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  } else if (Prob_Estimate=="Implied") {
    Implied_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Implied_Prob_Calculator(x))
    ) 
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Implied_Prob[,
                                   .SD,
                                   .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  }
  
  # Over_Under_Score_Exact_Prob (proportion)
  Over_Under_Score_Exact_Prob = do.call(rbind.data.frame,
                                        apply(Over_Under_Score_Odds, 1, function(x)
                                          Exact_Prob_Calculator(x, Results_Data[League==League_Text, ])))
  
  # Number of games
  Game_Ns = do.call(rbind.data.frame,
                    apply(Over_Under_Score_Odds, 1, function(x)
                      Game_Numbers(x, Results_Data[League==League_Text, ])))
  
  
  # Over_Under_Score_Profit
  Over_Under_Score_Profit = cbind(
    Over_Under_Score_Odds[, .SD, .SDcols = c("Date", "League", "Home_Team", "Away_Team")],
    (Over_Under_Score_Odds[, .SD, .SDcols =
                             Over_Under_Categories] - 1) *
      Over_Under_Score_Kelly[, .SD, .SDcols =
                               Over_Under_Categories]
  )
  
  
  # Over_Under_Score_Var (Variance of distribution of Over_Under_Score_Exact_Prob at each Over_Under_Category)
  Over_Under_Score_Var = Over_Under_Score_Exact_Prob %>%
    left_join(Game_Ns, by = c("Date", "League", "Home_Team", "Away_Team")) %>%
    mutate(
      `Over 0.5 Var` = `Over 0.5` * (1 - `Over 0.5`) / N,
      `Under 0.5 Var` = `Over 0.5` * (1 - `Over 0.5`) / N,
      
      `Over 1.5 Var` = `Over 1.5` * (1 - `Over 1.5`) / N,
      `Under 1.5 Var` = `Under 1.5` * (1 - `Under 1.5`) / N,
      
      `Over 2.5 Var` = `Over 2.5` * (1 - `Over 2.5`) / N,
      `Under 2.5 Var` = `Under 2.5` * (1 - `Under 2.5`) / N,
      
      `Over 3.5 Var` = `Over 3.5` * (1 - `Over 3.5`) / N,
      `Under 3.5 Var` = `Under 3.5` * (1 - `Under 3.5`) / N,
      
      `Over 4.5 Var` = `Over 4.5` * (1 - `Over 4.5`) / N,
      `Under 4.5 Var` = `Under 4.5` * (1 - `Under 4.5`) / N,
      
      `Over 5.5 Var` = `Over 5.5` * (1 - `Over 5.5`) / N,
      `Under 5.5 Var` = `Under 5.5` * (1 - `Under 5.5`) / N
    ) %>%
    dplyr::select(
      Date,
      League,
      Home_Team,
      Away_Team,
      `Over 0.5 Var`,
      `Under 0.5 Var`,
      `Over 1.5 Var`,
      `Under 1.5 Var`,
      `Over 2.5 Var`,
      `Under 2.5 Var`,
      `Over 3.5 Var`,
      `Under 3.5 Var`,
      `Over 4.5 Var`,
      `Under 4.5 Var`,
      `Over 5.5 Var`,
      `Under 5.5 Var`
    )
  colnames(Over_Under_Score_Var) = c("Date",
                                     "League",
                                     "Home_Team",
                                     "Away_Team",
                                     Over_Under_Categories)
  
  # verify values
  Over_Under_Score_Odds
  Over_Under_Score_Kelly
  Over_Under_Score_Profit
  Over_Under_Score_Exact_Prob
  Over_Under_Score_Var
  
  # algorithm
  for (i in 1:nrow(Over_Under_Score_Profit)) {
    if (Chosen_Profit_Criteria==1) {
      #**************************************************
      # Chosen_Profit Criterion - (1) : by maximum profit
      Chosen_Profit = unlist(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                       c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                  c(Over_Under_Categories)] > 0)][which.max(unlist(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                                                                                             c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                                                                                                                                                        c(Over_Under_Categories)] > 0)])]
    } else if (Chosen_Profit_Criteria==2) {
      #**************************************************
      # Chosen_Profit Criterion - (2) : by minimum profit
      Chosen_Profit = unlist(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                       c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                  c(Over_Under_Categories)] > 0)][which.min(unlist(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                                                                                             c(Over_Under_Categories)])[which(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                                                                                                                                                        c(Over_Under_Categories)] > 0)])]
    } else if (Chosen_Profit_Criteria==3) {
      #***************************************************************************************************************************
      # Chosen_Profit Criterion - (3) : by a larger profit between the lowest option of "Over"s and the highest option of "Under"s
      Temp = unlist(Over_Under_Score_Profit[i, .SD, .SDcols = Over_Under_Categories])[which(Over_Under_Score_Profit[i, .SD, .SDcols =
                                                                                                                      Over_Under_Categories] > 0)] # consider only upto Under 5.5
      Over_Selected_Temp = grep("Over", names(Temp))
      Under_Selected_Temp = grep("Under", names(Temp))
      
      if (length(Over_Selected_Temp) + length(Under_Selected_Temp) >
          0) {
        # bet only if there is positive expected profit
        if (length(Over_Selected_Temp) > 0) {
          Over_Selected = c()
          Under_Selected = -100
          Over_Selected = Temp[min(Over_Selected_Temp)] # select the lowest Over options
        }
        if (length(Under_Selected_Temp) > 0) {
          Under_Selected = c()
          Over_Selected = -100
          Under_Selected = Temp[max(Under_Selected_Temp)] # select the highest Under options
        }
        Chosen_Profit = ifelse(Over_Selected >= Under_Selected,
                               Over_Selected,
                               Under_Selected)
        
      } else{
        # if there is no positive expected profit
        Chosen_Profit = NULL
      }
    }
    
    #
    if (length(Chosen_Profit) != 0) {
      #
      Over_Under_Score_Odds[i, `Chosen_Option` := names(Chosen_Profit)]
      Over_Under_Score_Odds[i, `Chosen_Odds` := as.numeric(Over_Under_Score_Odds[i, .SD, .SDcols =
                                                                                   names(Chosen_Profit)])]
      #Over_Under_Score_Odds[i, Result:=Over_Under_Score_Result[i, .SD, .SDcols=names(Chosen_Profit)]]
      Over_Under_Score_Odds[i, Chosen_Option_Empirical_Prob := as.numeric(Over_Under_Score_Exact_Prob[i,
                                                                                                      .SD,
                                                                                                      .SDcols =
                                                                                                        Over_Under_Score_Odds[i, Chosen_Option]])]
      Over_Under_Score_Odds[i, N := Game_Ns[i, .SD, .SDcols = c("N")]]
      Over_Under_Score_Odds[i, Chosen_Option_Std := sqrt(Over_Under_Score_Var[i,
                                                                              .SD,
                                                                              .SDcols =
                                                                                Over_Under_Score_Odds[i, Chosen_Option]])]
      Over_Under_Score_Odds[i, Kelly := Over_Under_Score_Kelly[i, .SD, .SDcols =
                                                                 names(Chosen_Profit)]]
    } else{
      #
      Over_Under_Score_Odds[i, `Chosen_Option` := ""]
      Over_Under_Score_Odds[i, `Chosen_Odds` := as.numeric("")]
      #Over_Under_Score_Odds[i, Result:=""]
      Over_Under_Score_Odds[i, Chosen_Option_Empirical_Prob := as.numeric("")]
      Over_Under_Score_Odds[i, N := as.numeric("")]
      Over_Under_Score_Odds[i, Chosen_Option_Std := as.numeric("")]
      Over_Under_Score_Odds[i, Kelly := as.numeric("")]
    }
    
  }
  
  return(Over_Under_Score_Odds)
}




#*****************************
#
# Results_Wide_Table_Generator
#
#*****************************
Results_Wide_Table_Generator=function(x){
  return(
    data.table(
      Date=x["Date"],
      League=x["League"],
      Home_Team=x["Home_Team"],
      Away_Team=x["Away_Team"],
      Home_Goal=x["Home_Goal"],
      Away_Goal=x["Away_Goal"],
      Total_Goal=x["Total_Goal"],
      `Over 0.5`=sum(x["Total_Goal"]>0.5),
      `Under 0.5`=sum(x["Total_Goal"]<0.5),
      `Over 1.5`=sum(x["Total_Goal"]>1.5),
      `Under 1.5`=sum(x["Total_Goal"]<1.5),
      `Over 2.5`=sum(x["Total_Goal"]>2.5),
      `Under 2.5`=sum(x["Total_Goal"]<2.5),
      `Over 3.5`=sum(x["Total_Goal"]>3.5),
      `Under 3.5`=sum(x["Total_Goal"]<3.5),
      `Over 4.5`=sum(x["Total_Goal"]>4.5),
      `Under 4.5`=sum(x["Total_Goal"]<4.5),
      `Over 5.5`=sum(x["Total_Goal"]>5.5),
      `Under 5.5`=sum(x["Total_Goal"]<5.5)
    )
  )
}

#*********************
#
# Long_Table_Generator
#
#*********************
Long_Table_Generator=function(Over_Under_Score_Odds, 
                              Results_Data, 
                              League_Text, 
                              Prob_Estimate, 
                              ...){
  # Over_Under_Categories
  Over_Under_Categories = c(
    "Over 0.5",
    "Under 0.5",
    "Over 1.5",
    "Under 1.5",
    "Over 2.5",
    "Under 2.5",
    "Over 3.5",
    "Under 3.5",
    "Over 4.5",
    "Under 4.5",
    "Over 5.5",
    "Under 5.5"
  )
  #Prob_Estimate="Poisson" # Poisson, Exact_Dist, Negative_Binom
  if (Prob_Estimate=="Poisson") {
    Poisson_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Poisson_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Poisson_Prob[,
                                   .SD,
                                   .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  } else if (Prob_Estimate=="Negative_Binom") {
    Negative_Binom_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Negative_Binom_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Negative_Binom_Prob[,
                                          .SD,
                                          .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  } else if (Prob_Estimate=="Exact") {
    Exact_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Exact_Prob_Calculator(x, Results_Data[League==League_Text, ]))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Exact_Prob[,
                                 .SD,
                                 .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  } else if (Prob_Estimate=="Implied") {
    Implied_Prob = do.call(
      rbind.data.frame,
      apply(Over_Under_Score_Odds, 1, function(x)
        Implied_Prob_Calculator(x))
    )
    Over_Under_Score_Kelly=data.table(
      Over_Under_Score_Odds[, .SD, .SDcols=c("Date", "League", "Home_Team", "Away_Team")],
      Kelly_Criterion(Implied_Prob[,
                                   .SD,
                                   .SDcols=c(Over_Under_Categories)],
                      Over_Under_Score_Odds[,
                                            .SD,
                                            .SDcols=c(Over_Under_Categories)],
                      Coef)
    )
  }
  
  # Over_Under_Score_Exact_Prob (proportion)
  Over_Under_Score_Exact_Prob = do.call(rbind.data.frame,
                                        apply(Over_Under_Score_Odds, 1, function(x)
                                          Exact_Prob_Calculator(x, Results_Data[League==League_Text, ])))
  
  # Number of games
  Game_Ns = do.call(rbind.data.frame,
                    apply(Over_Under_Score_Odds, 1, function(x)
                      Game_Numbers(x, Results_Data[League==League_Text, ])))
  
  
  # Over_Under_Score_Profit
  Over_Under_Score_Profit = cbind(
    Over_Under_Score_Odds[, .SD, .SDcols = c("Date", "League", "Home_Team", "Away_Team")],
    (Over_Under_Score_Odds[, .SD, .SDcols =
                             Over_Under_Categories] - 1) *
      Over_Under_Score_Kelly[, .SD, .SDcols =
                               Over_Under_Categories]
  )
  
  # Over_Under_Score_Var (Variance of distribution of Over_Under_Score_Exact_Prob at each Over_Under_Category)
  Over_Under_Score_Var = Over_Under_Score_Exact_Prob %>%
    left_join(Game_Ns, by = c("Date", "League", "Home_Team", "Away_Team")) %>%
    mutate(
      `Over 0.5 Var` = `Over 0.5` * (1 - `Over 0.5`) / N,
      `Under 0.5 Var` = `Over 0.5` * (1 - `Over 0.5`) / N,
      
      `Over 1.5 Var` = `Over 1.5` * (1 - `Over 1.5`) / N,
      `Under 1.5 Var` = `Under 1.5` * (1 - `Under 1.5`) / N,
      
      `Over 2.5 Var` = `Over 2.5` * (1 - `Over 2.5`) / N,
      `Under 2.5 Var` = `Under 2.5` * (1 - `Under 2.5`) / N,
      
      `Over 3.5 Var` = `Over 3.5` * (1 - `Over 3.5`) / N,
      `Under 3.5 Var` = `Under 3.5` * (1 - `Under 3.5`) / N,
      
      `Over 4.5 Var` = `Over 4.5` * (1 - `Over 4.5`) / N,
      `Under 4.5 Var` = `Under 4.5` * (1 - `Under 4.5`) / N,
      
      `Over 5.5 Var` = `Over 5.5` * (1 - `Over 5.5`) / N,
      `Under 5.5 Var` = `Under 5.5` * (1 - `Under 5.5`) / N
    ) %>%
    dplyr::select(
      Date,
      League,
      Home_Team,
      Away_Team,
      `Over 0.5 Var`,
      `Under 0.5 Var`,
      `Over 1.5 Var`,
      `Under 1.5 Var`,
      `Over 2.5 Var`,
      `Under 2.5 Var`,
      `Over 3.5 Var`,
      `Under 3.5 Var`,
      `Over 4.5 Var`,
      `Under 4.5 Var`,
      `Over 5.5 Var`,
      `Under 5.5 Var`
    )
  
  colnames(Over_Under_Score_Var) = c("Date",
                                     "League",
                                     "Home_Team",
                                     "Away_Team",
                                     Over_Under_Categories)
  
  # verify values
  Over_Under_Score_Odds
  Over_Under_Score_Kelly
  Over_Under_Score_Profit
  Over_Under_Score_Exact_Prob
  Over_Under_Score_Var
  
  #
  Rows_to_Remove=is.na(Over_Under_Score_Odds$Total_Goal)
  Over_Under_Score_Odds=Over_Under_Score_Odds[!Rows_to_Remove,]
  Over_Under_Score_Kelly=Over_Under_Score_Kelly[!Rows_to_Remove,]
  Over_Under_Score_Profit=Over_Under_Score_Profit[!Rows_to_Remove,]
  Over_Under_Score_Exact_Prob=Over_Under_Score_Exact_Prob[!Rows_to_Remove,]
  Over_Under_Score_Var=Over_Under_Score_Var[!Rows_to_Remove,]
  
  #*********************
  # generate long tables
  #*********************
  # Results_Wide
  Results_Wide=data.table(
    Over_Under_Score_Odds[, 
                          .SD, 
                          .SDcols=c("Date", "League", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal", "Total_Goal")]
  )
  Results_Wide = do.call(rbind.data.frame,
                         apply(Results_Wide, 1, function(x) Results_Wide_Table_Generator(x)))
  # Results_Long
  Results_Long=melt(Results_Wide, 
                    id.vars=c("Date", "League", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal", "Total_Goal")) %>% as.data.table
  Results_Long=Results_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Results_Long)=c("Date", "League", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal", "Total_Goal", "Bet", "Result")
  Results_Long[, Date:=as.Date(Date)]
  
  # Odds_Long
  Odds_Long=melt(Over_Under_Score_Odds[, 
                                       .SD, 
                                       .SDcols=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)],
                 id.vars=c("Date", "League", "Home_Team", "Away_Team")) %>% as.data.table
  Odds_Long=Odds_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Odds_Long)=c("Date", "League", "Home_Team", "Away_Team", "Bet", "Odds")
  Odds_Long[, Date:=as.Date(Date)]
  
  # Kelly_Long
  Kelly_Long=melt(Over_Under_Score_Kelly[, 
                                         .SD, 
                                         .SDcols=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)],
                  id.vars=c("Date", "League", "Home_Team", "Away_Team")) %>% as.data.table
  Kelly_Long=Kelly_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Kelly_Long)=c("Date", "League", "Home_Team", "Away_Team", "Bet", "Kelly")
  Kelly_Long[, Date:=as.Date(Date)]
  
  # Profit_Long
  Profit_Long=melt(Over_Under_Score_Profit[, 
                                           .SD, 
                                           .SDcols=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)],
                   id.vars=c("Date", "League", "Home_Team", "Away_Team")) %>% as.data.table
  Profit_Long=Profit_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Profit_Long)=c("Date", "League", "Home_Team", "Away_Team", "Bet", "Profit")
  Profit_Long[, Date:=as.Date(Date)]
  
  # Exact_Prob_Long
  Exact_Prob_Long=melt(Over_Under_Score_Exact_Prob[, 
                                                   .SD, 
                                                   .SDcols=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)],
                       id.vars=c("Date", "League", "Home_Team", "Away_Team")) %>% as.data.table
  Exact_Prob_Long=Exact_Prob_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Exact_Prob_Long)=c("Date", "League", "Home_Team", "Away_Team", "Bet", "Exact_Prob")
  Exact_Prob_Long[, Date:=as.Date(Date)]
  
  # Var_Long
  Var_Long=melt(Over_Under_Score_Var[, 
                                     .SD, 
                                     .SDcols=c("Date", "League", "Home_Team", "Away_Team", Over_Under_Categories)],
                id.vars=c("Date", "League", "Home_Team", "Away_Team")) %>% as.data.table
  Var_Long=Var_Long[order(Date, League, Home_Team, Away_Team),]
  colnames(Var_Long)=c("Date", "League", "Home_Team", "Away_Team", "Bet", "Var")
  Var_Long[, Date:=as.Date(Date)]
  
  # combine all long tables
  Combined_Results_Long = Results_Long %>% left_join(
    Odds_Long, by=c("Date", "League", "Home_Team", "Away_Team", "Bet")
  ) %>% left_join(
    Kelly_Long, by=c("Date", "League", "Home_Team", "Away_Team", "Bet")
  ) %>% left_join(
    Profit_Long, by=c("Date", "League", "Home_Team", "Away_Team", "Bet")
  ) %>% left_join(
    Exact_Prob_Long, by=c("Date", "League", "Home_Team", "Away_Team", "Bet")
  ) %>% left_join(
    Var_Long, by=c("Date", "League", "Home_Team", "Away_Team", "Bet")
  )
  
  #
  
  return(Combined_Results_Long)
}


