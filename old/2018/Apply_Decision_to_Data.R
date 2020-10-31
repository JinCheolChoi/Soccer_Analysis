########################
# apply decision to data
########################
# place bets based on the decisions made
apply.decision = function(final.all.chosen.bet, initial.bankrole, ind.bet.money){
  Time = as.character(unique(final.all.chosen.bet$Time))
  
  bankrole = c()
  ind.bet = c()
  for(t.ind in 1:length(Time)){
    #t.ind=2
    
    # daily games
    daily.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind]),]
    
    # daily win games
    daily.win.games = daily.games[which(daily.games$result.mat == 1),]
    
    if(t.ind == 1){
      bankrole$Time[1] = Time[t.ind]
      bankrole$capital[1] = initial.bankrole 
      
      if(bankrole$capital[t.ind] <= 0){
        bankrole$capital[t.ind] = 0
        ind.bet = c(ind.bet, rep(0, nrow(daily.games)))
      }else{
        #############################
        # balance the capital (fixed)
        #############################
        if(Type == "Fixed"){
          #ind.bet.money = bankrole$capital[t.ind]*0.01
          bankrole$capital[t.ind] = initial.bankrole + (sum(daily.win.games$joint.odds)-length(which(final.all.chosen.bet$Time == Time[t.ind])))*ind.bet.money
          
          # record individual daily bet
          ind.bet = c(ind.bet, rep(ind.bet.money, nrow(daily.games)))
        }
        
        #############################
        # balance the capital (kelly)
        #############################
        if(Type == "Kelly"){
          # kelly criterion
          kelly.criterion = daily.games$kelly.criterion*kelly.proportion
          
          # normalize kelly criterion
          if(sum(kelly.criterion) > 1 ){kelly.criterion = kelly.criterion/sum(kelly.criterion)}
          
          # maximum individual bets
          daily.bets = kelly.criterion * initial.bankrole
          daily.bets[which(daily.bets > initial.bankrole*max.proportion)] = initial.bankrole*max.proportion
          
          
          bankrole$capital[t.ind] = initial.bankrole + 
            sum(daily.games$joint.odds[which(daily.games$result.mat == 1)]*((daily.bets)[which(daily.games$result.mat == 1)])) - 
            sum(daily.bets)
          
          # record individual daily bet
          ind.bet = c(ind.bet, daily.bets)
          
        }
      }
    }
    
    if(t.ind > 1){
      bankrole$Time[t.ind] = Time[t.ind]
      
      # balance the capital
      if(bankrole$capital[t.ind - 1] <= 0){
        bankrole$capital[t.ind] = 0
        ind.bet = c(ind.bet, rep(0, nrow(daily.games)))
      }else{
        #############################
        # balance the capital (fixed)
        #############################
        if(Type == "Fixed"){
          #ind.bet.money = bankrole$capital[t.ind - 1]*0.01
          bankrole$capital[t.ind] = bankrole$capital[t.ind - 1] + (sum(daily.win.games$joint.odds)-length(which(final.all.chosen.bet$Time == Time[t.ind])))*ind.bet.money
          
          # record individual daily bet
          ind.bet = c(ind.bet, rep(ind.bet.money, nrow(daily.games)))
        }
        
        #############################
        # balance the capital (kelly)
        #############################
        if(Type == "Kelly"){
          # kelly criterion
          kelly.criterion = daily.games$kelly.criterion*kelly.proportion
          
          # normalize kelly criterion
          if(sum(kelly.criterion) > 1 ){kelly.criterion = kelly.criterion/sum(kelly.criterion)}
          
          # maximum individual bets
          daily.bets = kelly.criterion * bankrole$capital[t.ind - 1]
          daily.bets[which(daily.bets > bankrole$capital[t.ind - 1]*max.proportion)] = bankrole$capital[t.ind - 1]*max.proportion
          
          bankrole$capital[t.ind] = bankrole$capital[t.ind - 1] + 
            sum(daily.games$joint.odds[which(daily.games$result.mat == 1)]*((daily.bets)[which(daily.games$result.mat == 1)])) - 
            sum(daily.bets)
              
          # record individual daily bet
          ind.bet = c(ind.bet, daily.bets)
        }
      }
    }
  }
  
  bankrole$Time = as.Date(bankrole$Time, origin = "1970-01-01")
  
  bankrole$Ind.bet = ind.bet
  
  return(bankrole)
}

