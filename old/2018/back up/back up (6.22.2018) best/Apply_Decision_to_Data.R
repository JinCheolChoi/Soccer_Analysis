########################
# apply decision to data
########################
# place bets based on the decisions made
apply.decision = function(final.all.chosen.bet, initial.bankrole, ind.bet.money){
  Time = as.character(unique(final.all.chosen.bet$Time))
  
  bankrole = c()
  ind.bet = c()
  for(t.ind in 1:length(Time)){
    #t.ind=1
    if(t.ind == 1){
      bankrole$Time[1] = Time[t.ind]
      bankrole$capital[1] = initial.bankrole 
      
      # daily games
      daily.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind]),]
      
      # daily win games
      daily.win.games = daily.games[which(daily.games$result.mat == 1),]
      
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
        kelly.criterion = daily.games$kelly.criterion
        
        # normalize kelly criterion
        if(sum(kelly.criterion) > 1 ){kelly.criterion = kelly.criterion/sum(kelly.criterion)}
        
            bankrole$capital[t.ind] = initial.bankrole + 
              sum(daily.games$joint.odds[which(daily.games$result.mat == 1)]*((kelly.criterion * initial.bankrole)[which(daily.games$result.mat == 1)])) - 
              sum(kelly.criterion * initial.bankrole)
            
            # record individual daily bet
            ind.bet = c(ind.bet, kelly.criterion * initial.bankrole)
      }
      
      
      if(bankrole$capital[t.ind] <= 0){
        bankrole$capital[t.ind] = 0
      }
    }
    
    if(t.ind > 1){
      bankrole$Time[t.ind] = Time[t.ind]
      
      # balance the capital
      if(bankrole$capital[t.ind - 1] <= 0){
        bankrole$capital[t.ind] = 0
      }else{
        # daily games
        daily.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind]),]
        
        # daily win games
        daily.win.games = daily.games[which(daily.games$result.mat == 1),]
        
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
          kelly.criterion = daily.games$kelly.criterion
          
          # normalize kelly criterion
          if(sum(kelly.criterion) > 1 ){kelly.criterion = kelly.criterion/sum(kelly.criterion)}

              bankrole$capital[t.ind] = bankrole$capital[t.ind - 1] + 
                sum(daily.games$joint.odds[which(daily.games$result.mat == 1)]*((kelly.criterion * bankrole$capital[t.ind - 1])[which(daily.games$result.mat == 1)])) - 
                sum(kelly.criterion * bankrole$capital[t.ind - 1])
              
              # record individual daily bet
              ind.bet = c(ind.bet, kelly.criterion * bankrole$capital[t.ind - 1])
        }
      }
    }
  }
  
  bankrole$Time = as.Date(bankrole$Time, origin = "1970-01-01")
  
  bankrole$Ind.bet = ind.bet
  
  return(bankrole)
}


