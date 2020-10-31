########################
# apply decision to data
########################
# place bets based on the decisions made
apply.decision = function(final.all.chosen.bet, initial.bankrole){
  Time = as.character(unique(final.all.chosen.bet$Time))
  
  bankrole = c()
  for(t.ind in 1:length(Time)){
    #t.ind=1
    if(t.ind == 1){
      bankrole$Time[1] = Time[t.ind]
      bankrole$capital[1] = initial.bankrole 
      
      # daily win games
      daily.win.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind] & final.all.chosen.bet$result.mat == 1),]
      
      # balance the capital
      #ind.bet.money = bankrole$capital[t.ind]*0.01
      bankrole$capital[t.ind] = initial.bankrole + (sum(daily.win.games$joint.odds)-length(which(final.all.chosen.bet$Time == Time[t.ind])))*ind.bet.money
      
      if(bankrole$capital[t.ind] <= 0){
        bankrole$capital[t.ind] = 0
      }
    }
    
    if(t.ind > 1){
      bankrole$Time[t.ind] = Time[t.ind]
      # daily win games
      daily.win.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind] & final.all.chosen.bet$result.mat == 1),]
      
      # balance the capital
      if(bankrole$capital[t.ind - 1] <= 0){
        bankrole$capital[t.ind] = 0
      }else{
        #ind.bet.money = bankrole$capital[t.ind - 1]*0.01
        bankrole$capital[t.ind] = bankrole$capital[t.ind - 1] + (sum(daily.win.games$joint.odds)-length(which(final.all.chosen.bet$Time == Time[t.ind])))*ind.bet.money
      }
    }
  }
  
  bankrole$Time = as.Date(bankrole$Time, origin = "1970-01-01")
  
  return(bankrole)
}


