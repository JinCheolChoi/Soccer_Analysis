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
      
      temp.daily.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind]),]
      
      # balance the capital
      #ind.bet.money = bankrole$capital[t.ind]*0.01

      if(sum(temp.daily.games$opt.proportion) <= 1){
        bankrole$capital[t.ind] = initial.bankrole + sum(daily.win.games$joint.odds) - sum(temp.daily.games$opt.proportion*initial.bankrole)
      }
      if(sum(temp.daily.games$opt.proportion) > 1){
        temp.daily.games$opt.proportion = temp.daily.games$opt.proportion/sum(temp.daily.games$opt.proportion)
        bankrole$capital[t.ind] = initial.bankrole + sum(daily.win.games$joint.odds) - sum(temp.daily.games$opt.proportion*initial.bankrole)
      }
        
      if(bankrole$capital[t.ind] <= 0){
        bankrole$capital[t.ind] = 0
      }
    }
    
    if(t.ind > 1){
      bankrole$Time[t.ind] = Time[t.ind]
      # daily win games
      daily.win.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind] & final.all.chosen.bet$result.mat == 1),]
      temp.daily.games = final.all.chosen.bet[which(final.all.chosen.bet$Time == Time[t.ind]),]
      
      # balance the capital
      if(bankrole$capital[t.ind - 1] <= 0){
        bankrole$capital[t.ind] = 0
      }else{
        #ind.bet.money = bankrole$capital[t.ind - 1]*0.01
        
        # balance the capital
        #ind.bet.money = bankrole$capital[t.ind]*0.01
        
        if(sum(temp.daily.games$opt.proportion) <= 1){
          bankrole$capital[t.ind] = initial.bankrole + sum(daily.win.games$joint.odds) - sum(temp.daily.games$opt.proportion*initial.bankrole)
        }
        if(sum(temp.daily.games$opt.proportion) > 1){
          temp.daily.games$opt.proportion = temp.daily.games$opt.proportion/sum(temp.daily.games$opt.proportion)
          bankrole$capital[t.ind] = initial.bankrole + sum(daily.win.games$joint.odds) - sum(temp.daily.games$opt.proportion*initial.bankrole)
        }
      }
    }
  }
  
  bankrole$Time = as.Date(bankrole$Time, origin = "1970-01-01")
  
  return(bankrole)
}


