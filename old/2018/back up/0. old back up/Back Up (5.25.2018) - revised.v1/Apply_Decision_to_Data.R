########################
# apply decision to data
########################
#initial.bankrole=initial.bankrole
#rate=Rate # weight of criterion
bankrole=c()
bankrole$money[1]=initial.bankrole
final.decision$Result=0

all.date.in.decision = final.decision$Time
bankrole.n.length = length(all.date.in.decision) # the bankrole length

for(bankrole.n in 1:bankrole.n.length){ #1
  if(bankrole.n == 1){ # the initial bankrole is already set
    bankrole$money[bankrole.n]=initial.bankrole
  }
  
  if(bankrole.n > 1){ 
    bankrole$money[bankrole.n]=bankrole$money[bankrole.n-1]
    if(bankrole$money[bankrole.n-1] <= 0){ # the negative bankrole equals bankruptcy
      bankrole$money[bankrole.n]=0
    }
  }
  
  target.data.index=which(analysis.data$Time==all.date.in.decision[bankrole.n])
  target.data=analysis.data[target.data.index,]
  
  target.decision.index=which(final.decision$Time==all.date.in.decision[bankrole.n])
  target.decision=final.decision[target.decision.index,]
  
  ## invest money
  temp.bankrole=c()
  if(bankrole.n == 1){
    temp.bankrole=bankrole$money[bankrole.n]
  }else{temp.bankrole=bankrole$money[bankrole.n-1]}
  temp.invest=c()
  for(ind.bet.n in 1:nrow(target.decision)){ #2
    temp.invest[ind.bet.n]=temp.bankrole*target.decision$kelly.prop[ind.bet.n]*Rate
    temp.invest[ind.bet.n]=0
    
    ################################################################
    # if kelly.prop is between 0.27 and 0.3, it's worth it to invest
    ################################################################
    if(0.27 < target.decision$kelly.prop[ind.bet.n] & target.decision$kelly.prop[ind.bet.n] < 0.3){ # if kelly.prop is too small, not invest
      temp.invest[ind.bet.n]=temp.bankrole*target.decision$kelly.prop[ind.bet.n]*Rate
      if(target.decision$kelly.prop[ind.bet.n]*Rate > 1){ # not to exceed the current bankrole for invest
        temp.invest[ind.bet.n]=temp.bankrole*1
      }
      
      #######################################
      # set limited amount of betting at once
      #######################################
      if(temp.invest[ind.bet.n]>temp.bankrole*0.5){
        temp.invest[ind.bet.n]=temp.bankrole*0.5
      }
      
      ###########################################
      # set the maximum amount of betting at once
      ###########################################
      if(temp.invest[ind.bet.n]>3000){
        temp.invest[ind.bet.n]=3000
      }
    }
    
    ##############################################################
    # if kelly.prop is larger than 0.4, it's almost safe to invest
    ##############################################################
    # dare to take a risk at a high proportion
    # sm.binomial(x=final.decision$kelly.prop, y=final.decision$Result, h=0.03, xlab="kelly.prop")
    if( target.decision$kelly.prop[ind.bet.n] > 0.40){
      temp.invest[ind.bet.n]=temp.bankrole*0.9
    }
    
    ### if multiplying odds is larger than 1.6, remove
    # NB <- NaiveBayes(x=as.numeric(final.decision.filtered$game.1.odds)*as.numeric(final.decision.filtered$game.2.odds), grouping=as.factor(final.decision.filtered$Result), usekernel=TRUE)
    # plot(NB, lwd=2)
    #if( as.numeric(unlist(strsplit(as.character(target.decision$game.1.odds), " : ") )[2])*as.numeric(unlist(strsplit(as.character(target.decision$game.2.odds), " : ") )[2]) > 1.6){
    #  temp.invest[ind.bet.n]=0
    #}
    
    temp.bankrole=temp.bankrole-temp.invest[ind.bet.n]
  } #2
  
  #####################
  # If I bet some money
  #####################
  if(temp.invest != 0){ #5
    ## balance money
    for(ind.bet.n in nrow(target.decision):1){ #3
      #ind.bet.n=1
      ### decision I make
      game.1.H.team.name=as.character(target.decision[ind.bet.n,]$game.1.H.team)
      game.1.A.team.name=as.character(target.decision[ind.bet.n,]$game.1.A.team)
      game.2.H.team.name=as.character(target.decision[ind.bet.n,]$game.2.H.team)
      game.2.A.team.name=as.character(target.decision[ind.bet.n,]$game.2.A.team)
      
      temp.target.data.1=target.data[which(target.data$H.team==game.1.H.team.name),]
      first.choose=substring(as.character(target.decision[ind.bet.n,]$game.1.odds),1,1)
      
      temp.target.data.2=target.data[which(target.data$H.team==game.2.H.team.name),]
      second.choose=substring(as.character(target.decision[ind.bet.n,]$game.2.odds),1,1)
      
      ### compare my decision and result
      first.result=temp.target.data.1[which(temp.target.data.1$A.team==game.1.A.team.name),]$Result
      first.match=first.result==first.choose
      
      second.result=temp.target.data.2[which(temp.target.data.2$A.team==game.2.A.team.name),]$Result
      second.match=second.result==second.choose
      
      ### when I win a bet
      if(first.match == TRUE && second.match == TRUE){ #4
        ## dividend rate
        dvd.rate=as.numeric(strsplit(as.character(target.decision[ind.bet.n,]$game.1.odds)," : ")[[1]][2])*
          as.numeric(strsplit(as.character(target.decision[ind.bet.n,]$game.2.odds)," : ")[[1]][2])
        
        ## rate is a tunning parameter
        temp.bankrole=temp.bankrole+temp.invest[ind.bet.n]*dvd.rate
        
        ## count win
        final.decision$Result[target.decision.index] = 1
      }else{ ### when I lose a bet
        ## count lose
        final.decision$Result[target.decision.index] = -1
      } #4
    } #3
  } #5
  
  ###########################
  # If I do not bet any money
  ###########################
  if(temp.invest == 0){
    final.decision$Result[target.decision.index] = 0
  }
  
  bankrole$time[bankrole.n] = all.date.in.decision[bankrole.n]
  bankrole$money[bankrole.n] = temp.bankrole
} #1
