################################################################
# Decision making algorithm + calculate the proportion to invest
################################################################
final.decision=c()
final.n=0
N=length(Date.list) # The total number of days
for(n in 1:N){ #1
  #n=1
  time = Date.list[n]
  target.data = analysis.data[which(analysis.data$Time == time),]
  
  # If there are more than one game on the day, find the combinations.
  if(nrow(target.data) > 1){ #5
    all.comb.choice=as.matrix(combn(1:nrow(target.data),2))

    for(ch.n in 1:ncol(all.comb.choice)){ #3
      if(ch.n == 1){temp.decision=c()}
      # select two soccer games
      selected.games=target.data[c(which(as.numeric(rownames(target.data))==all.comb.choice[1,ch.n]),
                                   which(as.numeric(rownames(target.data))==all.comb.choice[2,ch.n])),]
    
      #ch.n=1
      ### joint decimal odds
      row.odds=c(selected.games[1,]$H.odd, selected.games[1,]$D.odd, selected.games[1,]$A.odd)
      col.odds=c(selected.games[2,]$H.odd, selected.games[2,]$D.odd, selected.games[2,]$A.odd)
      dec.odds=outer(row.odds, col.odds)
      rownames(dec.odds)=c(row.odds)
      colnames(dec.odds)=c(col.odds)
      
      ### estimated joint probability using implied probability
      row.pro=c(selected.games[1,]$H.win.prob, selected.games[1,]$D.win.prob, selected.games[1,]$A.win.prob)
      col.pro=c(selected.games[2,]$H.win.prob, selected.games[2,]$D.win.prob, selected.games[2,]$A.win.prob)
      est.prob=outer(row.pro, col.pro)
      rownames(est.prob)=c(row.odds)
      colnames(est.prob)=c(col.odds)
      
      ################################
      # calculate temp kelly criterion
      ################################
      # kelly criterion
      temp.criterion = kelly.criterion(dec.odds, est.prob)

      # test criterion
      #temp.criterion=as.matrix(kelly.row)%*%t(as.matrix(kelly.column))
      #temp.criterion=test.criterion(temp.output$dec.odds,temp.output$est.prob)
      
      #####################
      # choose combinations
      #####################
      if(max(temp.criterion) > 0){
        # select combinations with positive value criterion
        #sel_ind = which(temp.criterion > 0, arr.ind = TRUE)
        
        # select the best combination of odds
        sel_ind = which(temp.criterion == max(temp.criterion), arr.ind = TRUE)
        
        for(chosen.id in 1:nrow(sel_ind)){
          row.ind = sel_ind[chosen.id ,1]
          col.ind = sel_ind[chosen.id ,2]
          
          chosen.games = c()
          chosen.games$Time=unique(selected.games$Time)
          
          chosen.games$game.1.H.team=as.character(selected.games$H.team[1])
          chosen.games$game.1.A.team=as.character(selected.games$A.team[1])
          if(row.ind==1){chosen.games$game.1.odds=paste0("H.odd : ", selected.games$H.odd[1])}
          if(row.ind==2){chosen.games$game.1.odds=paste0("D.odd : ", selected.games$D.odd[1])}
          if(row.ind==3){chosen.games$game.1.odds=paste0("A.odd : ", selected.games$A.odd[1])}
          
          chosen.games$game.2.H.team=as.character(selected.games$H.team[2])
          chosen.games$game.2.A.team=as.character(selected.games$A.team[2])
          if(col.ind==1){chosen.games$game.2.odds=paste0("H.odd : ", selected.games$H.odd[2])}
          if(col.ind==2){chosen.games$game.2.odds=paste0("D.odd : ", selected.games$D.odd[2])}
          if(col.ind==3){chosen.games$game.2.odds=paste0("A.odd : ", selected.games$A.odd[2])}
          
          chosen.games$kelly.prop = temp.criterion[row.ind,col.ind]
          
          chosen.games = as_tibble(chosen.games)
          
          temp.decision = rbind(temp.decision, chosen.games)
        }
      }
    } #3
    
    if(!is.null(temp.decision)){
      # consider all combinations with positive value criterion
      # final.decision = rbind(final.decision, temp.decision)
      
      # consider only the best combination on the day
      final.decision = rbind(final.decision, temp.decision[which.max(temp.decision$kelly.prop),])
    }
  } #5
  
  print(paste0("[Decision] Rate : ", Rate, ", | ", round(n/N, digits=3)*100 , "% |"))
} #1
