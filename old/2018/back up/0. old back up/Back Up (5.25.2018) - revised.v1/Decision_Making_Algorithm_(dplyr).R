################################################################
# Decision making algorithm + calculate the proportion to invest
################################################################
final.decision=c()
final.n=0
Dist.time = c(unique(analysis.data$Time))
N=length(Dist.time) # The total number of days
for(n in 1:N){ #1
  #n=1
  time = Dist.time[n]
  target.data = analysis.data %>% filter(Time == time)
  
  # If there are more than one game on the day, find the combinations.
  if(nrow(target.data) > 1){ #5
    all.comb.choice=as.matrix(combn(1:nrow(target.data),2))
    
    for(ch.n in 1:ncol(all.comb.choice)){ #3
      if(ch.n == 1){temp.decision=c()}
      # select two soccer games
      selected.games = target.data %>%
        filter(row_number() == all.comb.choice[1,ch.n] | row_number() == all.comb.choice[2,ch.n])
      
      #ch.n=1
      ### joint decimal odds
      row.odds = selected.games %>% 
        filter(row_number() == 1) %>% 
        select(H.odd, D.odd, A.odd) %>% unlist(use.names = FALSE)
      col.odds = selected.games %>% 
        filter(row_number() == 2) %>% 
        select(H.odd, D.odd, A.odd) %>% unlist(use.names = FALSE)
      
      dec.odds = outer(row.odds, col.odds)
      
      rownames(dec.odds) = c(row.odds)
      colnames(dec.odds) = c(col.odds)
      
      ### estimated joint probability using implied probability
      row.pro = selected.games %>% 
        filter(row_number() == 1) %>% 
        select(H.win.prob, D.win.prob, A.win.prob) %>% unlist(use.names = FALSE)
      col.pro = selected.games %>% 
        filter(row_number() == 2) %>% 
        select(H.win.prob, D.win.prob, A.win.prob) %>% unlist(use.names = FALSE)
      
      est.prob=outer(row.pro, col.pro)
      
      rownames(est.prob) = c(row.odds)
      colnames(est.prob) = c(col.odds)
      
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
        sel_ind = which(temp.criterion > 0, arr.ind = TRUE)
        #sel_ind = which(temp.criterion == max(temp.criterion), arr.ind = TRUE)
        
        for(chosen.id in 1:nrow(sel_ind)){
          row.ind = sel_ind[chosen.id ,1]
          col.ind = sel_ind[chosen.id ,2]
          
          chosen.games = c()
          chosen.games$Time=unique(selected.games$Time)
          chosen.games$game.1.H.team=selected.games %>% filter(row_number() == 1) %>% select(H.team) %>% as.character()
          chosen.games$game.1.A.team=selected.games %>% filter(row_number() == 1) %>% select(A.team) %>% as.character()
          if(row.ind==1){chosen.games$game.1.odds=paste0("H.odd : ", selected.games$H.odd[1])}
          if(row.ind==2){chosen.games$game.1.odds=paste0("D.odd : ", selected.games$D.odd[1])}
          if(row.ind==3){chosen.games$game.1.odds=paste0("A.odd : ", selected.games$A.odd[1])}
          
          chosen.games$game.2.H.team=selected.games %>% filter(row_number() == 2) %>% select(H.team) %>% as.character()
          chosen.games$game.2.A.team=selected.games %>% filter(row_number() == 2) %>% select(A.team) %>% as.character()
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
      #final.decision = rbind(final.decision, temp.decision)
      
      # consider only the best combination on the day
      final.decision = rbind(final.decision, temp.decision %>% filter(kelly.prop == max(kelly.prop)))
    }
  } #5
  
  print(paste0("Progress : ", round(n/N, digits=3)*100 , "%"))
} #1
