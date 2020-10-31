################################################################
# Decision making algorithm + calculate the proportion to invest
################################################################
final.decision=c()
final.n=0
N=length(c(unique(analysis.data$Time))) # The total number of days
process.count=0

for(n in c(unique(analysis.data$Time))){ #1
  process.count=process.count+1
  #n=3
  target.index=which(analysis.data$Time==n)
  target.data=analysis.data[target.index,]
  
  ### compare selected criterion
  remaining.target.index=target.index
  ind=0
  while(ind==0){ #2
    ### all.combination.of.two.choice
    if(length(remaining.target.index) == 1){ # If there is only one game on the day, let's skip it.
      ind=1
    }
    
    if(length(remaining.target.index) > 1){ #5
      
      all.comb.choice=as.matrix(combn(remaining.target.index,2))
      for(ch.n in 1:ncol(all.comb.choice)){ #3
        if(ch.n==1){temp.decision=c();kelly.prop=0;selected.index=c()}
        selected.games=target.data[c(which(as.numeric(rownames(target.data))==all.comb.choice[1,ch.n]),
                                     which(as.numeric(rownames(target.data))==all.comb.choice[2,ch.n])),]
        
        #ch.n=1
        ### joint decimal odds
        x1=c(selected.games[1,]$H.odd, selected.games[1,]$D.odd, selected.games[1,]$A.odd)
        x2=c(selected.games[2,]$H.odd, selected.games[2,]$D.odd, selected.games[2,]$A.odd)
        dec.odds=outer(x1, x2)
        rownames(dec.odds)=c(x1)
        colnames(dec.odds)=c(x2)
        
        ### estimated joint probability using implied probability
        row.pro=c(selected.games[1,]$H.win.prob, selected.games[1,]$D.win.prob, selected.games[1,]$A.win.prob)
        col.pro=c(selected.games[2,]$H.win.prob, selected.games[2,]$D.win.prob, selected.games[2,]$A.win.prob)
        est.prob=outer(row.pro, col.pro)
        rownames(est.prob)=c(x1)
        colnames(est.prob)=c(x2)
        
        ### estimated joint probability using posterior probability
        #row.pro=c(selected.games[1,]$H.post.win.prob, selected.games[1,]$D.post.win.prob, selected.games[1,]$A.post.win.prob)
        #col.pro=c(selected.games[2,]$H.post.win.prob, selected.games[2,]$D.post.win.prob, selected.games[2,]$A.post.win.prob)
        #if(abs(row.pro[1]-row.pro[3])<0.5){row.pro=c(1/3,1/3,1/3)}
        #if(abs(col.pro[1]-col.pro[3])<0.5){row.pro=c(1/3,1/3,1/3)}
        #est.prob=outer(row.pro, col.pro)
        #rownames(est.prob)=c(x1)
        #colnames(est.prob)=c(x2)
        
        temp.output=c()
        temp.output$dec.odds=dec.odds
        temp.output$est.prob=est.prob
        temp.output$pred.prof=dec.odds*est.prob
        
        #kelly.row=c(row.pro[1]/(x1[1])-(1-row.pro)[1]/(x1[1]),row.pro[2]-(1-row.pro)[2]/(x1[2]),row.pro[3]/(x1[3])-(1-row.pro)[3]/(x1[3]))
        #kelly.column=c(col.pro[1]/(x2[3])-(1-col.pro)[1]/sqrt(x2[1]),col.pro[2]/(x2[3])-(1-col.pro)[2]/(x2[2]),col.pro[3]/(x2[3])-(1-col.pro)[3]/(x2[3]))
        
        #kelly.row=row.pro/x1-((1-row.pro)/x1)
        #kelly.column=col.pro/x2-((1-col.pro)/x2)
        
        ###########################################
        # calculate temp daily criterion
        # kelly criterion
        temp.criterion=kelly.criterion(temp.output$dec.odds,temp.output$est.prob)
        # test criterion
        #temp.criterion=as.matrix(kelly.row)%*%t(as.matrix(kelly.column))
        #temp.criterion=test.criterion(temp.output$dec.odds,temp.output$est.prob)
        
        # max.proportion
        temp.max.prop=max(temp.criterion)
        if(kelly.prop < temp.max.prop){ #4
          #print(paste0(ch.n))
          kelly.prop=temp.max.prop
          
          ## find the element with max kelly.prop
          row.ind=c()
          col.ind=c()
          for(row.n in 1:nrow(temp.criterion)){
            for(col.n in 1:ncol(temp.criterion)){
              if(temp.criterion[row.n,col.n]==max(temp.criterion)){
                row.ind=row.n
                col.ind=col.n
              }
            }
          }
          
          temp.decision$Time=unique(selected.games$Time)
          temp.decision$game.1.H.team=as.character(selected.games$H.team[1])
          temp.decision$game.1.A.team=as.character(selected.games$A.team[1])
          if(row.ind==1){temp.decision$game.1.odds=paste0("H.odd : ",selected.games$H.odd[1])}
          if(row.ind==2){temp.decision$game.1.odds=paste0("D.odd : ",selected.games$D.odd[1])}
          if(row.ind==3){temp.decision$game.1.odds=paste0("A.odd : ",selected.games$A.odd[1])}
          
          temp.decision$game.2.H.team=as.character(selected.games$H.team[2])
          temp.decision$game.2.A.team=as.character(selected.games$A.team[2])
          if(col.ind==1){temp.decision$game.2.odds=paste0("H.odd : ",selected.games$H.odd[2])}
          if(col.ind==2){temp.decision$game.2.odds=paste0("D.odd : ",selected.games$D.odd[2])}
          if(col.ind==3){temp.decision$game.2.odds=paste0("A.odd : ",selected.games$A.odd[2])}
          
          temp.decision$kelly.prop=kelly.prop
          selected.index=all.comb.choice[,ch.n]
        } #4
      } #3
      
      #target.data[selected.index,]
      #as.data.frame(temp.decision)
      
      ### eliminate considered target indices
      if(is.null(selected.index)){ # if 
        ind=1
      }else{
        remaining.target.index=remaining.target.index[-c(which(remaining.target.index==selected.index[1]),
                                                         which(remaining.target.index==selected.index[2]))]
        final.decision=rbind(final.decision, as.data.frame(temp.decision))
      }
      
      if(length(remaining.target.index)==0){ # when the number of games is even, no game remains at the end
        ind=1
      }
      
      if(length(remaining.target.index)==1){ # when the number of games is odd, one game remains at the end
        ind=1
      }
      
    } #5
    
  } #2
  
  print(paste0("Progress : ", round(process.count/N, digits = 3)*100 , "%"))
} #1
