###########################
# decision making algorithm
###########################
# output : all.chosen.bet
#
#
Decision.time = sort(unique(analysis.data$Time))

# obtain the empirical winning probability 
all.chosen.bet = data.frame()
for(tn in 1:length(Decision.time)){
  #tn=3
  target.data = analysis.data[which(analysis.data$Time == Decision.time[tn]),]
  
  if(nrow(target.data) > 2){
    pos.comb = combn(1:nrow(target.data),2)
    
    for(comb.n in 1:ncol(pos.comb)){
      # generate matrix of joint odds
      row.odds=c(target.data[pos.comb[1,comb.n],]$H.odd, target.data[pos.comb[1,comb.n],]$D.odd, target.data[pos.comb[1,comb.n],]$A.odd)
      col.odds=c(target.data[pos.comb[2,comb.n],]$H.odd, target.data[pos.comb[2,comb.n],]$D.odd, target.data[pos.comb[2,comb.n],]$A.odd)
      joint.odds=outer(row.odds, col.odds)
      rownames(joint.odds)=c(row.odds)
      colnames(joint.odds)=c(col.odds)
      
      # generate matrix of result
      result.mat = matrix(0, 3, 3)
      result.mat[which(as.factor(c("H", "D", "A")) == target.data[pos.comb[1,comb.n],]$Result), which(as.factor(c("H", "D", "A")) == target.data[pos.comb[2,comb.n],]$Result)] = 1
      
      day.to.day.bet = data.frame(Time = rep(unique(target.data$Time), length(c(joint.odds))),
                                  H.team.1 = target.data$H.team[pos.comb[1, comb.n]],
                                  A.team.1 = target.data$A.team[pos.comb[1, comb.n]],
                                  H.team.2 = target.data$H.team[pos.comb[2, comb.n]],
                                  A.team.2 = target.data$A.team[pos.comb[2, comb.n]],
                                  Choose.1 = c(rep(c("H", "D", "A"), 3)),
                                  Choose.2 = c(rep("H", 3), rep("D", 3), rep("A", 3)),
                                  Odd.1 = c(rep(rownames(joint.odds), 3)),
                                  Odd.2 = c(rep(colnames(joint.odds)[1],3), rep(colnames(joint.odds)[2],3), rep(colnames(joint.odds)[3],3)),
                                  joint.odds = c(joint.odds),
                                  implied.joint.prob = c(c(target.data[pos.comb[1,comb.n],]$H.win.prob, target.data[pos.comb[1,comb.n],]$D.win.prob, target.data[pos.comb[1,comb.n],]$A.win.prob)%*%
                                                           t(c(target.data[pos.comb[2,comb.n],]$H.win.prob, target.data[pos.comb[2,comb.n],]$D.win.prob, target.data[pos.comb[2,comb.n],]$A.win.prob))),
                                  result.mat = c(result.mat))
      
      chosen.bet = day.to.day.bet[which(day.to.day.bet$joint.odds > min.odd & day.to.day.bet$joint.odds < max.odd),]
      
      all.chosen.bet = rbind(all.chosen.bet, chosen.bet)
    }
  }
  
  if(nrow(target.data) == 2){
    # generate matrix of joint odds
    row.odds=c(target.data[1,]$H.odd, target.data[1,]$D.odd, target.data[1,]$A.odd)
    col.odds=c(target.data[2,]$H.odd, target.data[2,]$D.odd, target.data[2,]$A.odd)
    joint.odds=outer(row.odds, col.odds)
    rownames(joint.odds)=c(row.odds)
    colnames(joint.odds)=c(col.odds)
    
    # generate matrix of result
    result.mat = matrix(0, 3, 3)
    result.mat[which(as.factor(c("H", "D", "A")) == target.data$Result[1]), which(as.factor(c("H", "D", "A")) == target.data$Result[2])] = 1

    day.to.day.bet = data.frame(Time = rep(unique(target.data$Time), length(c(joint.odds))),
                                H.team.1 = target.data$H.team[1],
                                A.team.1 = target.data$A.team[1],
                                H.team.2 = target.data$H.team[2],
                                A.team.2 = target.data$A.team[2],
                                Choose.1 = c(rep(c("H", "D", "A"), 3)),
                                Choose.2 = c(rep("H", 3), rep("D", 3), rep("A", 3)),
                                Odd.1 = c(rep(rownames(joint.odds), 3)),
                                Odd.2 = c(rep(colnames(joint.odds)[1],3), rep(colnames(joint.odds)[2],3), rep(colnames(joint.odds)[3],3)),
                                joint.odds = c(joint.odds),
                                implied.joint.prob = c(c(target.data[1,]$H.win.prob, target.data[1,]$D.win.prob, target.data[1,]$A.win.prob)%*%
                                                         t(c(target.data[2,]$H.win.prob, target.data[2,]$D.win.prob, target.data[2,]$A.win.prob))),
                                result.mat = c(result.mat))
    
    chosen.bet = day.to.day.bet[which(day.to.day.bet$joint.odds > min.odd & day.to.day.bet$joint.odds < max.odd),]
    
    all.chosen.bet = rbind(all.chosen.bet, chosen.bet)
  }
  
  print(paste0("Year : ", Year, ", Progress : ", tn/length(Decision.time)*100, "%"))
}


###################################
# generate the final.all.chosen.bet
###################################
# find all years that are considered for Empirical_Winning_Probability
target.year = c()
target.year.ind = 0
for(load.year in 1950:max(Years)){
  if(length(which(ls() == paste0("Logistic_Model.", load.year))) != 0){
    target.year.ind = target.year.ind + 1
    target.year[target.year.ind] = load.year
  }
}

# the key idea is taking the average of predicted probabilites in the past years
x.axis=seq(0, 1000, by=0.1)
# generate null predict.prob first
predict.prob = rep(0, length(x.axis))
for(load.year in target.year[which(target.year <= (Year - 1))]){
  #load.year = 2001
  Logistic_Model = get(paste0("Logistic_Model.", load.year))
  if(load.year == min(target.year)){
    predict.prob = predict(Logistic_Model, data.frame(joint.odds=x.axis), type="response")
  }
  if(load.year > min(target.year)){
    predict.prob = apply(cbind(predict.prob, predict(Logistic_Model, data.frame(joint.odds=x.axis),type="response")), 1, mean)
  }
}

##########################################
# decide the specific range of odds to bet
##########################################
#Logistic_Model = glm(result.mat ~ joint.odds, data=all.chosen.bet, family = binomial(link='logit'))
diff.prob = c(predict.prob - threshold.par/x.axis)

# if there is no bet that is made according to the criteria
if(length(x.axis[which(diff.prob > 0)]) == 0){
  final.all.chosen.bet = data.frame(
    Time=c(""),
    H.team.1=c(""),
    A.team.1=c(""),
    H.team.2=c(""),
    A.team.2=c(""),
    Choose.1=c(""),
    Choose.2=c(""),
    Odd.1=c(""),
    Odd.2=c(""),
    joint.odds=c(""),
    result.mat=c("")
  )
}

# if there are bets that are made according to the criteria
if(length(x.axis[which(diff.prob > 0)]) != 0){
  min.joint.odds = min(x.axis[which(diff.prob > 0)])
  max.joint.odds = max(x.axis[which(diff.prob > 0)])
  final.all.chosen.bet = all.chosen.bet[which(all.chosen.bet$joint.odds > min.joint.odds & all.chosen.bet$joint.odds < max.joint.odds),]
}

####################################
# calculate the predict prob of data
#
x.axis=final.all.chosen.bet$joint.odds
# generate null predict.prob first
predict.prob = rep(0, length(x.axis))
for(load.year in target.year[which(target.year <= (Year - 1))]){
  #load.year = 2001
  Logistic_Model = get(paste0("Logistic_Model.", load.year))
  if(load.year == min(target.year)){
    predict.prob = predict(Logistic_Model, data.frame(joint.odds=x.axis), type="response")
  }
  if(load.year > min(target.year)){
    predict.prob = apply(cbind(predict.prob, predict(Logistic_Model, data.frame(joint.odds=x.axis), type="response")), 1, mean)
  }
}

final.all.chosen.bet$predict.prob = predict.prob

###############################
# calculate the kelly criterion
#
#final.all.chosen.bet$kelly.criterion = (final.all.chosen.bet$predict.prob*(final.all.chosen.bet$joint.odds - kelly.tunning.par)-(1-final.all.chosen.bet$predict.prob))/(final.all.chosen.bet$joint.odds - kelly.tunning.par)
final.all.chosen.bet$kelly.criterion = (final.all.chosen.bet$implied.joint.prob*(final.all.chosen.bet$joint.odds - kelly.tunning.par)-(1-final.all.chosen.bet$implied.joint.prob))/(final.all.chosen.bet$joint.odds - kelly.tunning.par)

############################################
# choose picks with positive kelly criterion
final.all.chosen.bet = final.all.chosen.bet[which(final.all.chosen.bet$kelly.criterion > min.kelly),]

#final.all.chosen.bet=final.all.chosen.bet[which(final.all.chosen.bet$predict.prob > 0.24),]


#############################
# refine final.all.chosen.bet
if(Refine == "TRUE"){
  final.unique.time = unique(final.all.chosen.bet$Time)
  refine.final.all.chosen.bet = c()
  for(tn in 1:length(final.unique.time)){
    #tn=3
    target.final.all.chosen.bet = final.all.chosen.bet[which(final.all.chosen.bet$Time == final.unique.time[tn]),]
    if(nrow(target.final.all.chosen.bet) > 1){
      comb.list = paste(target.final.all.chosen.bet$H.team.1,
                        target.final.all.chosen.bet$A.team.1,
                        target.final.all.chosen.bet$H.team.2,
                        target.final.all.chosen.bet$A.team.2)
      
      unique.comb = unique(comb.list)
      for(i in 1:length(unique.comb)){
        #i=1
        to.compute.expected.gain=target.final.all.chosen.bet[which(comb.list == unique.comb[i]),]
        
        #refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, 
        #                                    to.compute.expected.gain[which.max((to.compute.expected.gain$joint.odds-1)*to.compute.expected.gain$kelly.criterion*to.compute.expected.gain$predict.prob-to.compute.expected.gain$kelly.criterion*(1-to.compute.expected.gain$predict.prob)),])
        
        refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, 
                                            to.compute.expected.gain[which.max(to.compute.expected.gain$joint.odds),])
        
        #refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, 
        #                                    to.compute.expected.gain[which.max(to.compute.expected.gain$kelly.criterion),])
        
  
      }
    }
    if(nrow(target.final.all.chosen.bet) == 1){
      refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, target.final.all.chosen.bet)
    }
  }
  
  final.all.chosen.bet = refine.final.all.chosen.bet
}

###############################
# save the final.all.chosen.bet
###############################
write.csv(final.all.chosen.bet, paste0(Directory,"/Decision/", League, "/", Year, ".csv"), row.names = FALSE)
