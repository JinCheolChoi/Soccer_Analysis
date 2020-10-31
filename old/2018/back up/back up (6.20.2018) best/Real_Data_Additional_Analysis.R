Time = sort(unique(analysis.data$Time))

# obtain the empirical winning probability 
empr.dis = data.frame()
for(tn in 1:length(Time)){
  target.data = analysis.data[which(analysis.data$Time == Time[tn]),]
  
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
      result.mat[which(as.factor(c("H", "D", "A")) == target.data$Result[1]), which(as.factor(c("H", "D", "A")) == target.data$Result[2])] = 1
      
      empr.dis = rbind(empr.dis, cbind(target.data$Time, c(joint.odds), c(result.mat), log(c(joint.odds))))
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
    
    empr.dis = rbind(empr.dis, cbind(target.data$Time, c(joint.odds), c(result.mat), log(c(joint.odds))))
  }
  
  print(paste0("Progress : ", tn/length(Time)*100, "%"))
}

colnames(empr.dis) = c("Time", "joint.odds", "result", "log.joint.odds")

# joint.odds
Logistic_Model = glm(result ~ joint.odds, data=empr.dis, family = binomial(link='logit'))
summary(Logistic_Model)
plot(empr.dis$joint.odds, empr.dis$result)
curve(predict(Logistic_Model, data.frame(joint.odds=x),type="response"), add=TRUE, col="red")
x.axis=seq(0, 1000, by=0.1)
lines(x.axis, 1/x.axis, col='blue')

# smooth logistic regression
#sm.binomial(x=empr.dis$joint.odds, y=empr.dis$result, h=60, xlab="joint.odds (h=100)")
#lines(x.axis, 1/x.axis, col='blue')

# 
thresh = seq(0, max(empr.dis$joint.odds), by = 0.1)
min.max.thresh = c(min(thresh[which(predict(Logistic_Model, data.frame(joint.odds=c(thresh)), type="response") > 1/thresh)]),
                 max(thresh[which(predict(Logistic_Model, data.frame(joint.odds=c(thresh)), type="response") > 1/thresh)]))

gain=c()
for(n in 1:500){
  
  #min.max.thresh[2]=seq(100, 140, length=500)[n]
  gain[n]=sum(empr.dis$joint.odds[which(empr.dis$joint.odds > min(min.max.thresh) & empr.dis$joint.odds < max(min.max.thresh))]*
                            empr.dis$result[which(empr.dis$joint.odds > min(min.max.thresh) & empr.dis$joint.odds < max(min.max.thresh))])-
    length(empr.dis$joint.odds[which(empr.dis$joint.odds > min(min.max.thresh) & empr.dis$joint.odds < max(min.max.thresh))])
  
}

plot(gain, type='l')
seq(8.3, 12, by=1/500)[which.max(gain)]

#######################
sum(empr.dis$joint.odds*empr.dis$result)*100-
nrow(empr.dis)*100

thresh.input = 11.2
sum(empr.dis$joint.odds[which(empr.dis$joint.odds > thresh.input)]*empr.dis$result[which(empr.dis$joint.odds > thresh.input)])*100-
nrow(empr.dis[which(empr.dis$joint.odds > thresh.input),])*100

thresh[which(predict(Logistic_Model, data.frame(joint.odds=c(thresh)), type="response") > 1/thresh)]


thresh[which.max(net.gain.plot)]

#
nrep = 1000
empr.dis = data.frame()
for(prog.ind in 1:nrep){
  # choose two games
  target.data = analysis.data[sample(1:nrow(analysis.data), 2, replace = FALSE),]
  
  # generate matrix of joint odds
  row.odds=c(target.data[1,]$H.odd, target.data[1,]$D.odd, target.data[1,]$A.odd)
  col.odds=c(target.data[2,]$H.odd, target.data[2,]$D.odd, target.data[2,]$A.odd)
  joint.odds=outer(row.odds, col.odds)
  rownames(joint.odds)=c(row.odds)
  colnames(joint.odds)=c(col.odds)
  
  # generate matrix of result
  result.mat = matrix(0, 3, 3)
  result.mat[which(as.factor(c("H", "D", "A")) == target.data$Result[1]), which(as.factor(c("H", "D", "A")) == target.data$Result[2])] = 1
  
  empr.dis = rbind(empr.dis, cbind(c(joint.odds), c(result.mat)))
  
  print(paste0("Progress : ", prog.ind/nrep*100, "%"))
}

colnames(empr.dis) = c("joint.odds", "result")

plot(log(empr.dis$joint.odds), empr.dis$result)

empr.dis$joint.odds = log(empr.dis$joint.odds)
sm.binomial(x=empr.dis$joint.odds, y=empr.dis$result, h=1, xlab="kelly.prop (h=0.02)")


Logistic_Model = glm(result ~ joint.odds, data=empr.dis, family = binomial(link='logit'))
summary(Logistic_Model)
plot(empr.dis$joint.odds, empr.dis$result)
curve(predict(Logistic_Model, data.frame(joint.odds=x),type="response"), add=TRUE, col="red")
lines(empr.dis$joint.odds, 1/empr.dis$joint.odds, type='o',col="blue")

