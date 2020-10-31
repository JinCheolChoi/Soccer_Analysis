win.data=final.decision[which(final.decision$result.mat==1),]
lose.data=final.decision[which(final.decision$result.mat==0),]

win.data$joint.odds
lose.data$joint.odds
head(win.data)

win.data$Odd.1=as.numeric(as.character(win.data$Odd.1))
win.data$Odd.2=as.numeric(as.character(win.data$Odd.2))
lose.data$Odd.1=as.numeric(as.character(lose.data$Odd.1))
lose.data$Odd.2=as.numeric(as.character(lose.data$Odd.2))
summary(win.data$Odd.1)
summary(win.data$Odd.2)
summary(lose.data$Odd.1)
summary(lose.data$Odd.2)


hist(lose.data$predict.prob, breaks=100, col=rgb(1, 0.2, 0.2, 1))
hist(win.data$predict.prob, breaks=100, add=T, col=rgb(0.2, 0.2, 1, 1))
hist(lose.data$joint.odds, breaks=100, col=rgb(1, 0.2, 0.2, 1))
hist(win.data$joint.odds, breaks=100, add=T, col=rgb(0.2, 0.2, 1, 1))
hist(lose.data$kelly.criterion, breaks=100, col=rgb(1, 0.2, 0.2, 1))
hist(win.data$kelly.criterion, breaks=100, add=T, col=rgb(0.2, 0.2, 1, 1))
hist(lose.data$bet.money, breaks=100, col=rgb(1, 0.2, 0.2, 1))
hist(win.data$bet.money, breaks=100, add=T, col=rgb(0.2, 0.2, 1, 1))

test.x = seq(1, 5, by=0.01)
test.y = c()
for(i in 1:length(test.x)){
  lose.filtered.data=lose.data[which(lose.data$joint.odds < test.x[i]),]
  win.filtered.data=win.data[which(win.data$joint.odds < test.x[i]),]
  test.y[i]=nrow(win.filtered.data)/nrow(lose.filtered.data)-1/mean(win.filtered.data$joint.odds)
}

lose.filtered.data=lose.data[which(lose.data$joint.odds < 3),]
win.filtered.data=win.data[which(win.data$joint.odds < 3),]
nrow(win.filtered.data)/nrow(lose.filtered.data)-1/mean(win.filtered.data$joint.odds)


win.data

Logistic_Model = glm(result.mat ~ joint.odds, data=final.decision, family = binomial(link='logit'))
plot(final.decision$joint.odds, final.decision$result.mat)
curve(predict(Logistic_Model, data.frame(joint.odds=x),type="response"), add=TRUE, col="red")


################
# logistic model
################
Logistic_Model = glm(result.mat ~ kelly.criterion, data=final.decision, family = binomial(link='logit'))
plot(final.decision$kelly.criterion, final.decision$result.mat)
curve(predict(Logistic_Model, data.frame(kelly.criterion=x),type="response"), add=TRUE, col="red")
#x.axis=seq(0, 1000, by=0.1)
#lines(x.axis, 1/x.axis, col='blue')
#test=sm.binomial(x=final.all.chosen.bet$kelly.criterion, y=final.all.chosen.bet$result.mat, h=0.02, xlab="kelly.prop (h=0.02)")

lines(final.all.chosen.bet$kelly.criterion[order(final.all.chosen.bet$kelly.criterion)] ,predict(Logistic_Model, data.frame(kelly.criterion=final.all.chosen.bet$kelly.criterion[order(final.all.chosen.bet$kelly.criterion)]), type="response"))

######
# plot
######
#plot(x.axis, predict.prob, type="l", col="red", ylim=c(0, 1), xlim=c(min.odd, max(final.decision$joint.odds)))
#lines(x.axis, threshold.par/x.axis, col='blue')

number.bet=c()
for(ind in 1:length(unique(final.decision$Time))){
  number.bet[ind]=nrow(final.decision[final.decision$Time==unique(final.decision$Time)[ind],])
} 
unique(final.decision$Time)[which(number.bet>100)]
number.bet[which(number.bet>100)]

# the number of bets on each day
number.bet

# average winning rate
mean(final.decision$result.mat)

# average joint odds
mean(final.decision$joint.odds)

hist(final.decision$joint.odds, breaks=50)







### refine final.all.chosen.bet
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
      
      refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, 
                                          to.compute.expected.gain[which.max((to.compute.expected.gain$joint.odds-1)*to.compute.expected.gain$kelly.criterion*to.compute.expected.gain$predict.prob-to.compute.expected.gain$kelly.criterion*(1-to.compute.expected.gain$predict.prob)),])
    }
  }
  if(nrow(target.final.all.chosen.bet) == 1){
    refine.final.all.chosen.bet = rbind(refine.final.all.chosen.bet, target.final.all.chosen.bet)
  }
}

final.all.chosen.bet=refine.final.all.chosen.bet

