################
# Data variables
################
# Time
# H.team 
# A.team 
# H.odd
# D.odd
# A.odd
# H.win.prob
# D.win.prob
# A.win.prob
# Result
# H.m.A.prob Draw
rm(list=ls())                         # Empty the workspace        #

#################
# Rcode directory
#################
setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")    
source("Functions.R")              # Functions

######
# Data
######
source("Real_Data.R")               # Real data

######################
# Descriptive analysis
######################
boxplot(analysis.data[,7:9])  # Boxplot of implied probability
summary(analysis.data[,7:9])  # summary of implied probability

table(analysis.data$Result)   # table

sum(analysis.data$Result=="H")/nrow(analysis.data)  # Empirical probability of home winning
sum(analysis.data$Result=="D")/nrow(analysis.data)  # Empirical probability of draw
sum(analysis.data$Result=="A")/nrow(analysis.data)  # Empirical probability of away winning

#################################
# Calculate Home.prob - Away.prob
analysis.data$H.m.A.prob=analysis.data$H.win.prob-analysis.data$A.win.prob

Linear_Model=lm(D.win.prob ~ I(H.m.A.prob^2), data=analysis.data) # fit linear regression with transformed x
Gam_Model=gam(D.win.prob ~ s(H.m.A.prob), family=gaussian, data=analysis.data) # fit gam model
Spline_Model=lm(D.win.prob ~ bs(analysis.data$H.m.A.prob,df=5), data=analysis.data) # fit cubic regression spline
Spline_Model_Fit=data.frame(analysis.data$H.m.A.prob, Spline_Model$fitted.values)
xaxis=seq(-1, 1, by=0.01)
plot(analysis.data$H.m.A.prob, analysis.data$D.win.prob)
lines(sort(analysis.data$H.m.A.prob), predict(Linear_Model, list(H.m.A.prob = sort(analysis.data$H.m.A.prob)), type = "response"), col=3, lwd=2)
lines(sort(analysis.data$H.m.A.prob), predict(Gam_Model, list(H.m.A.prob = sort(analysis.data$H.m.A.prob)), type = "response"), col="red", lwd=2)
lines(Spline_Model_Fit[order(Spline_Model_Fit[,1]),1], Spline_Model_Fit[order(Spline_Model_Fit[,1]),2], col="blue", lwd=2)
legend(-0.8, 0.45,c("Polynomial","Gam", "Kernel Smoothing"), lwd=c(2,2), col=c("green", "red","blue"))

### summary
summary(Gam_Model)
summary(Spline_Model)
summary(Linear_Model)

#######################
# Compare
#######################
expected_result=c()
for(i in 1:nrow(analysis.data)){
  temp=c(analysis.data$H.odd[i], analysis.data$D.odd[i], analysis.data$A.odd[i])
  expected_result[i]=which(temp==min(temp))[1]
  if(expected_result[i]==1){
    expected_result[i]="H"
  }
  if(expected_result[i]==2){
    expected_result[i]="D"
  }
  if(expected_result[i]==3){
    expected_result[i]="A"
  }
}

compare=c()
for(i in 1:length(expected_result)){
  if(expected_result[i]==analysis.data$Result[i]){
    compare[i]=1
  }else{compare[i] = 0}
}

correct_guess_data=analysis.data[which(compare==1),]
wrong_guess_data=analysis.data[which(compare==0),]

table(correct_guess_data$Result)
table(wrong_guess_data$Result)

boxplot(correct_guess_data[,7:9], ylim=c(0,1))  # Boxplot of implied probability
boxplot(wrong_guess_data[,7:9], ylim=c(0,1))  # Boxplot of implied probability
summary(correct_guess_data[,7:9])  # summary of implied probability
summary(wrong_guess_data[,7:9])  # summary of implied probability

sum(analysis.data$Result=="H")/nrow(analysis.data)  # Empirical probability of home winning
sum(analysis.data$Result=="D")/nrow(analysis.data)  # Empirical probability of draw
sum(analysis.data$Result=="A")/nrow(analysis.data)  # Empirical probability of away winning

####################################
# Y : 1 if the result is draw
#     0 otherwise
####################################
analysis.data$Draw=NA
analysis.data$Draw[which(analysis.data$Result=="D")]=1
analysis.data$Draw[which(analysis.data$Result!="D")]=0
analysis.data=as.data.frame(analysis.data)

Logistic_Model=glm(Draw ~ H.odd, data=analysis.data, family = binomial(link='logit'))
summary(Logistic_Model)
plot(analysis.data$H.odd, analysis.data$Draw)
curve(predict(Logistic_Model,data.frame(H.odd=x),type="response"), add=TRUE, col="red")

Logistic_Model=glm(Draw ~ A.odd, data=analysis.data, family = binomial(link='logit'))
summary(Logistic_Model)
plot(analysis.data$A.odd, analysis.data$Draw)
curve(predict(Logistic_Model,data.frame(A.odd=x),type="response"), add=TRUE, col="red")

Logistic_Model=glm(Draw ~ D.odd, data=analysis.data, family = binomial(link='logit'))
summary(Logistic_Model)
plot(analysis.data$D.odd, analysis.data$Draw)
curve(predict(Logistic_Model,data.frame(D.odd=x),type="response"), add=TRUE, col="red")


# kernel density for each result
head(analysis.data)
analysis.data$diff=analysis.data$H.post.win.prob-analysis.data$A.post.win.prob
NB <- NaiveBayes(x=analysis.data[,c(7:9,15)], grouping=as.factor(analysis.data[,c(10)]), usekernel=TRUE)
plot(NB, lwd=2)
analysis.data[,c(11:13,16)]
final.decision[-which(final.decision$kelly.prop<0.1),8]

head(final.decision)
NB <- NaiveBayes(x=final.decision[-which(final.decision$kelly.prop<0.25),8], grouping=as.factor(final.decision[-which(final.decision$kelly.prop<0.25),9]), usekernel=TRUE)
plot(NB, lwd=2)
NB <- NaiveBayes(x=final.decision[,8], grouping=as.factor(final.decision[,9]), usekernel=TRUE)
plot(NB, lwd=2)

## smooth logistic regression
sm.binomial(x=final.decision$kelly.prop, y=final.decision$Result, h=0.02, xlab="kelly.prop (h=0.02)")
sm.binomial(x=as.numeric(unlist(strsplit(as.character(final.decision$game.1.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision$game.1.odds), " : "))),by=2)]), y=final.decision$Result, h=0.4, xlab="kelly.prop (h=0.02)")
sm.binomial(x=as.numeric(unlist(strsplit(as.character(final.decision$game.2.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision$game.2.odds), " : "))),by=2)]), y=final.decision$Result, h=0.1, xlab="kelly.prop (h=0.02)")

