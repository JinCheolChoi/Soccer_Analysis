#####################
# Empty the workspace
#####################
rm(list=ls())

###################
# Parameter setting
###################
Years = 2006:2018
initial.bankrole = 10000     # the initial bankrole
ind.bet.money = 100
min.odd = 30
max.odd = 100
threshold.par = 2.5


######################################
# import empirical winning probability
######################################
# England Premium
load(paste0("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/Empirical_winning_probability.Rdata"))

#################
# Rcode directory
#################
# import fundamental functions
Directory = setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")
source("Functions.R")
source("Apply_Decision_to_Data.R")


##################################################
# Data save and load empirical winning probability
##################################################
# the empirical winning probabilities are saved in Logistic_Model.1999~2018
#save.image(paste0("Empirical_winning_probability.Rdata"))
#load(paste0("Empirical_winning_probability.Rdata"))



###############
# Create values
###############
final.bankrole = c()
time.merge = c()
bankrole.merge = c()
final.decision = c()
for(Year in Years){
  
  ind.bet.money = ind.bet.money + 50 * (which(Years == Year)-1)
  # specify the directory
  Directory = setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")
  source("Real_Data.R")
  
  source("Decision_Making_Algorithm.R")
  
  if(Year == Years[1]){
    initial.bankrole = initial.bankrole
  }
  
  if(Year > Years[1]){
    if(length(bankrole.merge) == 0){
      initial.bankrole = initial.bankrole
    }else{
      initial.bankrole = bankrole.merge[length(bankrole.merge)] 
      }
  }
  
  # bet according to the decision
  if(length(x.axis[which(diff.prob > 0)]) != 0){
    temp.bankrole = apply.decision(final.all.chosen.bet, initial.bankrole)
    time.merge = c(time.merge, temp.bankrole$Time)
    bankrole.merge = c(bankrole.merge, temp.bankrole$capital)
    
    # save all decisions that are made
    final.decision = rbind(final.decision, final.all.chosen.bet)
  }
}

# merge time and capital into a single value
final.bankrole$Time = as.Date(time.merge, origin = "1970-01-01")
final.bankrole$capital = bankrole.merge

# plot
plot(final.bankrole$Time, final.bankrole$capital, type = "o")


################
# logistic model
################
#Logistic_Model = glm(result.mat ~ joint.odds, data=all.chosen.bet, family = binomial(link='logit'))
#plot(all.chosen.bet$joint.odds, all.chosen.bet$result.mat)
#curve(predict(Logistic_Model, data.frame(joint.odds=x),type="response"), add=TRUE, col="red")
#x.axis=seq(0, 1000, by=0.1)
#lines(x.axis, 1/x.axis, col='blue')
######
# plot
######
#plot(x.axis, predict.prob, type="l", col="red", ylim=c(0, 1), xlim=c(min.odd, max.odd))
#lines(x.axis, threshold.par/x.axis, col='blue')

number.bet=c()
for(test in 1:length(unique(final.decision$Time))){
  number.bet[test]=nrow(final.decision[final.decision$Time==unique(final.decision$Time)[test],])
} 
unique(final.decision$Time)[which(number.bet>100)]
number.bet[which(number.bet>100)]

# the number of bets on each day
number.bet

# average winning rate
mean(final.decision$result.mat)

# average joint odds
mean(final.decision$joint.odds)




final.all.chosen.bet=all.chosen.bet[which(all.chosen.bet$joint.odds > min.odd + 2 & all.chosen.bet$joint.odds < max.odd -8),]
final.all.chosen.bet

write.csv(final.all.chosen.bet, paste0(Directory,"/Decision/", Year, ".csv"), row.names = FALSE)
