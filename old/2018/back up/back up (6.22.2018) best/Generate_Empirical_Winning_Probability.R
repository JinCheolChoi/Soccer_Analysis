#####################
# Empty the workspace
#####################
rm(list=ls())

###################
# Parameter setting
###################
Directory = setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")
League = "England_Premium"
#--- available league
# 1. England_Premium
# 2. World_Cup

###################
# Parameter setting
###################
Years = 2008:2018
min.odd = 0
max.odd = 1000000000000
# England_Premium : Years = 2008:2018
# World_Cup       : Years = c(1998, 2002, 2006, 2010, 2014)

#################
# Rcode directory
#################
# import fundamental functions
source("Functions.R")
source("Apply_Decision_to_Data.R")

##################################################
# Data save and load empirical winning probability
##################################################
# the empirical winning probabilities are saved in Logistic_Model.1999~2018
#save.image(paste0("Empirical_winning_probability.Rdata"))
#load(paste0("Empirical_winning_probability.Rdata"))

###########################
# Decision_Making_Algorithm
###########################
for(Year in Years){
  # Year=1998
  # specify the directory
  Directory = setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")
  
  # import data
  source("Real_Data.R")
  
  # Decision_Making_Algorithm
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
                                    result.mat = c(result.mat)
                                    )
        
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
                                  result.mat = c(result.mat))
      
      chosen.bet = day.to.day.bet[which(day.to.day.bet$joint.odds > min.odd & day.to.day.bet$joint.odds < max.odd),]
      
      all.chosen.bet = rbind(all.chosen.bet, chosen.bet)
    }
    
    print(paste0("Year : ", Year, ", Progress : ", tn/length(Decision.time)*100, "%"))
  }
  
  # assign logistic model to Logistic_Model.Year
  assign(paste0("Logistic_Model.", Year), glm(result.mat ~ joint.odds, data=all.chosen.bet, family = binomial(link='logit')))
}

### save all logistic models
rm(all.chosen.bet, analysis.data, chosen.bet, day.to.day.bet, joint.odds, pos.comb, real.data, result.mat, target.data, Year, Years, max.odd, min.odd, row.odds, tn, col.odds, comb.n, Decision.time)
save.image(paste0(Directory,"/Empirical_Winning_Probability/", League, "/Empirical_winning_probability.Rdata"))

