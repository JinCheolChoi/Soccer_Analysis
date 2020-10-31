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

######################################
# import empirical winning probability
######################################
load(paste0(Directory,"/Empirical_Winning_Probability/", League, "/Empirical_winning_probability.Rdata"))

###################
# Parameter setting
###################
Years = c(2018)
#-------------------------------------------------------#
# England_Premium : Years = 2009:2018                   #
# World_Cup       : Years = c(2002, 2006, 2010, 2014)   #
#-------------------------------------------------------#
Type = "Fixed"
#------------------#
# "Kelly", "Fixed" #
#------------------#
Refine = "FALSE"
initial.bankrole = 100000      # the initial bankrole
#ind.bet.money = 100          # the initial amount of money per each bet
min.odd = 0                   # minimum joint odds to consider
max.odd = 1000000             # maximum joint odds to consider
threshold.par = 1             # (joint.odds > threshold.par/probability)
kelly.tunning.par = -1         # 1 is used for the normal kelly criterion

#-------------------------------- Parameter settings ---------------------------------#
#-------------------------------------------------------------------------------------#
#|                  | Refine | min.odd | max.odd | threshold.par | kelly.tunning.par |#                       #
#| England_Premium  |        |     0   | 1000000 |               |                   |#
#| World_Cup        | TRUE   |     0   | 1000000 |         1     |        -1         |#
#-------------------------------------------------------------------------------------#

#################
# Rcode directory
#################
# import fundamental functions
source("Functions.R")
source("Apply_Decision_to_Data.R")

###########################
# Decision_Making_Algorithm
###########################
for(Year in Years){
  # Year=2002
  
  # import data
  source("Real_Data.R")
  
  # Decision_Making_Algorithm
  source("Decision_Making_Algorithm.R")
}

########################
# Apply_Decision_to_Data
########################
# Create values
final.bankrole = c()
time.merge = c()
bankrole.merge = c()
final.decision = c()
for(Year in Years){
  #Year=2008
  #---------------------
  # import decision data
  #---------------------
  final.all.chosen.bet = read.csv(paste0(Directory,"/Decision/", League, "/", Year, ".csv"))
  
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
  if(is.na(final.all.chosen.bet$Time)[1] == FALSE){
    
    ind.bet.money = 100
    if(ind.bet.money > 5000){ind.bet.money = 5000}
    
    temp.bankrole = apply.decision(final.all.chosen.bet, initial.bankrole, ind.bet.money)
    time.merge = c(time.merge, temp.bankrole$Time)
    bankrole.merge = c(bankrole.merge, temp.bankrole$capital)
    
    #---------------------------
    # save updated decision data
    #---------------------------
    final.all.chosen.bet$bet.money = temp.bankrole$Ind.bet
    write.csv(final.all.chosen.bet, paste0(Directory,"/Decision/", League, "/", Year, ".csv"), row.names = FALSE)

    # save all decisions that are made
    final.decision = rbind(final.decision, final.all.chosen.bet)
  }
}

# merge time and capital into a single value
final.bankrole$Time = as.Date(time.merge, origin = "1970-01-01")
final.bankrole$capital = bankrole.merge
final.bankrole$Time[1]
final.decision[which(final.decision$Time==unique(final.decision$Time)[1]),]

######
# plot
######
if(League == "England_Premium"){
  plot(final.bankrole$Time, final.bankrole$capital, type = "o")
}
if(League == "World_Cup"){
  plot(final.bankrole$capital, type = "o")
}
final.bankrole$capital[length(final.bankrole$capital)]


mean(final.decision$joint.odds)-1/mean(final.decision$result.mat)


