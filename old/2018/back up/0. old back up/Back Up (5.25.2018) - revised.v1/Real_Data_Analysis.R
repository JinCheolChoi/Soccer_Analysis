#####################
# Empty the workspace
#####################
rm(list=ls())

#################
# Rcode directory
#################
Directory = setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")    
source("Functions.R")              # Functions

####################
# Data save and load
####################
#save.image(paste0("Approximation_SPC.Rdata"))
#load(paste0("Approximation_SPC.Rdata"))

############
# Parameters
############
case = "Real_data"        # claims that this is a real data analysis
initial.bankrole = 1000     # the initial bankrole
Rates = c(0.5, 1, 1.5, 2)   # the fraction of the current bankroll to wager

###############
# Record values
###############
all.bankrole=list()
all.bankrole.n=0
all.win.or.loss=matrix(NA, 1, length(Rates))
all.win.or.loss.n=0
colnames(all.win.or.loss)=paste0("Rate : ", Rates)
rownames(all.win.or.loss)=paste0("nrep : ", 1)

######
# Data
######
source("Real_Data.R")                     # Real data
Date.list = unique(analysis.data$Time)

################
# Main Algorithm
################
for(Rate in Rates){
  all.bankrole.n=all.bankrole.n+1
  
  all.win.or.loss.n=all.win.or.loss.n+1
  ###############################
  # Run decision making algorithm
  ###############################
  source("Decision_Making_Algorithm.R")        # Generate simulation data
  
  ########################
  # apply decision to data
  ########################
  source("Apply_Decision_to_Data.R")           # Apply Decision to Data
  
  all.bankrole[[all.bankrole.n]]=matrix(NA, bankrole.n.length, 2)
  
  ### average winning rate
  all.win.or.loss[1, all.win.or.loss.n]=sum(final.decision$Result==1)/(sum(final.decision$Result==1)+sum(final.decision$Result==-1))
  
  all.bankrole[[all.bankrole.n]][,1] = bankrole$time
  all.bankrole[[all.bankrole.n]][,2] = bankrole$money
  
  print(paste0("Rate : ", Rate))
}

### outcome
head(analysis.data)   # data
head(final.decision)  # decision
head(all.bankrole)    # bankrole history

### visualize the result
plot(all.bankrole[[2]][,1], all.bankrole[[2]][,2], type='l', xaxt = "n")
axis.Date(side = 1, as.Date(all.bankrole[[2]][,1], origin = "1970-01-01", by="months"), format = "%d/%m/%Y")

### winning rate
all.win.or.loss

### filter final.decision
subset.1 = final.decision %>% 
  filter(row_number() <= nrow(final.decision)) %>% 
  filter(kelly.prop >= 0.27 & kelly.prop <= 0.30)

subset.2 = final.decision %>% 
  filter(row_number() <= nrow(final.decision)) %>% 
  filter(kelly.prop >= 0.4)


final.decision.filtered=rbind(subset.1,subset.2)

final.decision.filtered$game.1.odds=as.numeric(unlist(strsplit(as.character(final.decision.filtered$game.1.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision.filtered$game.1.odds), " : "))),by=2)])
final.decision.filtered$game.2.odds=as.numeric(unlist(strsplit(as.character(final.decision.filtered$game.2.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision.filtered$game.2.odds), " : "))),by=2)])

final.decision$game.1.odds=as.numeric(unlist(strsplit(as.character(final.decision$game.1.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision$game.1.odds), " : "))),by=2)])
final.decision$game.2.odds=as.numeric(unlist(strsplit(as.character(final.decision$game.2.odds), " : "))[seq(2,length(unlist(strsplit(as.character(final.decision$game.2.odds), " : "))),by=2)])

#final.decision.filtered=final.decision.filtered[-which(final.decision.filtered$game.1.odds*final.decision.filtered$game.2.odds>1.6),]


# winning rate
sum(final.decision.filtered[,9])/nrow(final.decision.filtered)

## probability of winning by kelly.prop
sm.binomial(x=final.decision$kelly.prop, y=final.decision$Result, h=0.02, xlab="kelly.prop")
sm.binomial(x=final.decision$game.1.odds*final.decision$game.2.odds, y=final.decision$Result, h=0.1, xlab="odd*odd")

sm.binomial(x=final.decision.filtered$kelly.prop, y=final.decision.filtered$Result, h=0.02, xlab="kelly.prop")
sm.binomial(x=final.decision.filtered$game.1.odds*final.decision.filtered$game.2.odds, y=final.decision.filtered$Result, h=0.05, xlab="odd*odd")

NB <- NaiveBayes(x=final.decision.filtered$game.1.odds*final.decision.filtered$game.2.odds, grouping=as.factor(final.decision.filtered$Result), usekernel=TRUE)
plot(NB, lwd=2)

## kelly.prop vs odd*odd plot
plot(final.decision$kelly.prop, final.decision$game.1.odds*final.decision$game.2.odds)
plot(final.decision.filtered$kelly.prop, as.numeric(final.decision.filtered$game.1.odds)*as.numeric(final.decision.filtered$game.2.odds))


