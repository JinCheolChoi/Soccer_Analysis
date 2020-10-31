rm(list=ls())                         # Empty the workspace        #

#################
# Rcode directory
#################
setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/")    
source("Functions.R")              # Functions

###############
# Save and Load
###############
#setwd("C:/Users/JinCheol Choi/Desktop/Soccer_Betting/Rdata")  
#save.image(paste0("Simulation.Rdata"))
#load(paste0("Simulation.Rdata"))

################
# Parameter
#################
### parameters
case = "Simulation"            # claims that this is a simulation analysis
n.year=1                       # the number of years
ave.n.game.d=4                 # the average number of games per day
Rates=c(0.5)                   # rates
#Rates=seq(0.1,1,by=0.1)       # rates
Nrep=1                         # the number of simulations per rate
initial.bankrole=1000          # the initial bankrole
bankrole.n.length=(365*n.year) # the backrole length of each rate parameter

### record values
all.bankrole=list()
all.bankrole.n=0
all.win.or.loss=matrix(NA, Nrep, length(Rates))
all.win.or.loss.n=0
colnames(all.win.or.loss)=paste0("Rate : ", Rates)
rownames(all.win.or.loss)=paste0("nrep : ", (1:Nrep))

for(Rate in Rates){
  all.bankrole.n=all.bankrole.n+1
  all.bankrole[[all.bankrole.n]]=matrix(NA,bankrole.n.length,Nrep)
  
  all.win.or.loss.n=all.win.or.loss.n+1
  
  for(nrep in 1:Nrep){
    ######
    # Data
    ######
    source("Simulation_Data_Generator.R")        # Generate simulation data
    
    ###############################
    # Run decision making algorithm
    ###############################
    source("Decision_Making_Algorithm.R")        # Decision Making Algorithm
    
    ########################
    # apply decision to data
    ########################
    source("Apply_Decision_to_Data.R")           # Apply Decision to Data

    ### average winning rate
    all.win.or.loss[nrep, all.win.or.loss.n]=sum(win.or.loss)/length(win.or.loss)
    
    all.bankrole[[all.bankrole.n]][,nrep]=bankrole
    
    print(paste0("Rate : ", Rate, ", nrep : ", nrep))
  }
}

### outcome
head(final.decision)
head(analysis.data)
head(all.bankrole)

plot(all.bankrole[[1]], type='l')

### calculate mean, standard deviation
mean.mat=matrix(NA,bankrole.n.length,length(Rates))
sd.mat=matrix(NA,bankrole.n.length,length(Rates))
for(i in 1:length(Rates)){
  for(j in 1:bankrole.n.length){
    mean.mat[j,i]=mean(all.bankrole[[i]][j,])
    sd.mat[j,i]=sd(all.bankrole[[i]][j,])
  }
}

### visualize the result
conf.plot(2, Nrep)

### winning rate
all.win.or.loss
