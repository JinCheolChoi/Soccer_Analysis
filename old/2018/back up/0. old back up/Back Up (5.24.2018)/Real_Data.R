#############
# Import Data
#############
real.data=read.csv("C:/Users/JinCheol Choi/Desktop/Soccer_Analysis/Real_Data/ALL_Analysis.csv")

###################################################
# Create a data for analysis by tailoring real data
###################################################
analysis.data=c()
analysis.data$Time=as.numeric(real.data$Date)
analysis.data$H.team=as.character(real.data$HomeTeam)
analysis.data$A.team=as.character(real.data$AwayTeam)
analysis.data$H.odd=as.numeric(as.character(real.data$Home.odds))
analysis.data$D.odd=as.numeric(as.character(real.data$Draw.odds))
analysis.data$A.odd=as.numeric(as.character(real.data$Away.odds))

analysis.data=as.data.frame(analysis.data)
analysis.data=est.prob.f(analysis.data)

analysis.data$Result=as.character(real.data$Full.time.result)


#################################
# Calculate posterior probability
#################################
analysis.data=post.prob(analysis.data)

analysis.data$Diff=analysis.data$H.post.win.prob-analysis.data$A.post.win.prob
head(analysis.data)

