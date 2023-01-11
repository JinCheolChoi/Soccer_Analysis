#********************
#
# empty the workspace
#
#********************
rm(list=ls())

#***************
#
# set parameters
#
#***************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
CODE.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rcodes/"
CODE.dir.3="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rcodes/Supplementary/"

data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis//Data/Game_results/"
# data.dir.1="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Data/Game_results/"

rdata.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/"

Years=c(1998:2022)

Countries=c(
  "england",
  "spain",
  "italy",
  "netherlands",
  "germany",
  # "china",
  # "japan",
  "turkey"
)
# england : "premier-league", "championship", "league-one", "league-two"
# spain : "laliga"
# italy : "serie-a"
# netherlands : "eerste-divisie"
# germany : "3-liga"
# china : "super-league"
# japan : "j1-league", "j2-league"
# turkey : "super-lig
Leagues=c(
  "premier-league",
  "championship",
  "league-one",
  "league-two",
  "laliga",
  "serie-a",
  "eerste-divisie",
  "3-liga",
  "bundesliga",
  # "super-league",
  # "j1-league",
  # "j2-league",
  "super-lig"
)
source(paste0(CODE.dir.1, "Functions.R"))
source(paste0(CODE.dir.3, "SA_Functions.R"))

lapply(c("data.table",
         "rvest",
         "stringr",
         "dplyr",
         "magrittr",
         "RSelenium",
         
         "profvis",
         "ggplot2",
         "readr", # readr::parse_number``
         "reshape2"), # melt() and dcast()
       checkpackages)


#*******************************
#
# Extract_Data_Game_Results ----
#
#*******************************
# source(paste0(CODE.dir.2, "Extract_Data_Game_Results.R"))


#*******************************
#
# Calculate expected profit ----
#
#*******************************
source(paste0(CODE.dir.2, "Calculate_Expected_Profit.R"))

#****************
#
# Simulation ----
#
#****************
# Parameters=Filtered_Results[Profit==max(Profit)]
Filtered_Results=Combined_Results[order(Avg_Profit, decreasing=T), ] %>% 
  filter(
    # Odds_Interval==0.1 &
    Denominator_N>50 &
    # Result=="Home"&
    # LO>=5.01&
    # HO==11.01&
    # Odds_Interval==5&
    Year>3000 &
      # Profit>0
      # Result%in%c("Home", "Draw", "Away")
      Kelly>0
    # Avg_Profit>0.1
    # Win_Prob>0
  )
Parameters=Filtered_Results
qplot(Filtered_Results$LO,
      Filtered_Results$Profit,
      col=Filtered_Results$Result)
Sim_Data=c()
for(Parameter_Ind in 1:nrow(Parameters)){
  Sim_Data=rbind(Sim_Data,
                 All_Years_Data[eval(parse(text=paste0(Parameters[Parameter_Ind,]$Result, "_Odds")))>=round(Parameters[Parameter_Ind,]$LO, 2) &
                                  eval(parse(text=paste0(Parameters[Parameter_Ind,]$Result, "_Odds")))<round(Parameters[Parameter_Ind,]$HO, 2) &
                                  Season_Year>=0&
                                  Country%in%c("england", "turkey", "germany", "netherlands", "spain"), ])
}


# Sim_Data=All_Years_Data[(Home_Odds>=12|Draw_Odds>=12|Away_Odds>=12) &
#                           Season_Year>=0&
#                           Country%in%c("england", "turkey", "germany", "netherlands", "spain"), ]
Sim_Data=Sim_Data[order(Season_Year, Date),]
# Sim_Data=Sim_Data[Season_Year>="2015-01-01",]
Principal=1
Balances=Balance=Principal
Winnig_Games=c()
Savings=0
Saving_Diffs=c()
# count the number of games to bet on the same day
Sim_Data[, Count:=.N, by="Date"]
table(Sim_Data[, Count])
# Sim_Data=Sim_Data[Count==1, ]
#
for(Ind in 1:nrow(Sim_Data)){
  # Ind=1
  Saving_Diff=0
  Saving_Added=0
  Saving_Subtracted=0
  if(Ind>1 & Balance<Principal){
    Balance=Balance+Principal
  }
  if(Balance>=Inf){
    Saving_Added=50000
    Balance=Balance-Saving_Added
  }
  if(Saving_Added==0 & Balance<100000 & tail(Savings, 1)>0){
    Saving_Subtracted=min(tail(Savings, 1), 10000)
    Balance=Balance+Saving_Subtracted
  }
  {
    # Bet=Balance*Parameters$Kelly*0.9
    Temp_Sim_Data=Sim_Data[Ind,
                           c("Home_Odds",
                             "Draw_Odds",
                             "Away_Odds")]
    Temp_Parameter_Ind=unlist(sapply(1:nrow(Parameters),
                                     function(x){
                                       y=unlist(Temp_Sim_Data)
                                       target_odds=y[paste0(Parameters[x, ][["Result"]], "_Odds")]
                                       if(length(y[target_odds>=round(Parameters[x, ][["LO"]], 2) & target_odds<round(Parameters[x, ][["HO"]], 2)])>0){x}
                                     }))
    
    if(length(Temp_Parameter_Ind)>=2){
      print(paste0("Ind : ", Ind, " / length(Temp_Parameter_Ind) from ", length(Temp_Parameter_Ind), " to 1"))
      Target_Orders=Parameters[Temp_Parameter_Ind, ][["Result"]]
      Target_Odds=unlist(Temp_Sim_Data)[which(names(Temp_Sim_Data)%in%paste0(Target_Orders, "_Odds"))]
      Temp_Parameter_Ind=Temp_Parameter_Ind[which(Target_Orders==unlist(strsplit(names(which.max(Target_Odds)), "_"))[1])]
    }
    
    Parameters_Temp=Parameters[Temp_Parameter_Ind, ]
    
    # Bet=min(round(Balance*0.05, 2),
    #         round(49500/Sim_Data[Ind,
    #                              eval(parse(text=paste0(Parameters_Temp[["Result"]], "_Odds")))], 2))
    Bet=min(round(Balance*Parameters_Temp[["Kelly"]], 2),
            round(49500/Sim_Data[Ind,
                                 eval(parse(text=paste0(Parameters_Temp[["Result"]], "_Odds")))], 2))
    
    Balance=Balance-Bet
    if(Sim_Data[Ind, ][["Result"]]==Parameters_Temp[["Result"]]){
      Balance=Balance+Sim_Data[Ind, eval(parse(text=paste0(Parameters_Temp$Result, "_Odds")))]*Bet
    }
  }
  # {
  #   Bet=min(round(Balance*0.05, 2),
  #           round(49500/max(Sim_Data[Ind,
  #                                    c("Home_Odds",
  #                                      "Draw_Odds",
  #                                      "Away_Odds")]), 2))
  #   Balance=Balance-Bet
  #   if(which.max(Sim_Data[Ind,
  #                         c("Home_Odds",
  #                           "Draw_Odds",
  #                           "Away_Odds")])==match(Sim_Data[Ind, ]$Result, c("Home", "Draw", "Away"))){
  #     Balance=Balance+max(Sim_Data[Ind,
  #                                  c("Home_Odds",
  #                                    "Draw_Odds",
  #                                    "Away_Odds")])*Bet
  #     
  #     Winnig_Games=rbind(Winnig_Games, Sim_Data[Ind, ])
  #   }
  # }
  Balances=c(Balances, Balance)
  Saving_Diff=Saving_Added-Saving_Subtracted
  Saving_Diffs=c(Saving_Diffs, Saving_Diff)
  Savings=c(Savings, tail(Savings, 1)+Saving_Diff)
  
  if(Ind==nrow(Sim_Data)){
    Balances=Balances[-1]
    Savings=Savings[-1]
  }
}
Sim_Data=Sim_Data[order(Date),]
plot(Sim_Data[Date<="2015-01-01", ]$Date,
     Balances[1:length(Sim_Data[Date<="2015-01-01", ]$Date)],
     type='o')
plot(Sim_Data[Date>="2019-06-01", ]$Date,
     Balances[(length(Sim_Data[Date<"2019-06-01", ]$Date)+1):nrow(Sim_Data)],
     type='o')
plot(Sim_Data$Date,
     Balances,
     type='o')
plot(Sim_Data$Date, Savings)
plot(Sim_Data[Date>="2019-06-01", ]$Date, Savings[(length(Sim_Data[Date<"2019-06-01", ]$Date)+1):nrow(Sim_Data)])
tail(Balances, 1)
tail(Savings, 1)+tail(Balances, 1)

sum(Balances<Principal)
#*****************
#
# Descriptive ----
#
#*****************
table(Combined_Results[Year<10000 &
                         Avg_Profit>0, c("Result", "LO")])

#
Filtered_Results=Combined_Results[order(Avg_Profit, decreasing=T), ] %>% 
  filter(
    # Odds_Interval==0.1 &
    # Denominator>500 &
    # Result=="Home"&
    # LO<=2&
    # HO==11.01&
    # Odds_Interval==5&
    Year>3000
      # Profit>0
    # Avg_Profit>0&
    # Win_Prob>0
  )
Parameters=Filtered_Results
plot(Filtered_Results[, c("LO", "Profit")])
abline(h=0, col="red")


library(stocks)
library(tseries)
MDD=mdd(Balances, indices=T)
MDD
Balances[MDD["start.index"]]
Balances[MDD["end.index"]]
plot(Filtered_Results$Profit,
     # ylim=c(-50,100),
     xaxt="n")
abline(h=1, col="red")
axis(1,
     at=seq(1, nrow(Filtered_Results), by=50),
     labels=Filtered_Results$LO[seq(1, nrow(Filtered_Results), by=50)])


#**************
# save and load
#**************
# save.image(paste0(rdata.dir, "2023-01-01.Rdata"))
# load(paste0(rdata.dir, "2022-12-31.Rdata"))



