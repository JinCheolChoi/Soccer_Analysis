#********************
#
# empty the workspace
#
#********************
rm(list=ls())

#*******************
#
# set directory path
#
#*******************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
CODE.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/"
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis//Data/Game_results/"
rdata.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/"

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
  "super-league",
  "j1-league",
  "j2-league",
  "super-lig"
)
source(paste0(CODE.dir.1, "Functions.R"))
source(paste0(CODE.dir.2, "SA_Functions.R"))

#**********
# functions
#**********
Imperical_Profit=function(Data){
  Data=Data[!is.na(Home_Odds), ]
  Data[Home_Goal>Away_Goal, Result:="Home"]
  Data[Home_Goal==Away_Goal, Result:="Draw"]
  Data[Home_Goal<Away_Goal, Result:="Away"]
  
  Differences=c(0.01) # interval length for grouping odds into thr same bracket
  # Differences=c(50) # interval length for grouping odds into thr same bracket
  Match_Results=c("Home", "Draw", "Away")
  Results=c()
  for(Difference_Ind in 1:length(Differences)){
    Difference=Differences[Difference_Ind]
    
    LOs=seq(1.01, 40, by=Difference) # lower boundaries of odds brackets
    # LOs=c(11) # lower boundaries of odds brackets
    Result_Temp=matrix(NA, nrow=length(LOs), ncol=3+1)
    for(LO_Ind in 1:length(LOs)){
      LO=LOs[LO_Ind]
      
      for(Match_Result_Ind in 1:length(Match_Results)){
        Denominator_Data=Data[eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
                                eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ]
        Denominator=nrow(Denominator_Data)
        
        Numerator_Data=Data[Result==Match_Results[Match_Result_Ind] &
                              eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
                              eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ]
        Numerator=nrow(Numerator_Data)
        
        Profit=sum(Numerator_Data[, eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))])-
          Denominator
        
        Avg_Profit=Profit/Denominator
        # Avg_Profit=(LO-1)*Numerator/Denominator-1*(1-Numerator/Denominator)
        
        if(Denominator>=1 & Numerator>=1){
          # if(Avg_Profit>0){
          # if(1/(Numerator/Denominator)>LO){
          # one-sample test of proportion
          Prop_Test=prop.test(x=Numerator,
                              n=Denominator,
                              p=1/LO,
                              alternative="greater",
                              correct=FALSE)
          # temp table with results
          Results_Temp=data.table(
            Year=sum(unique(Data[, Season_Year])),
            Result=Match_Results[Match_Result_Ind],
            LO=LO,
            HO=LO+Difference,
            Odds_Interval=Difference,
            Numerator=Numerator,
            Denominator=Denominator,
            Win_Prob=round(Numerator/Denominator, 3),
            Thresh_Odds=round(1/(Numerator/Denominator), 3),
            P_Value=round(Prop_Test$p.value, 3),
            Profit=round(Profit, 3),
            Avg_Profit=round(Avg_Profit, 3),
            Kelly=round(Numerator/Denominator-(1-Numerator/Denominator)/LO, 3)
          )
          
          # record results
          Results=rbind(Results,
                        Results_Temp)
          # }
        }
      }
    }
  }
  
  return(Results)
}


# Import data
Years=c(1998:2022)
All_Years_Data=c()
for(Country in Countries){
  source(paste0(CODE.dir.2, "Country_League_List.R"))
  for(League in All_Leagues){
    for(Year in Years){
      Available=FALSE
      
      # import
      if(Country=="spain" & League=="laliga" & Year>=1989){
        Available=TRUE
      }else if(Country=="china" & League=="super-league" & Year>=2000){
        Available=TRUE
      }else if(Country=="japan" & League=="j1-league" & Year>=1998){
        Available=TRUE
      }else if(Country=="japan" & League=="j2-league" & Year>=2003){
        Available=TRUE
      }else if(Country=="netherlands" & League=="eerste-divisie" & Year>=1989){
        Available=TRUE
      }else if(Country=="germany" & League=="3-liga" & Year>=2008){
        Available=TRUE
      }else if(Country=="germany" & League=="bundesliga" & Year>=1989){
        Available=TRUE
      }else if(Country=="turkey" & League=="super-lig" & Year>=1998){
        Available=TRUE
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="england" & Year>=1989){
        Available=TRUE
      }else{
        next
      }
      
      if(Available==TRUE){
        All_Years_Data=rbind(All_Years_Data,
                             data.table(
                               Country=Country,
                               League=League,
                               fread(paste0(data.dir.1, Country, "/", League, "/", Year, ".csv"))
                             ),
                             fill=TRUE)
      }
      
    }
  }
}
All_Years_Data[Home_Goal>Away_Goal, Result:="Home"]
All_Years_Data[Home_Goal==Away_Goal, Result:="Draw"]
All_Years_Data[Home_Goal<Away_Goal, Result:="Away"]

# algorithm
Results_list=list()
All_Years_Data=All_Years_Data[!is.na(Home_Odds), ] # remove years without odds data
for(Year_Ind in 1:length(Years)){
  # Year_Ind=1
  Results=Imperical_Profit(Data=All_Years_Data[Season_Year==Years[Year_Ind],])
  # Data=All_Years_Data[!is.na(Home_Odds) &
  #                       Season_Year==Years[Year_Ind], ]
  # Data[Home_Goal>Away_Goal, Result:="Home"]
  # Data[Home_Goal==Away_Goal, Result:="Draw"]
  # Data[Home_Goal<Away_Goal, Result:="Away"]
  # 
  # Differences=seq(1, 1, by=0.01)
  # Match_Results=c("Home", "Draw", "Away")
  # Results=c()
  # for(Difference_Ind in 1:length(Differences)){
  #   Difference=Differences[Difference_Ind]
  #   
  #   LOs=seq(1.01, 40, by=Difference)
  #   Result_Temp=matrix(NA, nrow=length(LOs), ncol=3+1)
  #   for(LO_Ind in 1:length(LOs)){
  #     LO=LOs[LO_Ind]
  #     
  #     for(Match_Result_Ind in 1:length(Match_Results)){
  #       Denominator=nrow(Data[eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
  #                               eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ]
  #       )
  #       Numerator=nrow(Data[Result==Match_Results[Match_Result_Ind] &
  #                             eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
  #                             eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ])
  #       Avg_Profit=(LO+1)*Numerator/Denominator-1
  #       if(Denominator>=1 & Numerator>=1){
  #         if(Avg_Profit>2){
  #           # if(1/(Numerator/Denominator)>LO){
  #           # one-sample test of proportion
  #           Prop_Test=prop.test(x=Numerator,
  #                               n=Denominator,
  #                               p=1/LO,
  #                               alternative="greater",
  #                               correct=FALSE)
  #           # temp table with results
  #           Results_Temp=data.table(
  #             Result=Match_Results[Match_Result_Ind],
  #             LO=LO,
  #             HO=LO+Difference,
  #             Numerator=Numerator,
  #             Denominator=Denominator,
  #             Win_Prob=Numerator/Denominator,
  #             Thresh_Odds=1/(Numerator/Denominator),
  #             P_Value=Prop_Test$p.value,
  #             Avg_Profit=round((LO+1)*Numerator/Denominator-1, 3),
  #             Kelly=Numerator/Denominator-(1-Numerator/Denominator)/LO
  #           )
  #           
  #           # record results
  #           Results=rbind(Results,
  #                         Results_Temp)
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  Results_list[[Year_Ind]]=Results
}
Entire_Data_Results=Imperical_Profit(Data=All_Years_Data)


#
Combined_Results=rbind(Entire_Data_Results,
                       do.call(rbind,
                               Results_list))

#
table(Combined_Results[LO>=11&
                         Year<10000 &
                         Avg_Profit>0, c("Result", "LO")])

#
Filtered_Results=Combined_Results[order(Avg_Profit, decreasing=T), ] %>% 
  filter(
    # Odds_Interval==0.1 &
    # Denominator>500 &
    # Result=="Home"&
    LO>=11&
      # HO==11.01&
      # Odds_Interval==5&
      Year>3000
    # Profit>0&
    # Avg_Profit>0&
    # Win_Prob>0
  )

Parameters=Filtered_Results[1,]
# Parameters=Filtered_Results[Profit==max(Profit)]
#
Parameters
# Sim_Data=All_Years_Data[eval(parse(text=paste0(Parameters$Result, "_Odds")))>=Parameters$LO &
#                           eval(parse(text=paste0(Parameters$Result, "_Odds")))<Parameters$HO &
#                           Season_Year>=0&
#                           Country%in%c("england", "turkey", "germany", "netherlands", "spain"), ]
Sim_Data=All_Years_Data[(Home_Odds>=11|Draw_Odds>=11|Away_Odds>=11) &
                          Season_Year>=0&
                          Country%in%c("england", "turkey", "germany", "netherlands", "spain"), ]
Sim_Data=Sim_Data[order(Season_Year, Date),]
Sim_Data=Sim_Data[Season_Year>=0,]
Capital=1
Balance=Capital
Balances=c()
Winnig_Games=c()
for(Ind in 1:nrow(Sim_Data)){
  if(Ind>1 & Balance<100){
    Balance=Balance+1
  }
  if(Balance>=10000){
    Balance=Balance-0
  }
  # {
  #   # Bet=Balance*Parameters$Kelly*0.9
  #   Bet=min(round(Balance*Parameters$Kelly*0.9, 2),
  #           round(49500/max(Sim_Data[Ind,
  #                                    c("Home_Odds")]), 2))
  #   Balance=Balance-Bet
  #   if(Sim_Data[Ind, ]$Result==Parameters$Result){
  #     Balance=Balance+Sim_Data[Ind, eval(parse(text=paste0(Parameters$Result, "_Odds")))]*Bet
  #   }
  # }
  {
    Bet=min(round(Balance*0.05, 2),
            round(49500/max(Sim_Data[Ind,
                                     c("Home_Odds",
                                       "Draw_Odds",
                                       "Away_Odds")]), 2))
    Balance=Balance-Bet
    if(which.max(Sim_Data[Ind,
                          c("Home_Odds",
                            "Draw_Odds",
                            "Away_Odds")])==match(Sim_Data[Ind, ]$Result, c("Home", "Draw", "Away"))){
      Balance=Balance+max(Sim_Data[Ind,
                                   c("Home_Odds",
                                     "Draw_Odds",
                                     "Away_Odds")])*Bet

      Winnig_Games=rbind(Winnig_Games, Sim_Data[Ind, ])
    }
  }
  Balances=c(Balances, Balance)
}
Balance
plot(Balances, type='o')
library(stocks)
library(tseries)
MDD=mdd(Balances, indices=T)
MDD
Balances[MDD["start.index"]]
Balances[MDD["end.index"]]
plot(Filtered_Results[LO>=11]$Profit,
     # ylim=c(-50,100),
     xaxt="n")
abline(h=1, col="red")
axis(1,
     at=seq(1, nrow(Filtered_Results), by=50),
     labels=Filtered_Results$LO[seq(1, nrow(Filtered_Results), by=50)])



#
# save.image(paste0(rdata.dir, "2022-12-31.Rdata"))
# load(paste0(rdata.dir, "2022-12-27.Rdata"))
