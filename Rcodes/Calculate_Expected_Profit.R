# load data
All_Years_Data=c()
for(Country in Countries){
  source(paste0(CODE.dir.3, "Country_League_List.R"))
  for(League in All_Leagues){
    for(Year in Years){
      # import
      if(
        (Country=="england" & League%in%c("premier-league", "championship") & Year>=1989) |
        (Country=="england" & League%in%c("league-one", "league-two") & Year>=1998) |
        (Country=="spain" & League=="laliga" & Year>=1989) |
        (Country=="china" & League=="super-league" & Year>=2000) |
        (Country=="japan" & League=="j1-league" & Year>=1998) |
        (Country=="japan" & League=="j2-league" & Year>=2003) |
        (Country=="netherlands" & League=="eerste-divisie" & Year>=1989) |
        (Country=="germany" & League=="3-liga" & Year>=2008) |
        (Country=="germany" & League=="bundesliga" & Year>=1989) |
        (Country=="turkey" & League=="super-lig" & Year>=1998) |
        (Country=="" & League=="" & Year%in%c(1990:2016))){
      }else{
        next
      }
      
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
All_Years_Data[Home_Goal>Away_Goal, Result:="Home"]
All_Years_Data[Home_Goal==Away_Goal, Result:="Draw"]
All_Years_Data[Home_Goal<Away_Goal, Result:="Away"]
All_Years_Data=All_Years_Data[!Date%in%c("Yesterday", "Today"), ]
All_Years_Data[str_length(Date)<8, Date:=paste0(Date, Season_Year)]
All_Years_Data[, Date:=as.Date(Date, format="%d.%m.%Y")]
All_Years_Data=All_Years_Data[order(Date), ]

# algorithm
Results_list=list()
All_Years_Data=All_Years_Data[!(is.na(Home_Odds)| # remove years without odds data
                                  is.na(Draw_Odds)|
                                  is.na(Away_Odds)), ]
for(Year_Ind in 1:length(Years)){
  # Year_Ind=1
  Results=Imperical_Profit(Data=All_Years_Data[Season_Year==Years[Year_Ind],],
                           Differences=0.5,
                           LLO=1.01,
                           HLO=max(summary(c(All_Years_Data$Away_Odds,
                                             All_Years_Data$Home_Odds,
                                             All_Years_Data$Away_Odds))))
  Results_list[[Year_Ind]]=Results
}
Entire_Data_Results=Imperical_Profit(Data=All_Years_Data,
                                     Differences=0.5,
                                     LLO=1.01,
                                     HLO=max(summary(c(All_Years_Data$Away_Odds,
                                                       All_Years_Data$Home_Odds,
                                                       All_Years_Data$Away_Odds))))

#
Combined_Results=rbind(Entire_Data_Results,
                       do.call(rbind,
                               Results_list))


#
# save.image(paste0(rdata.dir, "2022-12-31.Rdata"))
# load(paste0(rdata.dir, "2022-12-27.Rdata"))
