#********************
#
# empty the workspace
#
#********************
# rm(list=ls())

#*******************
#
# set directory path
#
#*******************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# data.dir.1="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Data/"
# data.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/"

# CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
# data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
# data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"

#***************
#
# import library
#
#***************
# source(paste0(CODE.dir.1, "Functions.R"))
# #Loading the rvest package
# lapply(c("data.table", "rvest", "stringr"), checkpackages)

#***************************
#
# Over_under_score_odds data
#
#***************************
#Specifying the url for desired website to be scraped
# Year=2021
# Country="england"
# Leagues=c("premier-league", "championship", "league-one", "league-two") # "national-league"

#**********
#
# RSelenium
#
#*****************
library(RSelenium)
#detach("package:RSelenium", unload=TRUE)
# In docker, run the following line first 
#docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
remDr=remoteDriver(
  #remoteServerAddr="192.168.99.103", # desktop
  remoteServerAddr="192.168.99.100", # laptop
  port=4445L,
  browserName="chrome"
)

for(League in Leagues){
  # League="premier-league"
  remDr$open() # Send a request to the remote server
  Sys.sleep(system_sleep)
  
  # # confirm you got there
  # remDr$getTitle()
  # remDr$screenshot(display=TRUE)
  # Sys.sleep(system_sleep)
  
  # "Show More Events"
  ShowMoreEvents=c()
  while(length(ShowMoreEvents)==0){
    #************************************
    # navigate to the website of interest
    #************************************
    print(paste0("League : [", League, "], navigate to the website"))
    
    #if(League=="England"){remDr$navigate("https://www.playnow.com/sports/sport/22/soccer/matches?preselectedFilters=142")} # England
    if(League=="premier-league"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=162")}
    if(League=="championship"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=427")}
    if(League=="league-one"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=434")}
    if(League=="league-two"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=433")}
    if(League==""){remDr$navigate("")}
    if(League==""){remDr$navigate("")}
    if(League==""){remDr$navigate("")}
    
    Sys.sleep(system_sleep)
    #print(remDr$getTitle())
    
    ShowMoreEvents=remDr$findElements(using='css selector', value='.content-loader__load-more-link')
    Sys.sleep(1)
  }
  ShowMoreEvents_Text=unlist(sapply(ShowMoreEvents, function(x){x$getElementText()}))
  for(i in 1:length(ShowMoreEvents_Text)){
    #i=6
    ShowMoreEvents[[i]]$clickElement()
    Sys.sleep(system_sleep)
  }
  
  # get all hyperlinks for games
  print(paste0("League : [", League, "], get all hyperlinks for games"))
  webElems=c()
  while(length(webElems)==0){
    webElems=remDr$findElements(using='css selector', value='a.event-list__item-link-anchor')
    Sys.sleep(1)
  }
  Games=unlist(sapply(webElems, function(x){x$getElementAttribute("href")}))
  
  # game times
  print(paste0("League : [", League, "], game times"))
  
  GameTime=c()
  while(length(GameTime)==0){
    GameTime=remDr$findElements(using='class name', value='event-card__event-time__date-time')
    Sys.sleep(1)
  }
  GameTime_Text=unlist(sapply(GameTime, function(x){x$getElementText()}))
  
  #**********************
  # Over_under_score_odds
  #**********************
  print(paste0("League : [", League, "], Over_under_score_odds"))
  
  Over_under_score_odds=data.table()
  Temp_Over_under_score_odds=data.table()
  Months_Text=c("Jan", "Feb", "Mar", "Apr",
                "May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec")
  for(Game_Ind in 1:length(GameTime_Text)){
    #Game_Ind=1
    Time_Texts=unique(na.omit(unlist(strsplit(unlist(GameTime_Text[Game_Ind]), "[^a-zA-Z]+"))))
    Time_Nums=unique(na.omit(unlist(strsplit(unlist(GameTime_Text[Game_Ind]), "\\D"))))
    if(Time_Texts[1]=="Tomorrow"){
      #Month_Text=month(Sys.Date()+1)
      Day_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%d")
      Year_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%Y")
      Month_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%m")
      
    }else{
      Month_Text=which(Months_Text==Time_Texts[2])
      Day_Text=Time_Nums[2]
      Year_Text=Time_Nums[3]
    }
    
    Temp_Over_under_score_odds[, Date:=as.Date(paste0(Month_Text, "/", Day_Text, "/", Year_Text), format="%m/%d/%Y")]
    Temp_Over_under_score_odds[, League:=League]
    
    Over_under_score_odds=rbind(Over_under_score_odds, Temp_Over_under_score_odds, fill=T)
  }
  #Over_under_score_odds
  
  # extract team names
  print(paste0("League : [", League, "], extract team names"))
  Teams=remDr$findElements(using='class name', value='button--outcome__text-title')
  Teams_Text=unlist(sapply(Teams, function(x){x$getElementText()}))
  Teams_Text=Teams_Text[Teams_Text!="draw"]
  
  # home team
  Over_under_score_odds[, Home_Team:=Teams_Text[1:(2*nrow(Over_under_score_odds))][c(T,F)]]
  # away team
  Over_under_score_odds[, Away_Team:=Teams_Text[1:(2*nrow(Over_under_score_odds))][c(F,T)]]
  
  # URL
  Over_under_score_odds[, URL:=Games]
  
  # extract games scheduled within 1 week (7 days)
  Over_under_score_odds=Over_under_score_odds[Date<=Sys.Date()+7, ]
  
  # remove duplicated games
  Over_under_score_odds=unique(Over_under_score_odds)
  
  #**********************************************************************************
  # extract Total goal odds (only games scheduled to happen within 7 days from today)
  print(paste0("League : [", League, "], extract Total goal odds"))
  
  Over_under_score_odds[,
                        `:=`(`Over 0.5`=0,
                             `Under 0.5`=0,
                             `Over 1.5`=0,
                             `Under 1.5`=0,
                             `Over 2.5`=0,
                             `Under 2.5`=0,
                             `Over 3.5`=0,
                             `Under 3.5`=0,
                             `Over 4.5`=0,
                             `Under 4.5`=0,
                             `Over 5.5`=0,
                             `Under 5.5`=0)]
  for(Game_Ind in 1:nrow(Over_under_score_odds)){
    #*********
    # headings
    Target_Options_N=0
    Heading_Algorithm_Count=0
    print(paste0("League : [", League, "], Target_Options_N"))
    while(Target_Options_N==0){
      Headings=c()
      print(paste0("League : [", League, "], Headings"))
      while(length(Headings)==0){
        # Game_Ind=1
        print(paste0("League : [", League, "], navigate to the game"))
        remDr$navigate(Over_under_score_odds[Game_Ind, URL])
        Sys.sleep(system_sleep)
        
        Headings=remDr$findElements(using='css selector', value='.event-panel__heading__market-name')
        Sys.sleep(1)
      }
      Headings_Text=unlist(sapply(Headings, function(x){x$getElementText()}))
      Sys.sleep(system_sleep)
      
      # count target options
      Target_Options=c("Total Goals (0.5)",
                       "Total Goals (1.5)", 
                       "Total Goals (2.5)", 
                       "Total Goals (3.5)", 
                       "Total Goals (4.5)", 
                       "Total Goals (5.5)")
      Target_Options_N=sum(Headings_Text%in%Target_Options)
      
      # if there's no target option even after 5 tries, just skip this while algorithm
      Heading_Algorithm_Count=Heading_Algorithm_Count+1
      if(Heading_Algorithm_Count==5){
        Target_Options_N=1
      }
    }
    
    # close all headings (the first 5 options are open by default)
    for(i in 1:5){
      #i=6
      Headings[[i]]$clickElement()
      Sys.sleep(system_sleep)
    }
    
    # open headings chosen
    for(i in which(Headings_Text%in%Target_Options)){
      #i=6
      Headings[[i]]$clickElement()
      Sys.sleep(system_sleep)
    }
    
    # over & under texts on the page
    Titles=remDr$findElements(using='css selector', value='.button--outcome__text')
    Titles_Text=unlist(sapply(Titles, function(x){x$getElementText()}))
    Sys.sleep(system_sleep)
    
    # odds of over & under on the page
    Odds=remDr$findElements(using='css selector', value='span.button--outcome__price')
    Odds_Text=unlist(sapply(Odds, function(x){x$getElementText()}))
    Sys.sleep(system_sleep)
    
    # target options available
    Target_Options_Available=Target_Options[Target_Options%in%Headings_Text[which(Headings_Text%in%Target_Options)]]
    
    #*****************************
    # enter odds of target options
    if("Total Goals (0.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 0.5`=as.numeric(Odds_Text[which(Titles_Text=="over 0.5")]),
                                 `Under 0.5`=as.numeric(Odds_Text[which(Titles_Text=="under 0.5")]))]
    }
    if("Total Goals (1.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 1.5`=as.numeric(Odds_Text[which(Titles_Text=="over 1.5")]),
                                 `Under 1.5`=as.numeric(Odds_Text[which(Titles_Text=="under 1.5")]))]
    }
    if("Total Goals (2.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 2.5`=as.numeric(Odds_Text[which(Titles_Text=="over 2.5")]),
                                 `Under 2.5`=as.numeric(Odds_Text[which(Titles_Text=="under 2.5")]))]
    }
    if("Total Goals (3.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 3.5`=as.numeric(Odds_Text[which(Titles_Text=="over 3.5")]),
                                 `Under 3.5`=as.numeric(Odds_Text[which(Titles_Text=="under 3.5")]))]
    }
    if("Total Goals (4.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 4.5`=as.numeric(Odds_Text[which(Titles_Text=="over 4.5")]),
                                 `Under 4.5`=as.numeric(Odds_Text[which(Titles_Text=="under 4.5")]))]
    }
    if("Total Goals (5.5)"%in%Target_Options_Available){
      Over_under_score_odds[Game_Ind,
                            `:=`(`Over 5.5`=as.numeric(Odds_Text[which(Titles_Text=="over 5.5")]),
                                 `Under 5.5`=as.numeric(Odds_Text[which(Titles_Text=="under 5.5")]))]
    }
    print(paste0("League : [", League, "], Progress : " , Game_Ind, " out of ", nrow(Over_under_score_odds)))
    
    Sys.sleep(system_sleep)
  }
  
  # export
  #Current_Over_under_score_odds=fread(paste0(data.dir.2, Country, "/", League, "/", Year, ".csv"))
  Over_under_score_odds=Over_under_score_odds[order(Date, decreasing=T), ]
  fwrite(Over_under_score_odds,
         paste0(data.dir.2, Country, "/", League, "/", Year, ".csv"))
  
  # close server
  remDr$close()
}


