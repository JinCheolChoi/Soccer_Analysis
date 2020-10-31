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
# output.dir="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/"

# CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
# data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Game_results/"
# output.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/Over_under_score_odds/"

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
# Year=2021
# Country="england"
# Leagues=c("premier-league", "championship", "league-one", "league-two") # "national-league"

#**********
#
# RSelenium
#
#*****************
#library(RSelenium)
#detach("package:RSelenium", unload=TRUE)
# In docker, run the following line first 
#docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
remDr=remoteDriver(
  #remoteServerAddr="192.168.99.103", # desktop
  remoteServerAddr="192.168.99.100", # laptop
  port=4445L,
  browserName="chrome"
)

#***********************
#
#
# main part of algorithm
#
#
#***********************
for(Country in Countries){
  #Country="england"
  for(League in Leagues){
    #League="premier-league"
    print("")
    print("")
    print("-------------------------------------------------------------")
    print(paste0("[Time] : ", Country, ", ", League, " - ", Sys.time()))
    
    #***********************
    #
    # read games on the page
    #
    #***********************
    # League="premier-league"
    print("------------ Send a request to the remote server ------------")
    remDr$open() # Send a request to the remote server
    print("-------------------------------------------------------------")
    Sys.sleep(system_sleep)
    
    # # confirm you got there
    # remDr$getTitle()
    # remDr$screenshot(display=TRUE)
    # Sys.sleep(system_sleep)
    
    #************************************
    # navigate to the website of interest
    #************************************
    # getTitle_Text
    getTitle_Text=""
    while(nchar(getTitle_Text)==0){
      print(paste0("Country : [", Country, "], League : [", League, "], navigate to the website"))
      
      #Specifying the url for desired website to be scraped
      #if(League=="England"){remDr$navigate("https://www.playnow.com/sports/sport/22/soccer/matches?preselectedFilters=142")} # England
      if(League=="premier-league"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=162")}
      if(League=="championship"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=427")}
      if(League=="league-one"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=434")}
      if(League=="league-two"){remDr$navigate("https://www.playnow.com/sports/sports/category/142/united-kingdom/matches?preselectedFilters=433")}
      if(League==""){remDr$navigate("https://www.playnow.com/sports/sports/competition/406/matches")}
      if(League==""){remDr$navigate("")}
      if(League==""){remDr$navigate("")}
      
      Sys.sleep(system_sleep)
      
      #
      getTitle_Text=remDr$getTitle()[[1]]
      
      # if error page pops up, re-do it
      if(getTitle_Text[[1]]=="Sportsbook"){
        getTitle_Text[[1]]=""
      }else if(getTitle_Text[[1]]=="Sports | PlayNow.com"){
        getTitle_Text[[1]]=""
      }
    }
    
    # ShowMoreEvents
    ShowMoreEvents=c()
    ShowMoreEvents=remDr$findElements(using='css selector', value='.content-loader__load-more-link')
    Sys.sleep(1)
    
    ShowMoreEvents_Text=unlist(sapply(ShowMoreEvents, function(x){x$getElementText()}))
    # if ther are ShowMoreEvents buttons to click to extend the page, click them
    if(!is.null(ShowMoreEvents_Text)){
      for(i in 1:length(ShowMoreEvents_Text)){
        #i=6
        ShowMoreEvents[[i]]$clickElement()
        Sys.sleep(system_sleep)
      }
    }
    
    # get all hyperlinks for games
    print(paste0("Country : [", Country, "], League : [", League, "], get all hyperlinks for games"))
    webElems=c()
    while(length(webElems)==0){
      webElems=remDr$findElements(using='css selector', value='a.event-list__item-link-anchor')
      Sys.sleep(system_sleep)
    }
    Games=c()
    Games=unlist(sapply(webElems, function(x){x$getElementAttribute("href")}))
    
    if(length(Games)!=0){ # Hyperlink to game exist (start) ----
      # if there is at last one hyperlinke to a game on the page, proceed with extracting data
      
      # game times
      print(paste0("Country : [", Country, "], League : [", League, "], game times"))
      
      GameTime=c()
      while(length(GameTime)==0){
        # GameTime=remDr$findElements(using='class name', value='event-card__event-time__date-time')
        # GameTime=remDr$findElements(using='class name', value='event-card__event-time__clock')
        GameTime=remDr$findElements(using='css selector', value='ul > li > div> div > div > div > span')
        Sys.sleep(1)
      }
      GameTime_Text=unlist(sapply(GameTime, function(x){x$getElementText()}))
      #GameTime_Text=GameTime_Text[GameTime_Text!=""]
      GameTime_Text=paste0(GameTime_Text, collapse = "/")
      GameTime_Text=unlist(strsplit(GameTime_Text, "//"))
      
      #**********************
      # Over_under_score_odds
      #**********************
      print(paste0("Country : [", Country, "], League : [", League, "], Over_under_score_odds"))
      Over_under_score_odds=data.table()
      Temp_Over_under_score_odds=data.table()
      Months_Text=c("Jan", "Feb", "Mar", "Apr",
                    "May", "Jun", "Jul", "Aug",
                    "Sep", "Oct", "Nov", "Dec")
      Days_Text=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      for(Game_Ind in 1:length(GameTime_Text)){
        #Game_Ind=1
        Time_Texts=unique(na.omit(unlist(strsplit(unlist(GameTime_Text[Game_Ind]), "[^a-zA-Z]+"))))
        Time_Texts=Time_Texts[Time_Texts!=""]
        Time_Nums=unique(na.omit(unlist(strsplit(unlist(GameTime_Text[Game_Ind]), "\\D"))))
        
        if(Time_Texts[1]=="Today"){
          #Month_Text=month(Sys.Date()+1)
          Day_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%d")
          Year_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%Y")
          Month_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%m")
          
        }else if(Time_Texts[1]=="Tomorrow"){
          #Month_Text=month(Sys.Date()+1)
          Day_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%d")
          Year_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%Y")
          Month_Text=format(as.Date(Sys.Date()+1, format="%Y-%m-%d"), format = "%m")
          
        }else if(Time_Texts[1]%in%Days_Text){
          Month_Text=which(Months_Text==Time_Texts[2])
          Day_Text=Time_Nums[2]
          Year_Text=Time_Nums[3]
        }else{ # if only time is shown, the game is going to be held today
          Day_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%d")
          Year_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%Y")
          Month_Text=format(as.Date(Sys.Date(), format="%Y-%m-%d"), format = "%m")
        }
        
        Temp_Over_under_score_odds[, Date:=as.Date(paste0(Month_Text, "/", Day_Text, "/", Year_Text), format="%m/%d/%Y")]
        Temp_Over_under_score_odds[, League:=League]
        
        Over_under_score_odds=rbind(Over_under_score_odds, Temp_Over_under_score_odds, fill=T)
      }
      #Over_under_score_odds
      
      # extract team names
      print(paste0("Country : [", Country, "], League : [", League, "], extract team names"))
      
      Home_Teams=remDr$findElements(using='class name', value='event-card__body__name__home')
      Home_Teams_Text=unlist(sapply(Home_Teams, function(x){x$getElementText()}))
      
      Away_Teams=remDr$findElements(using='class name', value='event-card__body__name__away')
      Away_Teams_Text=unlist(sapply(Away_Teams, function(x){x$getElementText()}))
      
      # home team
      Over_under_score_odds[, Home_Team:=Home_Teams_Text]
      # away team
      Over_under_score_odds[, Away_Team:=Away_Teams_Text]
      
      # game URL
      Over_under_score_odds[, URL:=Games]
      
      # extract games scheduled within 1 week (7 days)
      Over_under_score_odds=Over_under_score_odds[Date<=Sys.Date()+1, ]
      
      # remove duplicated games
      Over_under_score_odds=unique(Over_under_score_odds)
      
      # change team names
      Over_under_score_odds=Over_Under_Score_Odds_Manipulation(Over_under_score_odds)
      
      # remove games that are in-play now
      Over_under_score_odds=Over_under_score_odds[!grepl("in-play", URL, fixed=TRUE), ]
      
      #*******************************
      # check if there's new team name
      #*******************************
      data.dir_1=paste0(data.dir.1, Country, "/", League, "/")
      
      File_Lists=list.files(data.dir_1)
      Results_Data=c()
      for(File_to_Open in File_Lists){
        Results_Data=rbind(fread(paste0(data.dir_1, File_to_Open)), Results_Data)
      }
      
      # League column
      Results_Data[, League:=League]
      # Results_Data
      Results_Data=Results_Manipulation(Results_Data)
      
      # check new team names -> there shouldn't any output
      Check_Name=sort(setdiff(unique(c(Over_under_score_odds[, Home_Team], Over_under_score_odds[, Away_Team])), unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))))
      if(length(Check_Name)>0){
        print(paste0("Country : [", Country, "], League : [", League, "], New team names found. Add them in the function, Over_Under_Score_Odds_Manipulation."))
        print("[New team names to be edited]--------------------")
        print(Check_Name)
        print("-------------------------------------------------")
        print("")
        print("[Reference]--------------------------------------")
        print(sort(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))))
        print("-------------------------------------------------")
      }
      
      #***********************
      #
      # parse and record games
      #
      #*********************************************************
      if(file.exists(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))){ # Scenario 1 (Start) ----
        #***********
        # Scenario 1
        #***********
        # if there is an excel file that contains data of past games, 
        # remove games that are already recorded in the excel file
        #*********************************************************
        Current_Over_under_score_odds=fread(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
        if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%Y-%m-%d"))){
          Current_Over_under_score_odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
        }else if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%m/%d/%Y"))){
          Current_Over_under_score_odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
        }
        # class to numeric
        Current_Over_under_score_odds[, `:=`(`Over 0.5`=as.numeric(`Over 0.5`),
                                             `Under 0.5`=as.numeric(`Under 0.5`),
                                             `Over 1.5`=as.numeric(`Over 1.5`),
                                             `Under 1.5`=as.numeric(`Under 1.5`),
                                             `Over 2.5`=as.numeric(`Over 2.5`),
                                             `Under 2.5`=as.numeric(`Under 2.5`),
                                             `Over 3.5`=as.numeric(`Over 3.5`),
                                             `Under 3.5`=as.numeric(`Under 3.5`),
                                             `Over 4.5`=as.numeric(`Over 4.5`),
                                             `Under 4.5`=as.numeric(`Under 4.5`),
                                             `Over 5.5`=as.numeric(`Over 5.5`),
                                             `Under 5.5`=as.numeric(`Under 5.5`))]
        
        # New Date (sometime, game's postponsed)
        Over_under_score_odds[, New_Date:=Date]
        Over_under_score_odds[, Date:=NULL]
        
        # merge two data
        Over_under_score_odds %<>% 
          left_join(Current_Over_under_score_odds, by=c("League", "Home_Team", "Away_Team", "URL"))
        
        # update dates
        Date_Update_Games=Over_under_score_odds[New_Date!=Date, .SD, .SDcols=c("Home_Team", "Away_Team")]
        if(nrow(Date_Update_Games)>0){
          Updated_Games=Over_under_score_odds[New_Date!=Date, .SD, .SDcols=c("Date", "New_Date", "Home_Team", "Away_Team")]
          # update dates
          Current_Over_under_score_odds[Date==Updated_Games$Date &
                                          Home_Team==Updated_Games$Home_Team &
                                          Away_Team==Updated_Games$Away_Team, 
                                        Date:=as.Date(Updated_Games$New_Date)]
          # export
          Current_Over_under_score_odds=Current_Over_under_score_odds[order(Date, Home_Team, Away_Team, decreasing=T), ]
          fwrite(Current_Over_under_score_odds,
                 paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
          
          print(paste0("Country : [", Country, "], League : [", League, "], game dates updated"))
          print(Over_under_score_odds[New_Date!=Date, .SD, .SDcols=c("Date", "New_Date", "Home_Team", "Away_Team")])
        }
        # delete New_Date
        Over_under_score_odds[, Date:=New_Date]
        Over_under_score_odds[, New_Date:=NULL]
        
        # reorder Date column
        Date_Col=which(colnames(Over_under_score_odds)=="Date")
        setcolorder(Over_under_score_odds, c(Date_Col, which((1:ncol(Over_under_score_odds))!=Date_Col)))
        
        # remove already recorded games
        Over_under_score_odds=Over_under_score_odds[is.na(`Over 0.5`)&
                                                      is.na(`Under 0.5`), ]
        
        print(paste0("Country : [", Country, "], League : [", League, "], ", nrow(Over_under_score_odds), " new game(s) found to add."))
        if(nrow(Over_under_score_odds)==0){ # if all games are already archived in the excel file.
          print(paste0("Country : [", Country, "], League : [", League, "], There is no new game to be added. The update of this league will be skipped."))
        }else{ # if there is a game to be added (start) ----
          #**********************************************************************************
          # calculate kelly scores (only games scheduled to happen within 7 days from today)
          print(paste0("Country : [", Country, "], League : [", League, "], calculate kelly scores"))
          
          for(Game_Ind in 1:nrow(Over_under_score_odds)){
            #Game_Ind=1
            Total_Goals_Kelly=Total_Goals_Kelly_Calculator(Over_under_score_odds[Game_Ind, URL], Country, League, Kelly_Method, Chosen_Profit_Criteria)
            
            #*******
            # export
            #*******
            # import Current_Over_under_score_odds
            Current_Over_under_score_odds=fread(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
            if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%Y-%m-%d"))){
              Current_Over_under_score_odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
            }else if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%m/%d/%Y"))){
              Current_Over_under_score_odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
            }
            
            # New_Over_under_score_odds (merge two data)
            New_Over_under_score_odds=rbind(Current_Over_under_score_odds, Total_Goals_Kelly)
            
            # sort by Date, Home_Team, Away_Team
            New_Over_under_score_odds=New_Over_under_score_odds[order(Date, Home_Team, Away_Team, decreasing=T), ]
            
            # export
            fwrite(New_Over_under_score_odds,
                   paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
            #
            print(paste0("Country : [", Country, "], League : [", League, "], Progress : " , Game_Ind, " out of ", nrow(Over_under_score_odds)))
            print("Added game")
            print(Over_under_score_odds[Game_Ind, .SD, .SDcols=c("Date", "Home_Team", "Away_Team")])
          }
        } # if there is a game to be added (end) ----
      }else{ # Scenario 1 (End) & Scenario 2 (Start) ----
        #***********
        # Scenario 2
        #***********
        # if there is no an excel file that contains data of past games, 
        # obtain data of all new games on the page
        #**********************************************************************************
        # calculate kelly scores (only games scheduled to happen within 7 days from today)
        print(paste0("Country : [", Country, "], League : [", League, "], calculate kelly scores"))
        
        for(Game_Ind in 1:nrow(Over_under_score_odds)){
          #Game_Ind=1
          Total_Goals_Kelly=Total_Goals_Kelly_Calculator(Over_under_score_odds[Game_Ind, URL], Country, League, Kelly_Method, Chosen_Profit_Criteria)
          
          if(Game_Ind==1){
            #*******
            # export
            #*******
            # New_Over_under_score_odds (merge two data)
            New_Over_under_score_odds=rbind(Total_Goals_Kelly)
            
            # sort by Date, Home_Team, Away_Team
            New_Over_under_score_odds=New_Over_under_score_odds[order(Date, Home_Team, Away_Team, decreasing=T), ]
            
            # export
            fwrite(New_Over_under_score_odds,
                   paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
          }else{
            #*******
            # export
            #*******
            # import Current_Over_under_score_odds
            Current_Over_under_score_odds=fread(paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
            if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%Y-%m-%d"))){
              Current_Over_under_score_odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
            }else if(!is.na(as.Date(Current_Over_under_score_odds$Date[1], format="%m/%d/%Y"))){
              Current_Over_under_score_odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
            }
            
            # New_Over_under_score_odds (merge two data)
            New_Over_under_score_odds=rbind(Current_Over_under_score_odds, Total_Goals_Kelly)
            
            # sort by Date, Home_Team, Away_Team
            New_Over_under_score_odds=New_Over_under_score_odds[order(Date, Home_Team, Away_Team, decreasing=T), ]
            
            # export
            fwrite(New_Over_under_score_odds,
                   paste0(output.dir, Country, "/", League, "/", Year, ".csv"))
          }
          
          #
          print(paste0("Country : [", Country, "], League : [", League, "], Progress : " , Game_Ind, " out of ", nrow(Over_under_score_odds)))
          print("Added game")
          print(Over_under_score_odds[Game_Ind, .SD, .SDcols=c("Date", "Home_Team", "Away_Team")])
        }
      } # Scenario 2 (End) ----
      
    }else{ # Hyperlink to game exist (end) & No hyperlink to game (start) ----
      # if there is no hyperlink to a game on the page
      print(paste0("Country : [", Country, "], League : [", League, "], There is no hyperlink to a game on the page. The update of this league will be skipped."))
    } # No hyperlink to game (end) ----
    
    
    # close server
    remDr$close()
  }
  
}

