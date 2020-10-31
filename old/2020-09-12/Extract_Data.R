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
# data.dir.1="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Data/"
# data.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/"

CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
data.dir.1="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/"
data.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/"

#***************
#
# import library
#
#***************
source(paste0(CODE.dir.1, "Functions.R"))
#Loading the rvest package
lapply(c("data.table", "rvest"), checkpackages)

#**********
#
# goal data
#
#****************************************************
#Specifying the url for desired website to be scraped
Years=2020
Country="england"
Leagues=c("premier-league", "championship", "league-one", "league-two", "national-league")


for(League in Leagues){
  for(Year in Years){
    #Year=1999
    #Reading the HTML code from the website
    url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year-1, "-", Year, "/results/")
    webpage=read_html(url)
    
    #Using CSS selectors to scrape data
    Date=html_text(html_nodes(webpage,'.h-text-right'))
    Team_Names=t(matrix(html_text(html_nodes(webpage,'.h-text-left span')), nrow=2))
    #Goals=t(matrix(unlist(strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")), nrow=2))
    Goals=strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")
    
    # # Odds
    # Decimal_Odds=t(matrix(unlist(html_attrs(html_nodes(webpage, '[data-odd]')))[names(unlist(html_attrs(html_nodes(webpage, '[data-odd]'))))=="data-odd"], nrow=3))
    # if(length(unlist(html_attrs(html_nodes(webpage, '[data-odd]')))[names(unlist(html_attrs(html_nodes(webpage, '[data-odd]'))))=="data-odd"])%%3!=0){
    #   print(paste0("Error : ", Year))
    # }
    
    #
    Recorded=unlist(lapply(Goals, length)) # 1:No, 2:Yes
    
    #
    Extracted_Data_Temp=data.table(
      Date[Recorded==2],
      Year,
      Team_Names[Recorded==2, ],
      t(matrix(unlist(Goals[Recorded==2]), nrow=2))
      #Decimal_Odds
    )
    colnames(Extracted_Data_Temp)=c("Date", "Season_Year", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal"
                                    #"Home_Odds", "Draw_Odds", "Away_Odds"
    )
    
    # export
    fwrite(Extracted_Data_Temp,
           paste0(data.dir.1, Country, "/", League, "/", Year, ".csv"))
  }
  
  print(paste0("[", Country, "] ", League, " in ", Year, " updated."))
}


#****************
#
#****************
library(RSelenium)
#detach("package:RSelenium", unload=TRUE)

# In docker, run the following line first 
#docker run -d -p 4445:4444 selenium/standalone-chrome:3.141.59
remDr <- remoteDriver(
  #remoteServerAddr="192.168.99.103",
  remoteServerAddr="192.168.99.100",
  port=4445L,
  browserName="chrome"
)
remDr$open() # Send a request to the remote server
#remDr$close()

# navigate to the website of interest
remDr$navigate("https://www.playnow.com/sports/sport/22/soccer/matches?preselectedFilters=142")

# https://www.playnow.com/sports/sports/competition/162/english-premier-league/matches      # premier league
# https://www.playnow.com/sports/sports/competition/427/english-championship/matches        # championship

#remDr$navigate("http://www.google.com")

# confirm you got there
remDr$getTitle()
remDr$screenshot(display=TRUE)

# 
webElems=remDr$findElements(using='css selector', value='a.event-list__item-link-anchor')
Sys.sleep(1)

# get all hyperlinks for games
hrefs=unlist(sapply(webElems, function(x){x$getElementAttribute("href")}))
Sys.sleep(1)

# click the first hyperlink
remDr$navigate(hrefs[1])
Sys.sleep(1)

# 
remDr$getCurrentUrl() # current url
Sys.sleep(1)

remDr$screenshot(display=TRUE)
Sys.sleep(2)

# headings
Headings=remDr$findElements(using='css selector', value='.event-panel__heading__market-name')
Headings_Text=unlist(sapply(Headings, function(x){x$getElementText()}))
Sys.sleep(2)

# close all headings
for(i in 1:5){
  #i=6
  Headings[[i]]$clickElement()
  Sys.sleep(2)
}
# open headings chosen
for(i in which(Headings_Text%in%c("Total Goals (0.5)",
                                  "Total Goals (1.5)", 
                                  "Total Goals (2.5)", 
                                  "Total Goals (3.5)", 
                                  "Total Goals (4.5)", 
                                  "Total Goals (5.5)"))){
  #i=6
  Headings[[i]]$clickElement()
  Sys.sleep(2)
}


#
Titles=remDr$findElements(using='css selector', value='.button--outcome__text')
Titles_Text=unlist(sapply(Titles, function(x){x$getElementText()}))
Sys.sleep(2)

Odds=remDr$findElements(using='css selector', value='span.button--outcome__price')
Odds_Text=unlist(sapply(Odds, function(x){x$getElementText()}))
Sys.sleep(2)







#sports-wrapper > div > div > div > div > main > section > div.section.section--content-wrapper > div.section.section--main-content > div > div > div.main-content__content-canvas > div > div > article > div > div.concealed-widget > article > div > div:nth-child(12) > div.event-panel__body > div > div > div:nth-child(1) > div > button > span > span.button--outcome__text > span

##############
# library(robotstxt) # check if web-bot has permission to access certain parts of a web-page
# paths_allowed("https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vT7B2BJLEu9qPYIrzfDqORAFLVHN32e8DhS6lAFD9I8ibwKP7eh4KR4StjeLn3U0RMKs6EC2gCfrYIW/pubhtml/sheet?headers=false&gid=2015751929")
# 
# 
# 
# 
# library(rvest)
# library(stringr)
# library(plyr)
# library(dplyr)
# library(ggvis)
# library(knitr)
# options(digits = 4)
# 
# 
# #url=paste0("https://www.playnow.com/sports/sport/22/soccer/matches")
# Path=shortPathName("C:/Users/JinCheol Choi/Desktop/phantomjs-2.1.1-windows")
# system(paste0(Path, "/bin/phantomjs ", Path, "/Test.js"))
# 
# 
# withJS <- xml2::read_html(paste0(Path, "/techstars.html")) %>%
#   rvest::html_nodes(".avb-item") %>%
#   rvest::html_text()
# 
# webpage %>%
#   html_nodes(".wrapper div.wrapper")
# 
# webpage %>%
#   html_nodes(".event-list__item-link-anchor")
