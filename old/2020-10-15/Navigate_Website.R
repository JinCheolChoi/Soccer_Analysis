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
  
  if(League=="laliga"){remDr$navigate("https://www.playnow.com/sports/sports/competition/404/spanish-la-liga/matches")}
  
  if(League=="serie-a"){remDr$navigate("https://www.playnow.com/sports/sports/competition/395/italian-serie-a/matches")}
  
  if(League=="eerste-divisie"){remDr$navigate("https://www.playnow.com/sports/sports/competition/122/dutch-eerste-divisie/matches")}
  
  if(League=="3-liga"){remDr$navigate("https://www.playnow.com/sports/sports/competition/452/german-3-liga/matches")}
  
  Sys.sleep(system_sleep)
  
  #
  getTitle_Text=remDr$getTitle()[[1]]
  
  # if error page pops up, re-do it
  if(getTitle_Text[[1]]=="Sportsbook"){
    getTitle_Text[[1]]=""
  }else if(getTitle_Text[[1]]=="Sports | PlayNow.com"){
    getTitle_Text[[1]]=""
  }else{
    # ShowMoreEvents
    ShowMoreEvents = c()
    ShowMoreEvents = remDr$findElements(using = 'css selector', value =
                                          '.content-loader__load-more-link')
    Sys.sleep(1)
    
    ShowMoreEvents_Text = unlist(sapply(ShowMoreEvents, function(x) {
      x$getElementText()
    }))
    # if ther are ShowMoreEvents buttons to click to extend the page, click them
    if (!is.null(ShowMoreEvents_Text)) {
      for (i in 1:length(ShowMoreEvents_Text)) {
        #i=6
        ShowMoreEvents[[i]]$clickElement()
        Sys.sleep(system_sleep)
      }
    }
  }
}