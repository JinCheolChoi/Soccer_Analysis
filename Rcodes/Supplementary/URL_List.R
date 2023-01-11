#Reading the HTML code from the website
if(Country=="spain" & League=="laliga" & Year>=1989 & Year<2016){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/primera-division-", Year, "-", Year+1, "/results/")
}else if(Country=="spain" & League=="laliga" & Year>=2016){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/laliga-", Year, "-", Year+1, "/results/")
}else if(Country=="china" & League=="super-league" & Year>=2000){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/super-league-", Year, "/results/")
}else if(Country=="japan" & League=="j1-league" & Year>=1998){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/j-league-", Year, "/results/")
}else if(Country=="japan" & League=="j2-league" & Year>=2003){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/j-league-division-2-", Year, "/results/")
}else if(Country=="netherlands" & League=="eerste-divisie" & Year>=1989){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/eredivisie-", Year, "-", Year+1, "/results/")
}else if(Country=="germany" & League=="3-liga" & Year>=2008){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/3-liga-", Year, "-", Year+1, "/results/")
}else if(Country=="germany" & League=="bundesliga" & Year>=1989){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/bundesliga-", Year, "-", Year+1, "/results/")
}else if(Country=="turkey" & League=="super-lig" & Year>=1998){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/super-lig-", Year, "-", Year+1, "/results/")
}else if(Country=="" & League=="" & Year%in%c(1990:2016)){
  
}else if(Country=="england" & League%in%c("premier-league", "championship") & Year>=1989){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year, "-", Year+1, "/results/")
}else if(Country=="england" & League%in%c("league-one", "league-two") & Year>=1998){
  url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year, "-", Year+1, "/results/")
}else{
  next
}