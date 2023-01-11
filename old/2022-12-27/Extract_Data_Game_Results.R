#******************
#
# game results data
#
#****************************************************
for(Country in Countries){
  source(paste0(CODE.dir.2, "Country_League_List.R"))
  for(League in All_Leagues){
    for(Year in Years){
      #Year=1999
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
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="" & League=="" & Year%in%c(1990:2016)){
        
      }else if(Country=="england" & League%in%c("premier-league", "championship") & Year>=1989){
        url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year, "-", Year+1, "/results/")
      }else if(Country=="england" & League%in%c("league-one", "league-two") & Year>=1998){
        url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year, "-", Year+1, "/results/")
      }else{
        next
      }
      webpage=read_html(url)
      
      #Using CSS selectors to scrape data
      Date=html_text(html_nodes(webpage,'.h-text-right'))
      Team_Names=t(matrix(html_text(html_nodes(webpage,'.h-text-left span')), nrow=2))
      #Goals=t(matrix(unlist(strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")), nrow=2))
      Goals=strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")
      
      # Odds
      Decimal_Odds=t(matrix(unlist(html_attrs(html_nodes(webpage, '[data-odd]')))[names(unlist(html_attrs(html_nodes(webpage, '[data-odd]'))))=="data-odd"], nrow=3))
      if(length(unlist(html_attrs(html_nodes(webpage, '[data-odd]')))[names(unlist(html_attrs(html_nodes(webpage, '[data-odd]'))))=="data-odd"])%%3!=0){
        print(paste0("Error : ", Year))
        print(paste0("[", Country, "] ", League, " in ", Year, " UNUPDATED."))
        next
      }
      
      #
      Recorded=unlist(lapply(Goals, length)) # 1:No, 2:Yes
      
      #
      if(length(Date)>0){ # if data is not empty
        Extracted_Data_Temp=data.table(
          Date[Recorded==2],
          Year,
          Team_Names[Recorded==2, ],
          t(matrix(unlist(Goals[Recorded==2]), nrow=2)),
          Decimal_Odds
        )
        colnames(Extracted_Data_Temp)=c("Date", "Season_Year", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal",
                                        "Home_Odds", "Draw_Odds", "Away_Odds"
        )
      }else{ # if data is empty
        Extracted_Data_Temp=data.table(
          Date="",
          Season_Year="",
          Home_Team="",
          Away_Team="",
          Home_Goal="",
          Away_Goa="",
          Decimal_Odds
        )
        colnames(Extracted_Data_Temp)=c("Date", "Season_Year", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal",
                                        "Home_Odds", "Draw_Odds", "Away_Odds"
        )
      }
      
      # export
      fwrite(Extracted_Data_Temp,
             paste0(data.dir.1, Country, "/", League, "/", Year, ".csv"))
      
      print(paste0("[", Country, "] ", League, " in ", Year, " updated."))
    }
    
    
  }
}
