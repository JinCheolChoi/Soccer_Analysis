#******************
#
# game results data
#
#****************************************************
for(Country in Countries){
  if(Country=="england"){
    All_Leagues=c("premier-league",
                  "championship",
                  "league-one",
                  "league-two")
    All_Leagues=All_Leagues[All_Leagues%in%c(Leagues)]
    }else if(Country=="spain"){
      All_Leagues=c("laliga")
      All_Leagues=All_Leagues[All_Leagues%in%Leagues]
      }
  for(League in All_Leagues){
    for(Year in Years){
      #Year=1999
      #Reading the HTML code from the website
      if(Country=="spain" & League=="laliga" & Year%in%c(1990:2016)){
        url=paste0("https://www.betexplorer.com/soccer/", Country, "/primera-division-", Year-1, "-", Year, "/results/")
      }else{
        url=paste0("https://www.betexplorer.com/soccer/", Country, "/", League, "-", Year-1, "-", Year, "/results/")
      }
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
      if(length(Date)>0){ # if data is not empty
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
      }else{ # if data is empty
        Extracted_Data_Temp=data.table(
          Date="",
          Season_Year="",
          Home_Team="",
          Away_Team="",
          Home_Goal="",
          Away_Goa=""
          #Decimal_Odds
        )
        colnames(Extracted_Data_Temp)=c("Date", "Season_Year", "Home_Team", "Away_Team", "Home_Goal", "Away_Goal"
                                        #"Home_Odds", "Draw_Odds", "Away_Odds"
        )
      }
      
      # export
      fwrite(Extracted_Data_Temp,
             paste0(data.dir.1, Country, "/", League, "/", Year, ".csv"))
      
      print(paste0("[", Country, "] ", League, " in ", Year, " updated."))
    }
    
    
  }
}