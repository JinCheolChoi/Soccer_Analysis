#******************
#
# game results data
#
#****************************************************
lapply(c("data.table",
         "rvest"), checkpackages)
for(Country in Countries){
  source(paste0(CODE.dir.3, "Country_League_List.R"))
  for(League in All_Leagues){
    for(Year in Years){
      #Year=2022
      source(paste0(CODE.dir.3, "URL_List.R"))
      webpage=read_html(url)
      
      #Using CSS selectors to scrape data
      Date=html_text(html_nodes(webpage,'.h-text-right'))
      Team_Names=t(matrix(html_text(html_nodes(webpage,'.h-text-left span')), nrow=2))
      #Goals=t(matrix(unlist(strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")), nrow=2))
      Goals=strsplit(html_text(html_nodes(webpage,'.h-text-center a')), ":")
      
      # Odds
      Attrs=unlist(html_attrs(html_nodes(webpage, '[data-odd]')))
      
      # deal with missing Odds
      Page_Texts=html_text(html_nodes(webpage, 'td'))
      Page_Texts=Page_Texts[which(Page_Texts!="")]
      
      Odds=Attrs[names(Attrs)=="data-odd"]
      Missing_Inds=which(str_length(Page_Texts)==1)
      Missing_Inds=Missing_Inds-(0:(length(Missing_Inds)-1))
      Missing_Game_Inds=Missing_Inds%/%3
      
      for(Missing_Game_Ind in Missing_Game_Inds){
        Odds=c(Odds[1:((Missing_Game_Ind-1)*3)],
               "NA",
               Odds[((Missing_Game_Ind-1)*3+1):length(Odds)])
      }
      Decimal_Odds=t(matrix(Odds, nrow=3))
      if(length(Missing_Game_Inds)>=1){
        Partial_Odds_NA=TRUE
      }
      if(length(Missing_Game_Inds)>1){
        Partial_Odds_NA=TRUE
        print(paste0("[", Country, "] ", League, " in ", Year, " has length(Missing_Game_Inds)>1."))
      }
      
      #
      Recorded=unlist(lapply(Goals, length)) # 1:No, 2:Yes
      
      #
      if(length(Date)>0){ # if data is not empty
        Extracted_Data_Temp=data.table(
          Date,
          Year,
          Team_Names,
          do.call(rbind, Goals),
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
      
      # there likely be some missing odds, in which case we don't need *_Odds variables
      if(exists("Partial_Odds_NA")){
        if(Partial_Odds_NA==TRUE){
          print(paste0("[", Country, "] ", League, " in ", Year, " : Some Odds are missing."))
        }
        Partial_Odds_NA=FALSE
      }
      
      # export
      fwrite(Extracted_Data_Temp,
             paste0(data.dir.1, Country, "/", League, "/", Year, ".csv"))
      
      print(paste0("[", Country, "] ", League, " in ", Year, " updated."))
    }
    
  }
}

# 
# length(html_attrs(html_nodes(webpage, '[data-odd]')))
# html_attrs(html_elements(webpage, "td")) %>% 
# ?html_table
# html_attrs(html_elements(webpage, "td.table-main__odds")) %>% head
# html_text(html_elements(webpage, "td"))[1301:1350]
# 
# which(html_text(html_elements(webpage, "td"))=="&nbsp;")
# 
# which(html_text(html_elements(webpage, "tr td"))==" ")
# 
# html_attrs(html_elements(webpage, "td:empty"))
