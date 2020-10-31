
#**********************
#
# Over_Under_Score_Odds
#
#**********************
if(Over_Under_Score_Odds_Manipulation=="Yes"){
  # change team names for consistency
  Over_Under_Score_Odds[Home_Team=="Man City", Home_Team:="Manchester City"]
  Over_Under_Score_Odds[Away_Team=="Man City", Away_Team:="Manchester City"]
  Over_Under_Score_Odds[Home_Team=="Man Utd", Home_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Away_Team=="Man Utd", Away_Team:="Manchester Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Sheff Utd", Home_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Away_Team=="Sheff Utd", Away_Team:="Sheffield Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Sheff Wed", Home_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Away_Team=="Sheff Wed", Away_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Home_Team=="Nottm Forest", Home_Team:="Nottingham"]
  Over_Under_Score_Odds[Away_Team=="Nottm Forest", Away_Team:="Nottingham"]
  
  #
  Over_Under_Score_Odds_Manipulation=""
}




#********
#
# Results
#
#********
if(Results_Manipulation=="Yes"){
  # Date
  Results_Data[, Exact_Date:=as.Date(Date, format="%d.%m.%Y")]
  Results_Data[is.na(Exact_Date), Exact_Date:=as.Date(paste0(Date, Season_Year), format="%d.%m.%Y")]
  Results_Data[Date=="Yesterday", Exact_Date:=as.Date(format(Sys.Date()-1,"%Y-%m-%d"))]
  
  # Total_Goal
  Results_Data[, Home_Goal:=as.numeric(Home_Goal)]
  Results_Data[, Away_Goal:=as.numeric(Away_Goal)]
  
  Results_Data[, Total_Goal:=Home_Goal+Away_Goal]
  Results_Data[, Total_Goal] %>% mean
  Results_Data[, Total_Goal] %>% var
  
  #
  Results_Manipulation=""
}




#***************************
#
# Over_Under_Score_Algorithm
#
#***************************
if(Over_Under_Score_Algorithm=="Yes"){
  # total goal
  Combined_Over_Under_Score_Odds=Combined_Over_Under_Score_Odds %>% 
    left_join(Combined_Results_Data, by=c("Date"="Exact_Date", "Home_Team", "Away_Team")) %>% 
    dplyr::select(-Date.y) %>% 
    as.data.table
  
  #
  Combined_Over_Under_Score_Odds[, Result:=0]
  
  Combined_Over_Under_Score_Odds[Total_Goal<readr::parse_number(Chosen_Option)& # if the total goal is under the number of goals of the "Under" category chosen, result=1
                                   sub("^([[:alpha:]]*).*", "\\1", Chosen_Option)=="Under", Result:=1]
  Combined_Over_Under_Score_Odds[Total_Goal>readr::parse_number(Chosen_Option)& # if the total goal is over the number of goals of the "Over" category chosen, result=1
                                   sub("^([[:alpha:]]*).*", "\\1", Chosen_Option)=="Over", Result:=1]
  
  #
  Over_Under_Score_Algorithm=""
}



