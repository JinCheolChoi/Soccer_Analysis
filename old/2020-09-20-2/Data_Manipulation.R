
#**********************
#
# Over_Under_Score_Odds
#
#**********************
if(Over_Under_Score_Odds_Manipulation=="Yes"){
  # change team names for consistency
  
  #********************
  # premier-league ----
  #********************
  Over_Under_Score_Odds[Home_Team=="Arsenal FC", Home_Team:="Arsenal"]
  Over_Under_Score_Odds[Away_Team=="Arsenal FC", Away_Team:="Arsenal"]
  
  Over_Under_Score_Odds[Home_Team=="Aston Villa FC", Home_Team:="Aston Villa"]
  Over_Under_Score_Odds[Away_Team=="Aston Villa FC", Away_Team:="Aston Villa"]
  
  Over_Under_Score_Odds[Home_Team=="Brighton & Hove Albion FC", Home_Team:="Brighton"]
  Over_Under_Score_Odds[Away_Team=="Brighton & Hove Albion FC", Away_Team:="Brighton"]
  
  
  Over_Under_Score_Odds[Home_Team=="Burnley FC", Home_Team:="Burnley"]
  Over_Under_Score_Odds[Away_Team=="Burnley FC", Away_Team:="Burnley"]
  
  Over_Under_Score_Odds[Home_Team=="Chelsea FC", Home_Team:="Chelsea"]
  Over_Under_Score_Odds[Away_Team=="Chelsea FC", Away_Team:="Chelsea"]
  
  Over_Under_Score_Odds[Home_Team=="Crystal Palace FC", Home_Team:="Crystal Palace"]
  Over_Under_Score_Odds[Away_Team=="Crystal Palace FC", Away_Team:="Crystal Palace"]
  
  Over_Under_Score_Odds[Home_Team=="Everton FC", Home_Team:="Everton"]
  Over_Under_Score_Odds[Away_Team=="Everton FC", Away_Team:="Everton"]
  
  Over_Under_Score_Odds[Home_Team=="Fulham FC", Home_Team:="Fulham"]
  Over_Under_Score_Odds[Away_Team=="Fulham FC", Away_Team:="Fulham"]
  
  Over_Under_Score_Odds[Home_Team=="Leeds United", Home_Team:="Leeds"]
  Over_Under_Score_Odds[Away_Team=="Leeds United", Away_Team:="Leeds"]
  
  Over_Under_Score_Odds[Home_Team=="Leicester City FC", Home_Team:="Leicester"]
  Over_Under_Score_Odds[Away_Team=="Leicester City FC", Away_Team:="Leicester"]
  
  Over_Under_Score_Odds[Home_Team=="Liverpool FC", Home_Team:="Liverpool"]
  Over_Under_Score_Odds[Away_Team=="Liverpool FC", Away_Team:="Liverpool"]
  
  Over_Under_Score_Odds[Home_Team=="Man City", Home_Team:="Manchester City"]
  Over_Under_Score_Odds[Away_Team=="Man City", Away_Team:="Manchester City"]
  Over_Under_Score_Odds[Home_Team=="Man Utd", Home_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Away_Team=="Man Utd", Away_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Home_Team=="Manchester City FC", Home_Team:="Manchester City"]
  Over_Under_Score_Odds[Away_Team=="Manchester City FC", Away_Team:="Manchester City"]
  
  Over_Under_Score_Odds[Home_Team=="Manchester United FC", Home_Team:="Manchester Utd"]
  Over_Under_Score_Odds[Away_Team=="Manchester United FC", Away_Team:="Manchester Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Newcastle United FC", Home_Team:="Newcastle"]
  Over_Under_Score_Odds[Away_Team=="Newcastle United FC", Away_Team:="Newcastle"]
  
  Over_Under_Score_Odds[Home_Team=="Nottm Forest", Home_Team:="Nottingham"]
  Over_Under_Score_Odds[Away_Team=="Nottm Forest", Away_Team:="Nottingham"]
  
  Over_Under_Score_Odds[Home_Team=="Sheff Utd", Home_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Away_Team=="Sheff Utd", Away_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Home_Team=="Sheffield United FC", Home_Team:="Sheffield Utd"]
  Over_Under_Score_Odds[Away_Team=="Sheffield United FC", Away_Team:="Sheffield Utd"]
  
  
  Over_Under_Score_Odds[Home_Team=="Sheff Wed", Home_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Away_Team=="Sheff Wed", Away_Team:="Sheffield Wed"]
  
  Over_Under_Score_Odds[Home_Team=="Southampton FC", Home_Team:="Southampton"]
  Over_Under_Score_Odds[Away_Team=="Southampton FC", Away_Team:="Southampton"]
  
  Over_Under_Score_Odds[Home_Team=="Tottenham Hotspur FC", Home_Team:="Tottenham"]
  Over_Under_Score_Odds[Away_Team=="Tottenham Hotspur FC", Away_Team:="Tottenham"]
  
  Over_Under_Score_Odds[Home_Team=="West Bromwich Albion", Home_Team:="West Brom"]
  Over_Under_Score_Odds[Away_Team=="West Bromwich Albion", Away_Team:="West Brom"]
  
  Over_Under_Score_Odds[Home_Team=="West Ham United FC", Home_Team:="West Ham"]
  Over_Under_Score_Odds[Away_Team=="West Ham United FC", Away_Team:="West Ham"]
  
  Over_Under_Score_Odds[Home_Team=="Wolverhampton Wanderers F", Home_Team:="Wolves"]
  Over_Under_Score_Odds[Away_Team=="Wolverhampton Wanderers F", Away_Team:="Wolves"]
  Over_Under_Score_Odds[Home_Team=="Wolverhampton Wanderers FC", Home_Team:="Wolves"]
  Over_Under_Score_Odds[Away_Team=="Wolverhampton Wanderers FC", Away_Team:="Wolves"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #******************
  # championship ----
  #******************
  Over_Under_Score_Odds[Home_Team=="AFC Bournemouth", Home_Team:="Bournemouth"]
  Over_Under_Score_Odds[Away_Team=="AFC Bournemouth", Away_Team:="Bournemouth"]
  
  Over_Under_Score_Odds[Home_Team=="Birmingham City", Home_Team:="Birmingham"]
  Over_Under_Score_Odds[Away_Team=="Birmingham City", Away_Team:="Birmingham"]
  
  Over_Under_Score_Odds[Home_Team=="Blackburn Rovers", Home_Team:="Blackburn"]
  Over_Under_Score_Odds[Away_Team=="Blackburn Rovers", Away_Team:="Blackburn"]
  
  Over_Under_Score_Odds[Home_Team=="Brentford FC", Home_Team:="Brentford"]
  Over_Under_Score_Odds[Away_Team=="Brentford FC", Away_Team:="Brentford"]
  
  Over_Under_Score_Odds[Home_Team=="Cardiff City", Home_Team:="Cardiff"]
  Over_Under_Score_Odds[Away_Team=="Cardiff City", Away_Team:="Cardiff"]
  
  Over_Under_Score_Odds[Home_Team=="Derby County", Home_Team:="Derby"]
  Over_Under_Score_Odds[Away_Team=="Derby County", Away_Team:="Derby"]
  
  Over_Under_Score_Odds[Home_Team=="Huddersfield Town", Home_Team:="Huddersfield"]
  Over_Under_Score_Odds[Away_Team=="Huddersfield Town", Away_Team:="Huddersfield"]
  
  Over_Under_Score_Odds[Home_Team=="Luton Town", Home_Team:="Luton"]
  Over_Under_Score_Odds[Away_Team=="Luton Town", Away_Team:="Luton"]
  
  Over_Under_Score_Odds[Home_Team=="Middlesbrough FC", Home_Team:="Middlesbrough"]
  Over_Under_Score_Odds[Away_Team=="Middlesbrough FC", Away_Team:="Middlesbrough"]
  
  Over_Under_Score_Odds[Home_Team=="Millwall FC", Home_Team:="Millwall"]
  Over_Under_Score_Odds[Away_Team=="Millwall FC", Away_Team:="Millwall"]
  
  Over_Under_Score_Odds[Home_Team=="Norwich City FC", Home_Team:="Norwich"]
  Over_Under_Score_Odds[Away_Team=="Norwich City FC", Away_Team:="Norwich"]
  
  Over_Under_Score_Odds[Home_Team=="Nottingham Forest", Home_Team:="Nottingham"]
  Over_Under_Score_Odds[Away_Team=="Nottingham Forest", Away_Team:="Nottingham"]
  
  Over_Under_Score_Odds[Home_Team=="Preston North End", Home_Team:="Preston"]
  Over_Under_Score_Odds[Away_Team=="Preston North End", Away_Team:="Preston"]
  
  Over_Under_Score_Odds[Home_Team=="Reading FC", Home_Team:="Reading"]
  Over_Under_Score_Odds[Away_Team=="Reading FC", Away_Team:="Reading"]
  
  Over_Under_Score_Odds[Home_Team=="Rotherham United", Home_Team:="Rotherham"]
  Over_Under_Score_Odds[Away_Team=="Rotherham United", Away_Team:="Rotherham"]
  
  Over_Under_Score_Odds[Home_Team=="Sheffield Wednesday", Home_Team:="Sheffield Wed"]
  Over_Under_Score_Odds[Away_Team=="Sheffield Wednesday", Away_Team:="Sheffield Wed"]
  
  Over_Under_Score_Odds[Home_Team=="Stoke City", Home_Team:="Stoke"]
  Over_Under_Score_Odds[Away_Team=="Stoke City", Away_Team:="Stoke"]
  
  Over_Under_Score_Odds[Home_Team=="Swansea City", Home_Team:="Swansea"]
  Over_Under_Score_Odds[Away_Team=="Swansea City", Away_Team:="Swansea"]
  
  Over_Under_Score_Odds[Home_Team=="Watford FC", Home_Team:="Watford"]
  Over_Under_Score_Odds[Away_Team=="Watford FC", Away_Team:="Watford"]
  
  Over_Under_Score_Odds[Home_Team=="Wycombe Wanderers", Home_Team:="Wycombe"]
  Over_Under_Score_Odds[Away_Team=="Wycombe Wanderers", Away_Team:="Wycombe"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #****************
  # league-one ----
  #****************
  Over_Under_Score_Odds[Home_Team=="Accrington Stanley", Home_Team:="Accrington"]
  Over_Under_Score_Odds[Away_Team=="Accrington Stanley", Away_Team:="Accrington"]
  
  Over_Under_Score_Odds[Home_Team=="Burton Albion", Home_Team:="Burton"]
  Over_Under_Score_Odds[Away_Team=="Burton Albion", Away_Team:="Burton"]
  
  Over_Under_Score_Odds[Home_Team=="Charlton Athletic", Home_Team:="Charlton"]
  Over_Under_Score_Odds[Away_Team=="Charlton Athletic", Away_Team:="Charlton"]
  
  Over_Under_Score_Odds[Home_Team=="Crewe Alexandra", Home_Team:="Crewe"]
  Over_Under_Score_Odds[Away_Team=="Crewe Alexandra", Away_Team:="Crewe"]
  
  Over_Under_Score_Odds[Home_Team=="Doncaster Rovers", Home_Team:="Doncaster"]
  Over_Under_Score_Odds[Away_Team=="Doncaster Rovers", Away_Team:="Doncaster"]
  
  Over_Under_Score_Odds[Home_Team=="Fleetwood Town", Home_Team:="Fleetwood"]
  Over_Under_Score_Odds[Away_Team=="Fleetwood Town", Away_Team:="Fleetwood"]
  
  Over_Under_Score_Odds[Home_Team=="Gillingham FC", Home_Team:="Gillingham"]
  Over_Under_Score_Odds[Away_Team=="Gillingham FC", Away_Team:="Gillingham"]
  
  Over_Under_Score_Odds[Home_Team=="Hull City", Home_Team:="Hull"]
  Over_Under_Score_Odds[Away_Team=="Hull City", Away_Team:="Hull"]
  
  Over_Under_Score_Odds[Home_Team=="Ipswich Town", Home_Team:="Ipswich"]
  Over_Under_Score_Odds[Away_Team=="Ipswich Town", Away_Team:="Ipswich"]
  
  Over_Under_Score_Odds[Home_Team=="Lincoln City", Home_Team:="Lincoln"]
  Over_Under_Score_Odds[Away_Team=="Lincoln City", Away_Team:="Lincoln"]
  
  Over_Under_Score_Odds[Home_Team=="Milton Keynes Dons", Home_Team:="MK Dons"]
  Over_Under_Score_Odds[Away_Team=="Milton Keynes Dons", Away_Team:="MK Dons"]
  
  Over_Under_Score_Odds[Home_Team=="Northampton Town", Home_Team:="Northampton"]
  Over_Under_Score_Odds[Away_Team=="Northampton Town", Away_Team:="Northampton"]
  
  Over_Under_Score_Odds[Home_Team=="Oxford United", Home_Team:="Oxford Utd"]
  Over_Under_Score_Odds[Away_Team=="Oxford United", Away_Team:="Oxford Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Peterborough United", Home_Team:="Peterborough"]
  Over_Under_Score_Odds[Away_Team=="Peterborough United", Away_Team:="Peterborough"]
  
  Over_Under_Score_Odds[Home_Team=="Plymouth Argyle", Home_Team:="Plymouth"]
  Over_Under_Score_Odds[Away_Team=="Plymouth Argyle", Away_Team:="Plymouth"]
  
  Over_Under_Score_Odds[Home_Team=="Shrewsbury Town", Home_Team:="Shrewsbury"]
  Over_Under_Score_Odds[Away_Team=="Shrewsbury Town", Away_Team:="Shrewsbury"]
  
  Over_Under_Score_Odds[Home_Team=="Sunderland AFC", Home_Team:="Sunderland"]
  Over_Under_Score_Odds[Away_Team=="Sunderland AFC", Away_Team:="Sunderland"]
  
  Over_Under_Score_Odds[Home_Team=="Swindon Town", Home_Team:="Swindon"]
  Over_Under_Score_Odds[Away_Team=="Swindon Town", Away_Team:="Swindon"]
  
  Over_Under_Score_Odds[Home_Team=="Wigan Athletic", Home_Team:="Wigan"]
  Over_Under_Score_Odds[Away_Team=="Wigan Athletic", Away_Team:="Wigan"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
  #****************
  # league-two ----
  #****************
  Over_Under_Score_Odds[Home_Team=="Barrow AFC", Home_Team:="Barrow"]
  Over_Under_Score_Odds[Away_Team=="Barrow AFC", Away_Team:="Barrow"]
  
  Over_Under_Score_Odds[Home_Team=="Bolton Wanderers", Home_Team:="Bolton"]
  Over_Under_Score_Odds[Away_Team=="Bolton Wanderers", Away_Team:="Bolton"]
  
  Over_Under_Score_Odds[Home_Team=="Cambridge United", Home_Team:="Cambridge Utd"]
  Over_Under_Score_Odds[Away_Team=="Cambridge United", Away_Team:="Cambridge Utd"]
  
  Over_Under_Score_Odds[Home_Team=="Carlisle United", Home_Team:="Carlisle"]
  Over_Under_Score_Odds[Away_Team=="Carlisle United", Away_Team:="Carlisle"]
  
  Over_Under_Score_Odds[Home_Team=="Cheltenham Town", Home_Team:="Cheltenham"]
  Over_Under_Score_Odds[Away_Team=="Cheltenham Town", Away_Team:="Cheltenham"]
  
  Over_Under_Score_Odds[Home_Team=="Colchester United", Home_Team:="Colchester"]
  Over_Under_Score_Odds[Away_Team=="Colchester United", Away_Team:="Colchester"]
  
  Over_Under_Score_Odds[Home_Team=="Crawley Town", Home_Team:="Crawley"]
  Over_Under_Score_Odds[Away_Team=="Crawley Town", Away_Team:="Crawley"]
  
  Over_Under_Score_Odds[Home_Team=="Exeter City", Home_Team:="Exeter"]
  Over_Under_Score_Odds[Away_Team=="Exeter City", Away_Team:="Exeter"]
  
  Over_Under_Score_Odds[Home_Team=="Forest Green Rovers", Home_Team:="Forest Green"]
  Over_Under_Score_Odds[Away_Team=="Forest Green Rovers", Away_Team:="Forest Green"]
  
  Over_Under_Score_Odds[Home_Team=="Grimsby Town", Home_Team:="Grimsby"]
  Over_Under_Score_Odds[Away_Team=="Grimsby Town", Away_Team:="Grimsby"]
  
  Over_Under_Score_Odds[Home_Team=="Harrogate Town FC", Home_Team:="Harrogate"]
  Over_Under_Score_Odds[Away_Team=="Harrogate Town FC", Away_Team:="Harrogate"]
  
  Over_Under_Score_Odds[Home_Team=="Mansfield Town", Home_Team:="Mansfield"]
  Over_Under_Score_Odds[Away_Team=="Mansfield Town", Away_Team:="Mansfield"]
  
  Over_Under_Score_Odds[Home_Team=="Morecambe FC", Home_Team:="Morecambe"]
  Over_Under_Score_Odds[Away_Team=="Morecambe FC", Away_Team:="Morecambe"]
  
  Over_Under_Score_Odds[Home_Team=="Newport County AFC", Home_Team:="Newport"]
  Over_Under_Score_Odds[Away_Team=="Newport County AFC", Away_Team:="Newport"]
  
  Over_Under_Score_Odds[Home_Team=="Oldham Athletic", Home_Team:="Oldham"]
  Over_Under_Score_Odds[Away_Team=="Oldham Athletic", Away_Team:="Oldham"]
  
  Over_Under_Score_Odds[Home_Team=="Salford City FC", Home_Team:="Salford"]
  Over_Under_Score_Odds[Away_Team=="Salford City FC", Away_Team:="Salford"]
  
  Over_Under_Score_Odds[Home_Team=="Scunthorpe United", Home_Team:="Scunthorpe"]
  Over_Under_Score_Odds[Away_Team=="Scunthorpe United", Away_Team:="Scunthorpe"]
  
  Over_Under_Score_Odds[Home_Team=="Southend United", Home_Team:="Southend"]
  Over_Under_Score_Odds[Away_Team=="Southend United", Away_Team:="Southend"]
  
  Over_Under_Score_Odds[Home_Team=="Stevenage FC", Home_Team:="Stevenage"]
  Over_Under_Score_Odds[Away_Team=="Stevenage FC", Away_Team:="Stevenage"]
  
  Over_Under_Score_Odds[Home_Team=="Tranmere Rovers", Home_Team:="Tranmere"]
  Over_Under_Score_Odds[Away_Team=="Tranmere Rovers", Away_Team:="Tranmere"]
  
  Over_Under_Score_Odds[Home_Team=="Walsall FC", Home_Team:="Walsall"]
  Over_Under_Score_Odds[Away_Team=="Walsall FC", Away_Team:="Walsall"]
  
  Over_Under_Score_Odds[Home_Team=="", Home_Team:=""]
  Over_Under_Score_Odds[Away_Team=="", Away_Team:=""]
  
  
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
  Results_Data[Date=="Today", Exact_Date:=as.Date(format(Sys.Date(),"%Y-%m-%d"))]
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
    left_join(Combined_Results_Data, by=c("Date"="Exact_Date", "League", "Home_Team", "Away_Team")) %>% 
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

