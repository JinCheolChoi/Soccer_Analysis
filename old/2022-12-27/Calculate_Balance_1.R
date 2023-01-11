#*******************************
#
# obtain the results of bettings
#
#*******************************
Combined_Results_Data=c()
for(Country in Countries){
  # Country="england"
  source(paste0(CODE.dir.2, "Country_League_List.R"))
  for(League_Text in All_Leagues){
    #League_Text="premier-league"
    #League_Text="championship"
    
    #*******************
    #
    # set directory path
    #
    #*******************
    data.dir_1=paste0(data.dir.1, Country, "/", League_Text, "/")
    
    #********************
    # import results data
    #********************
    File_Lists=list.files(data.dir_1)
    Results_Data=c()
    for(File_to_Open in File_Lists){
      Results_Data=rbind(fread(paste0(data.dir_1, File_to_Open)), Results_Data, fill=T)
    }
    
    # League_Text column
    Results_Data[, League:=League_Text]
    Results_Data=Results_Manipulation(Results_Data)
    
    # combine all results data
    Combined_Results_Data=rbind(Combined_Results_Data, Results_Data, fill=T)
  }
}


#*******************************
#
# Combined_Over_Under_Score_Odds
#
#*******************************
if(Simulation=="No"){
  # combined Over_Under_Betting_Result bettings
  Combined_Over_Under_Score_Odds=c()
  for(Country in Countries){
    # Country="england"
    source(paste0(CODE.dir.2, "Country_League_List.R"))
    for(League_Text in All_Leagues){
      for(Year in Years){
        #
        if(file.exists(paste0(output.dir, Country, "/", League_Text, "/", Year, ".csv"))){ # if Over_Under_Score_Odds exists (start) ----
          Over_Under_Score_Odds=fread(paste0(output.dir, Country, "/", League_Text, "/", Year, ".csv"))
          if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%Y-%m-%d"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
          }else if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%m/%d/%Y"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
          }
          
          Combined_Over_Under_Score_Odds=rbind(Combined_Over_Under_Score_Odds, Over_Under_Score_Odds, fill=T)
        }
        
      }
    }
  }
}else if(Simulation=="Yes"){ # simulation settings (different kelly scores)
  # combined Over_Under_Betting_Result bettings
  Combined_Over_Under_Score_Odds=c()
  for(Country in Countries){ # Country (start) ----
    # Country="england"
    source(paste0(CODE.dir.2, "Country_League_List.R"))
    for(League_Text in All_Leagues){
      # League_Text="premier-league"
      #*******************
      #
      # set directory path
      #
      #*******************
      data.dir_1=paste0(data.dir.1, Country, "/", League_Text, "/")
      #data.dir_2=paste0(output.dir, Country, "/", League_Text, "/")
      
      #********************************************************
      # results data to extract historical data for Kelly score
      #********************************************************
      Results_Data=Combined_Results_Data[League==League_Text, ]
      
      #*******************************************
      # !!!! optimal parameters for the league !!!
      #*******************************************
      # use optimal parameters based on Optimal_Settings
      if(Optimal_Pars=="Yes"){
        Prob_Estimate=as.character(Optimal_Settings[League==League_Text, Prob_Est])
        Chosen_Profit_Criteria=Optimal_Settings[League==League_Text, Chosen_Profit_Criterion]
        Coef=Optimal_Settings[League==League_Text, Coef]
      }
      
      #
      for(Year in Years){ # Year (start) ----
        # Year=2021
        #************
        #
        # import data
        #
        #************
        # Over_Under_Score_Odds
        #*************************
        if(file.exists(paste0(output.dir, Country, "/", League_Text, "/", Year, ".csv"))){ # if Over_Under_Score_Odds exists (start) ----
          Over_Under_Score_Odds=fread(paste0(output.dir, Country, "/", League_Text, "/", Year, ".csv"))
          Over_Under_Score_Odds[, `:=`(Chosen_Option_Empirical_Prob=as.numeric(Chosen_Option_Empirical_Prob),
                                       Chosen_Option_Std=as.numeric(Chosen_Option_Std))]
          
          if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%Y-%m-%d"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%Y-%m-%d")]
          }else if(!is.na(as.Date(Over_Under_Score_Odds$Date[1], format="%m/%d/%Y"))){
            Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]
          }
          
          #******************
          #
          # Data manipulation
          #
          #******************
          Over_Under_Score_Odds=Over_Under_Score_Odds_Manipulation(Over_Under_Score_Odds)
          
          # check -> there shouldn't any output
          sort(setdiff(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])), unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team]))))
          sort(setdiff(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])), unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team]))))
          sort(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])))
          sort(unique(c(Results_Data[, Home_Team], Results_Data[, Away_Team])))
          
          #****************
          #
          # Compute indices
          #
          #****************
          Over_Under_Score_Odds=Compute_Kelly_Scores(Over_Under_Score_Odds, Results_Data, League_Text, Prob_Estimate, Chosen_Profit_Criteria, Coef)
          head(Over_Under_Score_Odds)
          # combine
          Combined_Over_Under_Score_Odds=rbind(Combined_Over_Under_Score_Odds, Over_Under_Score_Odds, fill=T)
          
        }else{ # if Over_Under_Score_Odds exists (end) ----
          print(paste0("Country : [", Country, "], League : [", League_Text, "], Year : ", Year, " file doesn't exist."))
        }
      } # Year (end) ----
      
    }
    
  } # Country (end) ----
}
Combined_Over_Under_Score_Odds=Over_Under_Betting_Result(Combined_Over_Under_Score_Odds, Combined_Results_Data)
