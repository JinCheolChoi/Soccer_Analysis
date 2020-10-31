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
Country="england"
League="premier-league" # premier-league, championship, league-one, league-two, national-league

# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
# data.dir=paste0("C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Data/", Country, "/", League, "/")
# data.dir_2=paste0("C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/", Country, "/")

CODE.dir.1="C:/Users/jchoi02/Filr/My Files/JinCheol/R code/Functions/"
CODE.dir.2="C:/Users/jchoi02/Filr/My Files/Desktop/R/Soccer_Analysis/"
data.dir=paste0("C:/Users/jchoi02/Filr/My Files/Desktop/R/Soccer_Analysis/Data/", Country, "/", League, "/")
data.dir_2=paste0("C:/Users/jchoi02/Filr/My Files/Desktop/R/Soccer_Analysis/Over_under_score_odds_data/", Country, "/")

#***************
#
# import library
#
#***************
source(paste0(CODE.dir.1, "Functions.R"))
lapply(c("dplyr", 
         "data.table",
         "profvis",
         
         "ggplot2"
         
), 
checkpackages)

#************
#
# import data
#
#************
# Data_to_use
File_Lists=list.files(data.dir)
Data_to_use=c()
for(File_to_Open in File_Lists){
  Data_to_use=rbind(fread(paste0(data.dir, File_to_Open)), Data_to_use)
}

# Over_Under_Score_Odds
Over_Under_Score_Odds=fread(paste0(data.dir_2, "/", League, ".csv"))
Over_Under_Score_Odds[, Date:=as.Date(Date, format="%m/%d/%Y")]

#*************************
# manipulate 'Data_to_use'
#*************************
# Date
Data_to_use[, Exact_Date:=as.Date(Date, format="%d.%m.%Y")]
Data_to_use[is.na(Exact_Date), Exact_Date:=as.Date(paste0(Date, Season_Year), format="%d.%m.%Y")]
Data_to_use[Date=="Yesterday", Exact_Date:=as.Date(format(Sys.Date()-1,"%Y-%m-%d"))]

# Total_Goal
Data_to_use[, Home_Goal:=as.numeric(Home_Goal)]
Data_to_use[, Away_Goal:=as.numeric(Away_Goal)]

Data_to_use[, Total_Goal:=Home_Goal+Away_Goal]
Data_to_use[, Total_Goal] %>% mean
Data_to_use[, Total_Goal] %>% var

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

# check -> there shouldn't any output
setdiff(unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])), unique(c(Data_to_use[, Home_Team], Data_to_use[, Away_Team])))
setdiff(unique(c(Data_to_use[, Home_Team], Data_to_use[, Away_Team])), unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team])))

unique(c(Over_Under_Score_Odds[, Home_Team], Over_Under_Score_Odds[, Away_Team]))
unique(c(Data_to_use[, Home_Team], Data_to_use[, Away_Team]))

# total goal
Over_Under_Score_Odds=Over_Under_Score_Odds %>% 
  left_join(Data_to_use, by=c("Date"="Exact_Date", "Home_Team", "Away_Team")) %>% 
  dplyr::select(-Date.y) %>% 
  as.data.table

Over_Under_Score_Odds[4, Home_Goal:=1]
Over_Under_Score_Odds[4, Away_Goal:=1]

Over_Under_Score_Odds[5, Home_Goal:=0]
Over_Under_Score_Odds[5, Away_Goal:=2]

Over_Under_Score_Odds[6, Home_Goal:=2]
Over_Under_Score_Odds[6, Away_Goal:=1]

Over_Under_Score_Odds[7, Home_Goal:=5]
Over_Under_Score_Odds[7, Away_Goal:=0]

Over_Under_Score_Odds[8, Home_Goal:=3]
Over_Under_Score_Odds[8, Away_Goal:=0]

Over_Under_Score_Odds[9, Home_Goal:=0]
Over_Under_Score_Odds[9, Away_Goal:=3]

Over_Under_Score_Odds[10, Home_Goal:=1]
Over_Under_Score_Odds[10, Away_Goal:=2]

Over_Under_Score_Odds[, Season_Year:=2020]
Over_Under_Score_Odds[, Total_Goal:=Home_Goal+Away_Goal]
#***************
#
# Temp Functions
#
#***************
Kelly_Criterion=function(p, b){p-(1-p)/(b-1)}



Kelly_Criterion_Calculator_Negative_Binom=function(x){
  
  # Mean=Data_to_use[Home_Team==x["Home_Team"]&
  #                 Away_Team==x["Away_Team"], Total_Goal] %>% mean
  # Var=Data_to_use[Home_Team==x["Home_Team"]&
  #                Away_Team==x["Away_Team"], Total_Goal] %>% var
  
  Mean=Data_to_use[, Total_Goal] %>% mean
  Var=Data_to_use[, Total_Goal] %>% var
  
  return(
    data.table(
      x["Date"],
      x["League"],
      x["Home_Team"],
      x["Away_Team"],
      
      Kelly_Criterion((1-pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 0.5"])),
      Kelly_Criterion((pnbinom(0.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 0.5"])),
      
      Kelly_Criterion((1-pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 1.5"])),
      Kelly_Criterion((pnbinom(1.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 1.5"])),
      
      Kelly_Criterion((1-pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 2.5"])),
      Kelly_Criterion((pnbinom(2.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 2.5"])),
      
      Kelly_Criterion((1-pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 3.5"])),
      Kelly_Criterion((pnbinom(3.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 3.5"])),
      
      Kelly_Criterion((1-pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 4.5"])),
      Kelly_Criterion((pnbinom(4.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 4.5"])),
      
      Kelly_Criterion((1-pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Over 5.5"])),
      Kelly_Criterion((pnbinom(5.5, size=Mean^2/(Var-Mean), mu=Mean)), as.numeric(x["Under 5.5"]))
    )
  )
}

Kelly_Criterion_Calculator_Exact_Dist=function(x){
  
  return(
    data.table(
      x["Date"],
      x["League"],
      x["Home_Team"],
      x["Away_Team"],
      
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<0.5)), as.numeric(x["Over 0.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<0.5), as.numeric(x["Under 0.5"])),
      #
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<1.5)), as.numeric(x["Over 1.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<1.5), as.numeric(x["Under 1.5"])),
      #
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<2.5)), as.numeric(x["Over 2.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<2.5), as.numeric(x["Under 2.5"])),
      #
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<3.5)), as.numeric(x["Over 3.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<3.5), as.numeric(x["Under 3.5"])),
      #
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<4.5)), as.numeric(x["Over 4.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<4.5), as.numeric(x["Under 4.5"])),
      #
      # Kelly_Criterion((1-mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<5.5)), as.numeric(x["Over 5.5"])),
      # Kelly_Criterion(mean(Data_to_use[Home_Team==x["Home_Team"]&Away_Team==x["Away_Team"], Total_Goal]<5.5), as.numeric(x["Under 5.5"]))

      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<0.5)), as.numeric(x["Over 0.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<0.5), as.numeric(x["Under 0.5"])),
      
      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<1.5)), as.numeric(x["Over 1.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<1.5), as.numeric(x["Under 1.5"])),
      
      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<2.5)), as.numeric(x["Over 2.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<2.5), as.numeric(x["Under 2.5"])),
      
      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<3.5)), as.numeric(x["Over 3.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<3.5), as.numeric(x["Under 3.5"])),
      
      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<4.5)), as.numeric(x["Over 4.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<4.5), as.numeric(x["Under 4.5"])),
      
      Kelly_Criterion((1-mean(Data_to_use[, Total_Goal]<5.5)), as.numeric(x["Over 5.5"])),
      Kelly_Criterion(mean(Data_to_use[, Total_Goal]<5.5), as.numeric(x["Under 5.5"]))
    )
  )
}


Kelly_Criterion_Calculator_Poisson=function(x){
  
  # Mean=Data_to_use[Home_Team==x["Home_Team"]&
  #                 Away_Team==x["Away_Team"], Total_Goal] %>% mean
  # Var=Data_to_use[Home_Team==x["Home_Team"]&
  #                Away_Team==x["Away_Team"], Total_Goal] %>% var
  
  Mean=Data_to_use[, Total_Goal] %>% mean
  Var=Data_to_use[, Total_Goal] %>% var
  
  return(
    data.table(
      x["Date"],
      x["League"],
      x["Home_Team"],
      x["Away_Team"],
      
      Kelly_Criterion((1-ppois(0.5, lambda=Mean)), as.numeric(x["Over 0.5"])),
      Kelly_Criterion((ppois(0.5, lambda=Mean)), as.numeric(x["Under 0.5"])),
      
      Kelly_Criterion((1-ppois(1.5, lambda=Mean)), as.numeric(x["Over 1.5"])),
      Kelly_Criterion((ppois(1.5, lambda=Mean)), as.numeric(x["Under 1.5"])),
      
      Kelly_Criterion((1-ppois(2.5, lambda=Mean)), as.numeric(x["Over 2.5"])),
      Kelly_Criterion((ppois(2.5, lambda=Mean)), as.numeric(x["Under 2.5"])),
      
      Kelly_Criterion((1-ppois(3.5, lambda=Mean)), as.numeric(x["Over 3.5"])),
      Kelly_Criterion((ppois(3.5, lambda=Mean)), as.numeric(x["Under 3.5"])),
      
      Kelly_Criterion((1-ppois(4.5, lambda=Mean)), as.numeric(x["Over 4.5"])),
      Kelly_Criterion((ppois(4.5, lambda=Mean)), as.numeric(x["Under 4.5"])),
      
      Kelly_Criterion((1-ppois(5.5, lambda=Mean)), as.numeric(x["Over 5.5"])),
      Kelly_Criterion((ppois(5.5, lambda=Mean)), as.numeric(x["Under 5.5"]))
    )
  )
}

#Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Negative_Binom(x)))
Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Exact_Dist(x)))
#Over_Under_Score_Kelly=do.call(rbind.data.frame, apply(Over_Under_Score_Odds, 1, function(x) Kelly_Criterion_Calculator_Poisson(x)))

colnames(Over_Under_Score_Kelly)=colnames(Over_Under_Score_Odds)[1:16]
Over_Under_Score_Profit=cbind(Over_Under_Score_Odds[, 1:4],
                              Over_Under_Score_Odds[, 5:16]*Over_Under_Score_Kelly[, 5:16]-Over_Under_Score_Kelly[, 5:16])

Over_Under_Score_Result=data.table(
  Over_Under_Score_Odds[, 1:4],
  `Over 0.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>0.5, 1, 0),
  `Under 0.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<0.5, 1, 0),
  `Over 1.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>1.5, 1, 0),
  `Under 1.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<1.5, 1, 0),
  `Over 2.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>2.5, 1, 0),
  `Under 2.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<2.5, 1, 0),
  `Over 3.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>3.5, 1, 0),
  `Under 3.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<3.5, 1, 0),
  `Over 4.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>4.5, 1, 0),
  `Under 4.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<4.5, 1, 0),
  `Over 5.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]>5.5, 1, 0),
  `Under 5.5`=ifelse(Over_Under_Score_Odds[, Total_Goal]<5.5, 1, 0)
)

Over_Under_Score_Odds
Over_Under_Score_Kelly
Over_Under_Score_Profit
Over_Under_Score_Result

Capital=1000
# algorithm
for(i in 1:nrow(Over_Under_Score_Profit)){
  
  #*************************************************
  # Chosen_Profit Criterion - (1) : by maximum profit
  #*************************************************
  #Chosen_Profit=unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)][which.max(unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)])]
  
  #*************************************************
  # Chosen_Profit Criterion - (2) : by minimum profit
  #*************************************************
  #Chosen_Profit=unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)][which.min(unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)])]
  
  #***************************************************************************************************************************
  # Chosen_Profit Criterion - (3) : by a larger profit between the lowest option of "Over"s and the highest option of "Under"s
  #***************************************************************************************************************************
  Temp=unlist(Over_Under_Score_Profit[i, 5:16])[which(Over_Under_Score_Profit[i, 5:16]>0)] # consider only upto Under 5.5
  Over_Selected_Temp=grep("Over", names(Temp))
  Under_Selected_Temp=grep("Under", names(Temp))
  
  if(length(Over_Selected_Temp)+length(Under_Selected_Temp)>0){ # bet only if there is positive expected profit
    if(length(Over_Selected_Temp)>0){
      Over_Selected=c()
      Under_Selected=-100
      Over_Selected=Temp[min(Over_Selected_Temp)]
    }
    if(length(Under_Selected_Temp)>0){
      Under_Selected=c()
      Over_Selected=-100
      Under_Selected=Temp[max(Under_Selected_Temp)]
    }
    Chosen_Profit=ifelse(Over_Selected>=Under_Selected, Over_Selected, Under_Selected)
    
    #
    Over_Under_Score_Odds[i, `Chosen_Option`:=names(Chosen_Profit)]
    Over_Under_Score_Odds[i, `Chosen_Odds`:=Over_Under_Score_Odds[i, .SD, .SDcols=names(Chosen_Profit)]]
    Over_Under_Score_Odds[i, Kelly:=Over_Under_Score_Kelly[i, .SD, .SDcols=names(Chosen_Profit)]]
    Over_Under_Score_Odds[i, Result:=Over_Under_Score_Result[i, .SD, .SDcols=names(Chosen_Profit)]]
    
  }
}

#*******************************************
#
# Calculate balance based on chosen bettings
#
#*******************************************
#Over_Under_Score_Odds=Over_Under_Score_Odds[sample(1:nrow(Over_Under_Score_Odds)), ]
Over_Under_Score_Odds=Over_Under_Score_Odds[!is.na(Chosen_Odds), ]
for(i in 1:nrow(Over_Under_Score_Odds)){
  if(i==1){
    Over_Under_Score_Odds[i, Bet:=round(Capital*Kelly, 0)]
    Over_Under_Score_Odds[i, Balance:=ifelse(Result==1, Bet*(Chosen_Odds-1)+Capital, Capital-Bet)]
  }else if(i>1){
    Over_Under_Score_Odds[i, Bet:=round(Over_Under_Score_Odds[i-1, Balance]*Kelly, 0)]
    Over_Under_Score_Odds[i, Balance:=ifelse(Result==1, Bet*(Chosen_Odds-1)+Over_Under_Score_Odds[i-1, Balance], Over_Under_Score_Odds[i-1, Balance]-Bet)]
  }
}

Unique_Dates=sort(unique(Over_Under_Score_Odds[, Date]))
for(Dat_Ind in 1:length(Unique_Dates)){
  #Dat="2020-03-08"
  Dat=Unique_Dates[Dat_Ind]
  Temp_Over_Under_Score_Odds=Over_Under_Score_Odds[Date==Dat, ]
  
  #
  setorderv(Temp_Over_Under_Score_Odds, "Kelly", -1) # order rows by Kelly
  
  #
  if(Dat==min(Unique_Dates)){
    for(i in 1:nrow(Temp_Over_Under_Score_Odds)){
      if(i==1){
        Temp_Over_Under_Score_Odds[i, Bet:=round(Capital*Kelly, 0)]
        Temp_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        Temp_Over_Under_Score_Odds[i, Bet:=round((Capital-Temp_Over_Under_Score_Odds[i-1, Bet])*Kelly, 0)]
        Temp_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }
    }
    Total_Profit=sum(Temp_Over_Under_Score_Odds[, Profit]) # profit
    Temp_Over_Under_Score_Odds[, Balance:=Capital+Total_Profit] # balance
  }else{ # if Data!=min(Unique_Dates)
    for(i in 1:nrow(Temp_Over_Under_Score_Odds)){
      if(i==1){
        Temp_Over_Under_Score_Odds[i, Bet:=round(Capital*Kelly, 0)]
        Temp_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }else if(i>1){
        Temp_Over_Under_Score_Odds[i, Bet:=round((Capital-Temp_Over_Under_Score_Odds[i-1, Bet])*Kelly, 0)]
        Temp_Over_Under_Score_Odds[i, Profit:=ifelse(Result==1, Bet*(Chosen_Odds-1), -Bet)]
      }
    }
    Total_Profit=sum(Temp_Over_Under_Score_Odds[, Profit]) # profit
    Temp_Over_Under_Score_Odds[, Balance:=Capital+Total_Profit] # balance
  }
  
  
}


Over_Under_Score_Odds
Over_Under_Score_Odds

for(i in 1:nrow(Over_Under_Score_Odds)){
  if(i==1){
    Over_Under_Score_Odds[i, Balance:=ifelse(Result==1, Capital*Kelly*(Chosen_Odds-1)+Capital, Capital-Capital*Kelly)]
  }else if(i>1){
    Over_Under_Score_Odds[i, Balance:=ifelse(Result==1, Over_Under_Score_Odds[i-1, Balance]*Kelly*(Chosen_Odds-1)+Over_Under_Score_Odds[i-1, Balance], Over_Under_Score_Odds[i-1, Balance]-Over_Under_Score_Odds[i-1, Balance]*Kelly)]
  }
}


