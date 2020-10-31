# date
Data_set[, Date:=as.Date(Date, "%m/%d/%Y")]

# order by Date
Data_set=Data_set[order(Date), ]

#
Data_set_2=rbind(
  data.table(
    Date=Data_set[, Date],
    Team=Data_set[, Home_Team],
    Opponent=Data_set[, Away_Team],
    Score=Data_set[, Home_Score],
    Opponent_Score=Data_set[, Away_Score],
    Result=ifelse(Data_set[, "F_Result"]=="H", "W", 
                  ifelse(Data_set[, "F_Result"]=="D", "D", "L"))
  ),
  data.table(
    Date=Data_set[, Date],
    Team=Data_set[, Away_Team],
    Opponent=Data_set[, Home_Team],
    Score=Data_set[, Away_Score],
    Opponent_Score=Data_set[, Home_Score],
    Result=ifelse(Data_set[, "F_Result"]=="A", "W", 
                  ifelse(Data_set[, "F_Result"]=="D", "D", "L"))
  )
)
names(Data_set_2)[names(Data_set_2)=="Result.F_Result"]="Result"


