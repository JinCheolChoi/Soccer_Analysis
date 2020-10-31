#assign(paste0("a", Years[1]), c(1))
#unique_date = real.data$Exact_Date %>% unique() %>% sort()
#which (real.data$Exact_Date[2] == unique_date)
#match( real.data$Exact_Date %in% unique_date[1] )
#which(unique_date %in% real.data$Exact_Date)

#############
# Import Data
#############
#Years = 2017 : 2018
real.data = c()
for(Year in Year){
  # import data of each year and combines them by rows as tibble table
  real.data = rbind(real.data, as_tibble(read.csv(paste0(Directory,"/Real_Data/", League, "/", Year, ".csv"))))
}

# Convert Exact_Date into the form of Date
real.data$Exact_Date = as.Date(ISOdate(real.data$Exact_Date %>% substr(7, 10), 
                                           real.data$Exact_Date %>% substr(4, 5), 
                                           real.data$Exact_Date %>% substr(1, 2)))

###################################################
# Create a data for analysis by tailoring real data
###################################################
analysis.data = tibble(Time = real.data$Exact_Date,
                     H.team = as.character(real.data$HomeTeam),
                     A.team = as.character(real.data$AwayTeam),
                     H.odd = real.data$Home.odds,
                     D.odd = real.data$Draw.odds,
                     A.odd = real.data$Away.odds,
                     Result = as.character(real.data$Full.time.result))

# calculate implied probability
analysis.data = est.prob.f(analysis.data)

# sort data by Time
analysis.data = mat.sort(analysis.data, 1)


#################################
# Calculate posterior probability
#################################
#analysis.data=post.prob(analysis.data)
#analysis.data$Diff=analysis.data$H.post.win.prob-analysis.data$A.post.win.prob

