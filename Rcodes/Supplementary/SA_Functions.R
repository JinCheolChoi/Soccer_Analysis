#**********
# functions
#**********
Imperical_Profit=function(
    Data,
    Differences=0.01, # interval length for grouping odds into thr same bracket
    LLO=1.01, # lowest low boundary of odds brackets
    HLO=40 # highest low boundary of odds brackets
){
  # Differences=0.01
  Data=Data[!is.na(Home_Odds), ]
  
  Match_Results=c("Home", "Draw", "Away")
  Results=c()
  for(Difference_Ind in 1:length(Differences)){
    # Difference_Ind=1
    Difference=Differences[Difference_Ind]
    
    LOs=seq(LLO, HLO, by=Difference) # lower boundaries of odds brackets
    # LOs=c(11) # lower boundaries of odds brackets
    Result_Temp=matrix(NA, nrow=length(LOs), ncol=3+1)
    for(LO_Ind in 1:length(LOs)){
      # LO_Ind=100
      LO=LOs[LO_Ind]
      
      for(Match_Result_Ind in 1:length(Match_Results)){
        # Match_Result_Ind=1
        Denominator_Data=Data[eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
                                eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ]
        Denominator_N=nrow(Denominator_Data)
        
        Numerator_Data=Data[Result==Match_Results[Match_Result_Ind] &
                              eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))>=LO &
                              eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))<LO+Difference, ]
        Numerator_N=nrow(Numerator_Data)
        
        Profit=sum(Numerator_Data[, eval(parse(text=paste0(Match_Results[Match_Result_Ind], "_Odds")))])-
          Denominator_N
        
        Avg_Profit=Profit/Denominator_N
        # Avg_Profit=(LO-1)*Numerator_N/Denominator_N-1*(1-Numerator_N/Denominator_N)
        
        if(Denominator_N>=1 & Numerator_N>=1){
          # if(Avg_Profit>0){
          # if(1/(Numerator_N/Denominator_N)>LO){
          # one-sample test of proportion
          Prop_Test=prop.test(x=Numerator_N,
                              n=Denominator_N,
                              p=1/LO,
                              alternative="greater",
                              correct=FALSE)
          # temp table with results
          Results_Temp=data.table(
            Year=sum(unique(Data[, Season_Year])),
            Result=Match_Results[Match_Result_Ind],
            LO=LO,
            HO=LO+Difference,
            Odds_Interval=Difference,
            Numerator_N=Numerator_N,
            Denominator_N=Denominator_N,
            Win_Prob=round(Numerator_N/Denominator_N, 3),
            Thresh_Odds=round(1/(Numerator_N/Denominator_N), 3),
            P_Value=round(Prop_Test$p.value, 3),
            Profit=round(Profit, 3),
            Avg_Profit=round(Avg_Profit, 3),
            Kelly=round(Numerator_N/Denominator_N-(1-Numerator_N/Denominator_N)/(LO-1), 3)
          )
          
          # record results
          Results=rbind(Results,
                        Results_Temp)
          # }
        }
      }
    }
  }
  
  return(Results)
}

# #****************************************************
# # plot the sequence of bankrolls with Kelly-criterion
# #****************************************************
# # method-1
# Odds=5.51
# Win_Prob=0.193
# Kelly=Win_Prob-(1-Win_Prob)/(Odds-1)
# 
# Current_Net=Capital=10
# for(i in 1:3000){
#   Result=sample(c(0, 1),
#                 1,
#                 prob=c(1-Win_Prob, Win_Prob))
#   Bet=tail(Current_Net, 1)*Kelly
#   if(Bet>0){
#     # Current_Net=Current_Net-Bet+Result*Odds*Bet
#     Current_Net=c(Current_Net, tail(Current_Net, 1)+Bet*(Result*Odds-1))
#   }
# }
# tail(Current_Net, 1)
# plot(Current_Net)
# 
# 
# # method-2
# Length=3000
# # Odds=abs(rnorm(Length))+1
# # Win_Prob=runif(Length)
# Odds=rep(5.51, length=Length)
# Win_Prob=rep(0.193, length=Length)
# Kelly=Win_Prob-(1-Win_Prob)/(Odds-1)
# Result=sapply(Win_Prob,
#               function(x){
#                 sample(c(0, 1),
#                        1,
#                        replace=TRUE,
#                        prob=c(1-x, x))})
# 
# Prof=1-Kelly[Kelly>0]+Kelly[Kelly>0]*Odds[Kelly>0] # x1=(x0+(Odds-1)*k)=x0*(1-k+k*Odds)
# Prof[Result[Kelly>0]==0]=1-Kelly[Kelly>0][Result[Kelly>0]==0] # x1=(x0-k*x0)=x0*(1-k)
# tail(cumprod(Prof), 1)
# plot(cumprod(Prof))
