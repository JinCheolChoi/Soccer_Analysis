Home_Odds=1.91
Draw_Odds=3.4
Away_Odds=4.25

Home_Min_Profit=0
Draw_Min_Profit=0
Away_Min_Profit=0

# algorithm
Home_Betting=Draw_Betting=Away_Betting=1 # initial betting amount for each position
loop=T
while(loop==T){
  if(floor(Home_Odds*Home_Betting*100)/100-Home_Betting - (Draw_Betting+Away_Betting) <= Home_Min_Profit){
    Home_Betting=Home_Betting+0.01
    print(paste0("Home_Betting : ", Home_Betting-0.01, " -> ", Home_Betting))
    next
  }
  if(floor(Draw_Odds*Draw_Betting*100)/100-Draw_Betting - (Home_Betting+Away_Betting) <= Draw_Min_Profit){
    Draw_Betting=Draw_Betting+0.01
    print(paste0("Draw_Betting : ", Draw_Betting-0.01, " -> ", Draw_Betting))
    next
  }
  if(floor(Away_Odds*Away_Betting*100)/100-Away_Betting - (Home_Betting+Draw_Betting) <= Away_Min_Profit){
    Away_Betting=Away_Betting+0.01
    print(paste0("Away_Betting : ", Away_Betting-0.01, " -> ", Away_Betting))
    next
  }
  
  # if((floor(Home_Odds*Home_Betting*100)/100-1 - Draw_Betting+Away_Betting > 0) &
  #    (floor(Draw_Odds*Draw_Betting*100)/100-1 - Home_Betting+Away_Betting > 0) &
  #    (floor(Away_Odds*Away_Betting*100)/100-1 - Home_Betting+Draw_Betting > 0)){
  #   loop=FALSE
  # }
  loop=FALSE
}

print(paste0("Home_Betting : ", Home_Betting))
print(paste0("Draw_Betting : ", Draw_Betting))
print(paste0("Away_Betting : ", Away_Betting))

floor(Home_Odds*Home_Betting*100)/100-Home_Betting-(Draw_Betting+Away_Betting)
floor(Draw_Odds*Draw_Betting*100)/100-Draw_Betting-(Home_Betting+Away_Betting)
floor(Away_Odds*Away_Betting*100)/100-Away_Betting-(Home_Betting+Draw_Betting)



