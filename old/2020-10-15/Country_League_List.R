if(Country=="england"){
  All_Leagues=c("premier-league",
                "championship",
                "league-one",
                "league-two")
  All_Leagues=All_Leagues[All_Leagues%in%c(Leagues)]
}else if(Country=="spain"){
  All_Leagues=c("laliga")
  All_Leagues=All_Leagues[All_Leagues%in%Leagues]
}else if(Country=="italy"){
  All_Leagues=c("serie-a")
  All_Leagues=All_Leagues[All_Leagues%in%Leagues]
}else if(Country=="netherlands"){
  All_Leagues=c("eerste-divisie")
  All_Leagues=All_Leagues[All_Leagues%in%Leagues]
}else if(Country=="germany"){
  All_Leagues=c("3-liga")
  All_Leagues=All_Leagues[All_Leagues%in%Leagues]
}
