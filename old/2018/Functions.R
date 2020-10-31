###############
# checkpackages
###############
# dplyr
# gam
# BioPhysConnectoR  : for "mat.sort"
checkpackages=function(package){
  # Checking the Availability of packages 
  # Installs them.  
  # example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package, character.only =T)
}
lapply(c("dplyr", "gam", "klaR", "ggplot2", "arrangements", "sm"), checkpackages)
#"BioPhysConnectoR"

############
# est.prob.f
############
# compute 'implied probability', which is 1/odd
est.prob.f = function(database){
  database$H.win.prob=1/database$H.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  database$D.win.prob=1/database$D.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  database$A.win.prob=1/database$A.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  
  return(database)
}

