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
lapply(c("dplyr", "gam", "klaR", "ggplot2", "BioPhysConnectoR", "arrangements", "sm"), checkpackages)


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

#######################################
# compute the optimal proportion to bet
#######################################
pro.compute = function(max.loss.gain.prob, joint.odds.scalar, winning.prob){
  loss.gain.prob = c()
  k.space = seq(0.001, 0.999, by=0.001)
  for(k in k.space){
    loss.gain.prob[which(k.space == k)] = pnbinom(ceiling( ceiling(1/k) * (1/(joint.odds.scalar-1)) ), size = ceiling(1/k), prob = 1 - winning.prob, lower.tail = TRUE, log.p = FALSE)
  }
  
  optimal.k = k.space[max(which(loss.gain.prob <= max.loss.gain.prob))]
  
  return(optimal.k)
}
