###############
# checkpackages
###############
# dplyr
# gam
checkpackages=function(package){
  # Checking the Availability of packages 
  # Installs them.  
  # example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package, character.only =T)
}
lapply(c("dplyr", "gam", "klaR"), checkpackages)


############
# est.prob.f
############
# compute 'implied probability', which is 1/odd
est.prob.f=function(database){
  database$H.win.prob=1/database$H.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  database$D.win.prob=1/database$D.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  database$A.win.prob=1/database$A.odd/(1/database$H.odd+1/database$D.odd+1/database$A.odd)
  
  return(database)
}

#################
# generate result
#################
gen.result=function(database, a, b, c){
  sample(c("H","D","A"), 1, 
         prob=c(database[a],database[b],database[c]))
}

#################
# kelly criterion
#################
kelly.criterion = function(b,p){
  q=1-p
  (b*p-q)/b
}

################
# test criterion
################
test.criterion = function(b,p){
  q=1-p
  p-(q/(b))
}

############################
# plot + confidence interval
############################
conf.plot=function(Rate.n, Nrep){
  mean.trend=mean.mat[,Rate.n]
  upper.trend=mean.mat[,Rate.n]+qnorm(0.975, mean=0, sd=1)*sd.mat[,Rate.n]/sqrt(Nrep)
  lower.trend=mean.mat[,Rate.n]-qnorm(0.975, mean=0, sd=1)*sd.mat[,Rate.n]/sqrt(Nrep)
  
  mean.trend[which(mean.trend < 0)]=0
  upper.trend[which(upper.trend < 0)]=0
  lower.trend[which(lower.trend < 0)]=0
  
  plot(mean.trend, type='l', ylim=c(0, max(upper.trend)))
  lines(upper.trend, col="blue")
  lines(lower.trend, col="red")
}

###########
# post.prob
###########
# estimate winning probability from beysian approach
post.prob=function(analysis.data){
  # prior distribution
  prior.H=sum(analysis.data$Result=="H")/length(analysis.data$Result)
  prior.D=sum(analysis.data$Result=="D")/length(analysis.data$Result)
  prior.A=sum(analysis.data$Result=="A")/length(analysis.data$Result)
  
  # evidence
  evidence.H=approxfun(density(analysis.data$H.win.prob, kernel="gaussian", bw=0.02))
  evidence.D=approxfun(density(analysis.data$D.win.prob, kernel="gaussian", bw=0.005))
  evidence.A=approxfun(density(analysis.data$A.win.prob, kernel="gaussian", bw=0.02))
  
  # likelihood conditional on H
  likelihood.H.H=approxfun(density(analysis.data$H.win.prob[which(analysis.data$Result=="H")], kernel="gaussian", bw=0.02))
  likelihood.D.H=approxfun(density(analysis.data$D.win.prob[which(analysis.data$Result=="H")], kernel="gaussian", bw=0.005))
  likelihood.A.H=approxfun(density(analysis.data$A.win.prob[which(analysis.data$Result=="H")], kernel="gaussian", bw=0.02))
  
  # likelihood conditional on D
  likelihood.H.D=approxfun(density(analysis.data$H.win.prob[which(analysis.data$Result=="D")], kernel="gaussian", bw=0.02))
  likelihood.D.D=approxfun(density(analysis.data$D.win.prob[which(analysis.data$Result=="D")], kernel="gaussian", bw=0.005))
  likelihood.A.D=approxfun(density(analysis.data$A.win.prob[which(analysis.data$Result=="D")], kernel="gaussian", bw=0.02))
  
  # likelihood conditional on A
  likelihood.H.A=approxfun(density(analysis.data$H.win.prob[which(analysis.data$Result=="A")], kernel="gaussian", bw=0.02))
  likelihood.D.A=approxfun(density(analysis.data$D.win.prob[which(analysis.data$Result=="A")], kernel="gaussian", bw=0.005))
  likelihood.A.A=approxfun(density(analysis.data$A.win.prob[which(analysis.data$Result=="A")], kernel="gaussian", bw=0.02))
  
  analysis.data=analysis.data
  for(i in 1:nrow(analysis.data)){
    H.value=analysis.data$H.win.prob[i]
    D.value=analysis.data$D.win.prob[i]
    A.value=analysis.data$A.win.prob[i]
    
    evidence=(evidence.H(analysis.data$H.win.prob[i])*evidence.D(analysis.data$D.win.prob[i])*evidence.A(analysis.data$A.win.prob[i]))
    
    likelihood.H=likelihood.H.H(H.value)*likelihood.D.H(D.value)*likelihood.A.H(A.value)
    likelihood.D=likelihood.H.D(H.value)*likelihood.D.D(D.value)*likelihood.D.D(A.value)
    likelihood.A=likelihood.H.A(H.value)*likelihood.D.A(D.value)*likelihood.A.A(A.value)
    
    post.prob.H=prior.H*likelihood.H/evidence
    post.prob.D=prior.D*likelihood.D/evidence
    post.prob.A=prior.A*likelihood.A/evidence
    
    if(is.na(post.prob.H)){post.prob.H=0}
    if(is.na(post.prob.D)){post.prob.D=0}
    if(is.na(post.prob.A)){post.prob.A=0}
    
    adj.post.prob.H=post.prob.H/(post.prob.H+post.prob.D+post.prob.A)
    adj.post.prob.D=post.prob.D/(post.prob.H+post.prob.D+post.prob.A)
    adj.post.prob.A=post.prob.A/(post.prob.H+post.prob.D+post.prob.A)
    
    analysis.data$H.post.win.prob[i]=adj.post.prob.H
    analysis.data$D.post.win.prob[i]=adj.post.prob.D
    analysis.data$A.post.win.prob[i]=adj.post.prob.A
  }
  
  return(analysis.data)
}

