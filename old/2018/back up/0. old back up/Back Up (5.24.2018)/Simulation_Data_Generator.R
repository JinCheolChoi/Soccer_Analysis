########################
# win-draw-win (2 picks)
########################
### generate database for simulation
sim.data=c()
sim.data$Time=rep(1:(365*n.year),each=ave.n.game.d)
sim.data$H.team=sample(letters, 365*ave.n.game.d, replace = TRUE)
sim.data$A.team=sample(LETTERS, 365*ave.n.game.d, replace = TRUE)
sim.data$H.odd=round(1+rgamma(365*n.year*ave.n.game.d, 1.6*0.7, 0.7) ,digits=2)
sim.data$D.odd=round(3+rgamma(365*n.year*ave.n.game.d, 0.6*2, 2), digits=2)
sim.data$A.odd=round(1+rgamma(365*n.year*ave.n.game.d, 3.6*0.5, 0.5), digits=2)
#sim.data$H.odd=round(1+abs(rnorm(365*n.year*ave.n.game.d)) ,digits=2)
#sim.data$D.odd=round(3+abs(rnorm(365*n.year*ave.n.game.d)), digits=2)
#sim.data$A.odd=round(2+abs(rnorm(365*n.year*ave.n.game.d)), digits=2)

sim.data=as.data.frame(sim.data)

######################
# estimate probability
######################
sim.data=est.prob.f(sim.data)

#######################################################
# generate pseudo result based on estimated probability
#######################################################
sim.data$Result=apply(sim.data, 1, gen.result, a="H.win.prob", b="D.win.prob", c="A.win.prob")

##################
# simultation data
##################
analysis.data=sim.data

