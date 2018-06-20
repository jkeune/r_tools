# source("~/R/lehre/plot_cdf.r")
# plotte pdf und cdf von
# beta-Verteilung
# Normalverteilung
# Gamma-Verteilung
 rm(list=ls())


# Gamma
 x <- seq(0.1,120.,by=0.02)
 p <- seq(0.001,0.999,by=0.001)
 x <- qgamma(p, shape=5, scale=8)
 cdf <-  pgamma(x, shape=5, scale=8)

 q0 <- qgamma(0.001, shape=5, scale=8)
 q99 <- qgamma(0.99, shape=5, scale=8)
 q90 <- qgamma(0.90, shape=5, scale=8)
 q95 <- qgamma(0.95, shape=5, scale=8)
 q75 <- qgamma(0.75, shape=5, scale=8)
 q50 <- qgamma(0.50, shape=5, scale=8)
 q25 <- qgamma(0.25, shape=5, scale=8)
 q10 <- qgamma(0.10, shape=5, scale=8)
 q05 <- qgamma(0.05, shape=5, scale=8)
 q01 <- qgamma(0.01, shape=5, scale=8)

# oeffne postscript-file
#
 postscript("~/R/lehre/cdfGamma.eps",horizontal = F, onefile = FALSE
           ,paper = "special",width = 7.0, height = 7)
# X11()
 par(mfrow = c(3, 2),lend="butt",ljoin="mitre",mar=c(2,2,3,1),mgp=c(2,1,0))

 plot(x,cdf ,type='l')
 lines(c(q0,q99),c(0.99,0.99),type="l",col="gray")
 lines(c(q0,q95),c(0.95,0.95),type="l",col="gray")
 lines(c(q0,q90),c(0.90,0.90),type="l",col="gray")
 lines(c(q0,q75),c(0.75,0.75),type="l",col="gray")
 lines(c(q0,q50),c(0.50,0.50),type="l",col="gray")
 lines(c(q0,q25),c(0.25,0.25),type="l",col="gray")
 lines(c(q0,q10),c(0.10,0.10),type="l",col="gray")

 lines(c(q99,q99),c(0,0.99),type="l",col="gray")
 lines(c(q95,q95),c(0,0.95),type="l",col="gray")
 lines(c(q90,q90),c(0,0.90),type="l",col="gray")
 lines(c(q75,q75),c(0,0.75),type="l",col="gray")
 lines(c(q50,q50),c(0,0.50),type="l",col="gray")
 lines(c(q25,q25),c(0,0.25),type="l",col="gray")
 lines(c(q10,q10),c(0,0.10),type="l",col="gray")
 dev.off()
 
# oeffne postscript-file
#
 postscript("~/R/lehre/qGamma.eps",horizontal = F, onefile = FALSE
           ,paper = "special",width = 4.0, height = 4)
# X11()
 par(mfrow = c(1, 1),lend="butt",ljoin="mitre",mar=c(2,2,1,1),mgp=c(2,1,0))

 plot(cdf,x ,type='l', xlim=c(-0.2,1))
 lines(c(0.99,0.99),c(q0,q99),type="l",col="gray")
 lines(c(0.95,0.95),c(q0,q95),type="l",col="gray")
 lines(c(0.95,0.95),c(q0,q90),type="l",col="gray")
 lines(c(0.75,0.75),c(q0,q75),type="l",col="gray")
 lines(c(0.50,0.50),c(q0,q50),type="l",col="gray")
 lines(c(0.25,0.25),c(q0,q25),type="l",col="gray")
 lines(c(0.10,0.10),c(q0,q10),type="l",col="gray")
 lines(c(0.05,0.05),c(q0,q05),type="l",col="gray")
 lines(c(0.01,0.01),c(q0,q01),type="l",col="gray")

 lines(c(0,0.99),c(q99,q99),type="l",col="gray")
 lines(c(0,0.95),c(q95,q95),type="l",col="gray")
 lines(c(0,0.90),c(q90,q90),type="l",col="gray")
 lines(c(0,0.75),c(q75,q75),type="l",col="gray")
 lines(c(0,0.50),c(q50,q50),type="l",col="gray")
 lines(c(0,0.25),c(q25,q25),type="l",col="gray")
 lines(c(0,0.10),c(q10,q10),type="l",col="gray")
 lines(c(0,0.05),c(q05,q05),type="l",col="gray")
 lines(c(0,0.01),c(q01,q01),type="l",col="gray")

 x1 <- -0.16
 x2 <- -0.13
 x3 <- -0.19
 x22 <- -0.14
 x33 <- -0.18
 x222 <- -0.15
 x333 <- -0.17

 lines(c(x1,x1),c(q75,q90),type="l",lty=1,col="red")
 lines(c(x1,x1),c(q95,q90),type="l",lty=2,col="red")
 lines(c(x1,x1),c(q95,q99),type="l",lty=3,col="red")

 lines(c(x2,x3),c(q90,q90),type="l",lty=1,col="red")
 lines(c(x2,x3),c(q50,q50),type="l",lty=1,col="red")
 lines(c(x22,x33),c(q95,q95),type="l",lty=1,col="red")
 lines(c(x222,x333),c(q99,q99),type="l",lty=1,col="red")

 lines(c(x2,x2),c(q25,q75),type="l",lty=1,col="red")
 lines(c(x3,x3),c(q25,q75),type="l",lty=1,col="red")
 lines(c(x2,x3),c(q25,q25),type="l",lty=1,col="red")
 lines(c(x2,x3),c(q75,q75),type="l",lty=1,col="red")

 lines(c(x1,x1),c(q25,q10),type="l",lty=1,col="red")
 lines(c(x1,x1),c(q05,q10),type="l",lty=2,col="red")
 lines(c(x1,x1),c(q05,q01),type="l",lty=3,col="red")

 lines(c(x2,x3),c(q10,q10),type="l",lty=1,col="red")
 lines(c(x22,x33),c(q05,q05),type="l",lty=1,col="red")
 lines(c(x222,x333),c(q01,q01),type="l",lty=1,col="red")

 text(c(-0.09),c(q01,q05,q10,q25,q50,q75,q90,q95,q99),c(".01",".05",".10",".25",".50",".75",".90",".95",".99"),cex=0.8)

 dev.off()
 
