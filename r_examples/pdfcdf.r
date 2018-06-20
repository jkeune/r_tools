# source("~/R/lehre/pdfcdf.r")
# plotte pdf und cdf von
# beta-Verteilung
# Normalverteilung
# Gamma-Verteilung
 library(evd)

 rm(list=ls())

# oeffne postscript-file
#
 postscript("~/R/lehre/pdfcdf.eps",horizontal = F, onefile = FALSE
           ,paper = "special",width = 7.0, height = 7)
# X11()
 par(mfrow = c(3, 2),lend="butt",ljoin="mitre",mar=c(2,2,3,1),mgp=c(2,1,0))


# beta
 x <- seq(0.01,0.99,by=0.01)
 pdfbeta <-  dbeta(x, shape1=0.7, shape2=0.5, ncp=0, log = FALSE)
 cdfbeta <-  pbeta(x, shape1=0.7, shape2=0.5, ncp=0, lower.tail = T, log.p = FALSE)

 plot(x,pdfbeta ,type='l')
 title("BETA-Verteilung mit p=0.7, q=0.5: PDF")
 plot(x,cdfbeta, ,type='l')
 title("BETA-Verteilung mit p=0.7, q=0.5: CDF")


# Gauss
 x <- seq(-3,3,by=0.1)
 pdfnorm <- dnorm(x, 0, 1)
 cdfnorm <- pnorm(x, 0, 1)

 plot(x,pdfnorm ,type='l')
 title("Standard-Normalverteilung: PDF")
 plot(x,cdfnorm,type='l')
 title("Standard-Normalverteilung: CDF")

# Gamma
 x <- seq(0,3,by=0.03)
 pdfgamma <- dgamma(x, shape=2, scale = 0.3, log = FALSE)
 cdfgamma <- pgamma(x, shape=2, scale = 0.3)

 plot(x,pdfgamma,type='l')
 title("GAMMA-Verteilung mit a=2, b=0.3: PDF")
 plot(x,cdfgamma,type='l')
 title("GAMMA-Verteilung mit a=2, b=0.3: CDF")

 dev.off()

 postscript("~/R/lehre/tails.eps",horizontal = T, onefile = FALSE
            ,paper = "a4")
# X11()
 par(mfrow = c(1, 2),lend="butt",ljoin="mitre",mar=c(3,3,5,3),mgp=c(2,1,0))

 x <- seq(0.03,6,by=0.03)
 pdfgpd <- dgpd(x, loc=0, scale=1, shape=0)
 
 plot(x,log(pdfgpd),type='l')
 title("GPD density loc=0, scale=1, shape=0")

 x <- seq(10.03,16,by=0.03)
 pdfnorm <- dnorm(x, 0, 1)

 plot(x,log(pdfnorm),type='l')
 title("Standard normal distribution - logarithmic tail")

 dev.off()
 
