## source("~/R/AtmosScales.r")
##
## Plot atmospheric scales
##
 library("fields")
 #
 # define function for nice plot axes
  pal01 = colorRampPalette(c("white", "darkblue"))
  axes.pretty <- function(xlim, ylim,
                          xticks.sm = 50, xticks.med = 10, xticks.lab = 5,
                          yticks.sm = 50, yticks.med = 10, yticks.lab = 5)
  {
    xdat = range(xlim)  # can be bounds or whole vector as well
    ydat = range(ylim)
    axis(1, at=pretty(xdat, n = xticks.sm),  labels = FALSE, tcl = -0.20)
    axis(1, at=pretty(xdat, n = xticks.med), labels = FALSE, tcl = -0.50)
    axis(1, at=pretty(xdat, n = xticks.lab), labels = FALSE)
    axis(2, at=pretty(ydat, n = yticks.sm),  labels = FALSE, tcl = -0.20)
    axis(2, at=pretty(ydat, n = yticks.med), labels = FALSE, tcl = -0.50)
    axis(2, at=pretty(ydat, n = yticks.lab), labels = FALSE)
  }
 #
 # plot 
  outeps <- sprintf('~/R/AtmosScales.eps',c(1))
  postscript(outeps,horizontal = F, paper = "special",width =6.00, height = 6)

  x <- c(10^-2,10^-1,10^0,10^1,10^2,10^3,10^4,10^5,10^6,10^7,10^8)
  y <- c(10^-2,10^-1,10^0,10^1,10^2,10^3,10^4,10^5,10^6,10^7,10^8)
  plot(x,y,xlog=T,ylog=T,xaxt="n",yaxt="n",col="white" ,xlab="Characteristic spatial scales in [m]",ylab="Characteristic time scales in [s]")
#  axis(1,at=x) #,label=c("1 cm","1 dm","1 m","10 m","100 m","1 km","10 km","100 km","1000 km","10.000 km","100.000 km"))
  axes.pretty(x,y); box()
  dev.off()
