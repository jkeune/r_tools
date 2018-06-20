# This function caluclates the relative entropy (Tippett et al. 2004, GRL)
# and uses function entropy in reldist package
# x is ensemble of forecasts
# y is observation
#

 relentropy <- function(x,y,ctxt,csea,xm)
 {
  dims <- dimension(x)
  ntmax = max(dims)
  nsim  = min(dims)
  ntot  = ntmax*nsim

  R=array(0,c(ntmax))
  Rgauss=array(0,c(ntmax))
  Rmc=array(0,c(ntmax))
  mu.p=array(0,c(ntmax))
  sigma.p=array(0,c(ntmax))

# estimate pdf q (climatology) and p (conditional prob.)
# calculate distribution for x with kernel density estimation
# climatology
#
 q <- density(x,kernel="gaussian")
 pdf.qx <- q$x 
 pdf.q  <- q$y 
 pdf.qxx <- (pdf.qx-min(pdf.qx))/(max(pdf.qx)-min(pdf.qx))
 pdfq <- list(x=pdf.qxx,y=pdf.q) 

# relative entropy with kernel density estimation
#
 for( i in 1:ntmax )
 {
# conditional pdf
#
  p <- density(x[i,],kernel="gaussian")
  pdf.px <- p$x
  pdf.p  <- p$y
  pdf.pxx <- (pdf.px-min(pdf.qx))/(max(pdf.qx)-min(pdf.qx))
  pdfp <- list(x=pdf.pxx,y=pdf.p) 
  R[i] <- -1*entropy(pdfp,pdfq)
 } 

# 95% significance level for relative entropy
# MC = 300
# 
 nnmc = 1000
 for( imc in 1:nnmc )
 {
  nmc <- as.integer(runif(15,min=1,max=ntot))
  pmc <- density(x[nmc],kernel="gaussian")
  pdf.mcx <- pmc$x
  pdf.mc  <- pmc$y
  pdf.mcxx <- (pdf.mcx-min(pdf.qx))/(max(pdf.qx)-min(pdf.qx))
  pdfmc <- list(x=pdf.mcxx,y=pdf.mc)
  Rmc[imc] = -1*entropy(pdfmc,pdfq)
 } 
 Rmc <- sort(Rmc)
 R95 <- Rmc[ceiling(0.95*nnmc)]
 R99 <- Rmc[ceiling(0.99*nnmc)]

# assume Gaussian distribution:
# then R is simple: R = 1/2*( log(sigma.q^2/sigma.p^2)+sigma.p^2/sigma.q^2 + mu.p^2/sigma.q^2 )  
#
 sigma.q <- var(x)
 for( i in 1:ntmax )
 {
  mu.p[i] = mean(x[i,]) 
  sigma.p[i]= var(x[i,])
  Rgauss[i] = 1/2*( log(sigma.q^2/sigma.p[i]^2)+sigma.p[i]^2/sigma.q^2 + mu.p[i]^2/sigma.q^2 )
 }

# plot(1903:2002,x[,1],type="l",xlab="",ylab="ECHAM4 + CRU",ylim=c(-3,3),col="red")
# for(i in 2:5){
#  lines(1903:2002,x[,i],col="red") }
# title("GISST+GHG")
# plot(1903:2002,x[,6],type="l",xlab="",ylab="ECHAM4 + CRU",ylim=c(-3,3),col="blue")
# for(i in 7:10){
#  lines(1903:2002,x[,i],col="blue") }
# title("GISST+GHG+SOLVOL")
# plot(1903:2002,x[,11],type="l",xlab="",ylab="ECHAM4 + CRU",ylim=c(-3,3),col="green")
# for(i in 12:15){
#  lines(1903:2002,x[,i],col="green") }
# title("GISST")

 plot(1903:2002,y,type="l",xlab="",ylab="ECHAM4 + CRU",ylim=c(-3,3))
 for(i in 1:15)
 { 
  lines(1903:2002,x[,i],col="grey")
 }
 lines(1903:2002,y,col="black")
 lines(1903:2002,xm,col="red")
 xmm = array(0,c(ntmax))
 for(i in 1:15){
  xmm = xmm + x[,i]/15
 } 
# xmm = xmm/sd(xmm)
 lines(1903:2002,xmm,col="blue")

 ctit <- sprintf('MOS ECHAM4 - %s  %s',ctxt,csea)
 cor1 = cor(xm,y)
 cor2 = cor(xmm,y)
 cor3 = cor(xm,xmm)
 csubtit <- sprintf('MOS ens-OBS: %4.2f  ens MOS=OBS: %4.2f  MOS ens-ens MOS: %4.2f',cor1,cor2,cor3)
 print(csubtit)
 title(main=ctit,sub=csubtit)
 
 plot(1903:2002,R,type='h',lwd=5,axes=T,
       ,las=1,col="grey",xlab="",ylab="relalive entropy")
 lines(c(1902,2003),c(R95,R95),col="black")
 lines(c(1902,2003),c(R99,R99),lty="dashed",col="black")

 sE = abs(y-xm)
 plot(1903:2002,sE,type="l",xlab="",ylab="OBS+rel.Entropy",ylim=c(0,3))
 lines(1903:2002,R*3,col='red')
 cor1 = cor(R,sE)
 ctit <- sprintf('OBS-rel.Entropy cor: %4.2f',cor1)
 title(ctit)

 plot(1903:2002,sE,type="l",xlab="",ylab="OBS+rel.Entropy",ylim=c(0,3))
 lines(1903:2002,sigma.p,col='red')
 cor1 = cor(sigma.p,sE)
 ctit <- sprintf('OBS-ens.var. cor: %4.2f',cor1)
 title(ctit)

# plot(1:ntmax,Rgauss,type='h',lwd=8,axes=T,
#       ,las=1,col="grey",xlab="",ylab="")
# lines(1903:2002,Rgauss,type='p')

 set.panel(3,2)
 plot(mu.p*mu.p,R,xlab="squared cond. mean",ylab="relalive entropy")
 ctit <- sprintf('cor=%2.2f',cor(mu.p*mu.p,R))
 title(ctit)
 plot(sigma.p,R,xlab="cond. variance",ylab="relalive entropy")
 ctit <- sprintf('cor=%2.2f',cor(sigma.p,R))
 title(ctit)

# set.panel(2,1)
# plot(mu.p*mu.p,Rgauss,xlab="squared cond. mean",ylab="relalive entropy")
# ctit <- sprintf('correlation: squared cond. mean - rel. entropy: %2.2f',cor(mu.p*mu.p,Rgauss))
# title(ctit)
# plot(sigma.p,Rgauss,xlab="cond. variance",ylab="relalive entropy")
# ctit <- sprintf('correlation: squared cond. variance - rel. entropy: %2.2f',cor(sigma.p,Rgauss))
# title(ctit)

 return ( list(R=R,Rgauss=Rgauss,mu=mu.p,sigma=sigma.p) )
 }
