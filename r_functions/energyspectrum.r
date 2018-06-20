energyspectrum = function(u,v,Nx,Ny,delta_x){

# u : 2D array U-WIND with dim(Nx,NY)
# v : 2D array V-WIND with dim(Nx,NY)
# Nx: dim(x)
# Ny: dim(y)
# delta_x: grid-spacing in m [e.g., 12500]

##------------------------------------------------
##         1) Spektrum fuer die u-Komponente
##------------------------------------------------
aij.small <- u

## Detrending des Feldes aij
i <- seq(1,Nx)
j <- seq(1,Ny)
sj <- rep(NA,length(j))
si <- rep(NA,length(i))
aij.detrend <- Re(aij.small[(1:Nx),(1:Ny)])

sj <- (Re(aij.small[Nx,])-Re(aij.small[1,]))/(Nx-1)

sj <- Re(sj)

for(ii in 1:Nx){
        aij.detrend[ii,] <- aij.small[ii,] - (2*ii-Nx-1)*sj/2
}

si <- (Re(aij.detrend[,Ny])-Re(aij.detrend[,1]))/(Ny-1)
si <- Re(si)

for(jj in 1:Ny){
        aij.detrend[,jj] <- aij.detrend[,jj] - (2*jj-Ny-1)*si/2
}

##   Ermittelung der Fourier-Koeffizienten aus aij
u <- aij.detrend
cpq.back <- fft(aij.detrend)/((length(aij.detrend[100,]))*(length(aij.detrend[,100])))
l.back <- trunc(length(cpq.back[,1])/2)
l1.back <- trunc(length(cpq.back[1,])/2)
cpq.small <- cpq.back[(1:(l.back+1)),(1:(l1.back+1))]
cpq.small.u <- cpq.small

delta_k <- 2*pi/(delta_x*(Ny-1))
l_1 <- seq(0,trunc(Ny/2))
l <- seq(0,trunc(Nx/2))
k <- l_1*delta_k
Su <- rep(NA,length(k))

mat <- matrix(0,length(l),length(l_1))

for(il in l){
        p <- 2*pi*il/((Nx-1)*delta_x)
        q <- 2*pi*l_1/((Ny-1)*delta_x)
        p_sqr <- p^2
        q_sqr <- q^2
        mat[il,(1:length(q))] <- sqrt(p_sqr + q_sqr)
}

k1 <- k[2:length(k)]

for(ik in k1){
        Su[k==ik] <- 0
        k_1 <- ik - delta_k/2
        k_2 <- ik + delta_k/2
        root <- which(mat<k_2&mat>k_1,arr.ind=TRUE)
        for(j in 1:length(root[,1])){
                pl <- root[j,1]
                ql <- root[j,2]
                Su[k==ik] <- Su[k==ik] + cpq.small[pl,ql]*Conj(cpq.small[pl,ql])
        }
}
Su <- Re(Su)
Su[1] <- 0
Su <- Su*sqrt(4)
##------------------------------------------------
##         2) Spektrum fuer die v-Komponente
##------------------------------------------------

aij.small <- v

##           Detrending des Feldes aij
i <- seq(1,Nx)
j <- seq(1,Ny)
sj <- rep(NA,length(j))
si <- rep(NA,length(i))
aij.detrend <- Re(aij.small[(1:Nx),(1:Ny)])

sj <- (Re(aij.small[Nx,])-Re(aij.small[1,]))/(Nx-1)

sj <- Re(sj)

for(ii in 1:Nx){
        aij.detrend[ii,] <- aij.small[ii,] - (2*ii-Nx-1)*sj/2
}

si <- (Re(aij.detrend[,Ny])-Re(aij.detrend[,1]))/(Ny-1)
si <- Re(si)

for(jj in 1:Ny){
        aij.detrend[,jj] <- aij.detrend[,jj] - (2*jj-Ny-1)*si/2
}
##   Ermittelung der Fourier-Koeffizienten aus aij
v <- aij.detrend

cpq.back <- fft(aij.detrend)/((length(aij.detrend[100,]))*(length(aij.detrend[,100])))
l.back <- trunc(length(cpq.back[,1])/2)
l1.back <- trunc(length(cpq.back[1,])/2)
cpq.small <- cpq.back[(1:(l.back+1)),(1:(l1.back+1))]
cpq.small.v <- cpq.small

delta_k <- 2*pi/(delta_x*(Ny-1))
l_1 <- seq(0,trunc(Ny/2))
l <- seq(0,trunc(Nx/2))
k <- l_1*delta_k
Sv <- rep(NA,length(k))

mat <- matrix(0,length(l),length(l_1))

for(il in l){
        p <- 2*pi*il/((Nx-1)*delta_x)
        q <- 2*pi*l_1/((Ny-1)*delta_x)
        p_sqr <- p^2
        q_sqr <- q^2
        mat[il,(1:length(q))] <- sqrt(p_sqr + q_sqr)
}
k1 <- k[2:length(k)]

for(ik in k1){
        Sv[k==ik] <- 0
        k_1 <- ik - delta_k/2
        k_2 <- ik + delta_k/2
        root <- which(mat<k_2&mat>k_1,arr.ind=TRUE)
        for(j in 1:length(root[,1])){
                pl <- root[j,1]
                ql <- root[j,2]
                Sv[k==ik] <- Sv[k==ik] + cpq.small[pl,ql]*Conj(cpq.small[pl,ql])
        }
}

Sv <- Re(Sv)
Sv[1] <- 0
Sv <- Sv*sqrt(4)

##------------------------------------------------
## Finalize
##------------------------------------------------


Sekin <- (Su+Sv)/2

Sekin2D <- (cpq.small.v*Conj(cpq.small.v) + cpq.small.u*Conj(cpq.small.u))/2

Nx_cosmo<- Nx
k <- seq(trunc((Nx)/2),0,by=-1)
kappa<- (2*pi*k)/(delta_x*Nx)

k_cosmo <- seq(trunc((Nx_cosmo)/2),0,by=-1)
kappa_cosmo <- (2*pi*k_cosmo)/(delta_x*Nx_cosmo)

energy <- (k^(-5/3))*(10^8)
energy[length(energy)] <- 0

energy1 <- (k^(-3))*(10^8)
energy1[length(energy1)] <- 0

a <- seq(0,(length(Sekin)-1))
kappa_S <- a*delta_k


##------------------------------------------------
## Fit linear model
##------------------------------------------------

ln.kappa <- log(kappa_S[6:55]/sqrt(2*pi))
ln.Sekin <- log(Sekin[6:55])
fit <- lm(ln.Sekin ~ ln.kappa)
incpt <- fit$coefficients[1]
slope <- fit$coefficients[2]
incpt <- exp(incpt)
slop <- (kappa_S^(slope))*(incpt)
slop[1] <- 0

slope <- round(slope,digit=2)
sum <- summary( lm(ln.Sekin ~ ln.kappa))
quant <- sum$coefficients[4]
quant <- 2*quant

results=list(kappa_S=kappa_S,Sekin=Sekin)
#plot(kappa_S,Re(Sekin_mean)/sqrt(2*pi),log='xy',type='l',xlab=expression(paste(kappa,' [rad/m]')),ylab=expression(paste('E (',kappa,') ',' [',m^3/s^2,']')),ylim=c(1e-07*321*361,1e+03*321*361),col='blue',pch=21)
#text(2e-04,1e-01*321*361, adj=0.5, xpd=NA, font=1, labels=expression(paste(kappa^(-5/3))),cex=1.6)
#axis(3,at=axTicks(1),labels=trunc(2*pi/(1000*axTicks(1))),cex.lab=1,cex.axis=1.1)
#mtext (expression(paste( lambda , ' [km]')),side=3,line=2,outer=F,adj=.5,cex=1.5)
#polygon(c((kappa_S),(kappa_S[seq(181,1,by=-1)])),c(k1[1,]/sqrt(2*pi),k1[2,seq(181,1,by=-1)]/sqrt(2*pi)),col=plot.col[3])
#lines(kappa_S,Re(Sekin_mean)/sqrt(2*pi),type='l',pch=21)
#lines(kappa_S[6:55],slop[6:55]/3.3*432*444,lty="dotted",lwd=2)
#text(1e-04,0.10e-04*432*444,adj=0.5, xpd=NA, font=1,labels=bquote(paste(kappa^(.(slope ) %+-% .(quant) ))),cex=1.6)
#text(5.2e-04,6e+02*432*444,adj=0.5, xpd=NA, font=1,labels=bquote(paste(.('05.06.2011'))),cex=2)
#lines(kappa,energy/150000000*321*361,lty='dashed',lwd=2)
#abline(v=kappa_S[6],col='grey',lwd=2)
#abline(v=kappa_S[55],col='grey',lwd=2)

return(results)} # end function calc_spectrum

