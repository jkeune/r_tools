rm(list=ls())

library(ncdf4)
library(abind)

ipath1="/work/jicg43/jicg4301/jkeune/tsmp/TerrSysMP_cordex11_full/heterogeneous/heatwave/r39_spinup4_fd/run/output"
ipath2="/work/jicg43/jicg4301/jkeune/tsmp/TerrSysMP_cordex11_full/heterogeneous/heatwave/r39_spinup5_fd/run/output"
ifilename="cosmo_out_QV_days1-12_hours7-23_daymean.nc"

ifile1=sprintf("%s/%s",ipath1,ifilename)
ifile2=sprintf("%s/%s",ipath2,ifilename)

ncfile1=nc_open(ifile1)
ptemp1=ncvar_get(ncfile1,"QV")
ncfile2=nc_open(ifile2)
ptemp2=ncvar_get(ncfile2,"QV")

mydim   = dim(ptemp1)
nlon    = mydim[1]
nlat    = mydim[2]
nlay    = mydim[3]
ntim    = mydim[4]

calc_decomp_lambda_lev = function(x,y){
# x,y: array of dim nlev x ntim
#
n = dim(x)[2]
xmean=rowMeans(x,na.rm=T)
ymean=rowMeans(y,na.rm=T)
xsig=apply(x,1,FUN=sd)
ysig=apply(y,1,FUN=sd)
corsign = diag(cor(t(x),t(y)))
if(all(is.na(xsig))){lambda=NA;alpha=NA;corr=NA}else{
  # lambda
        kappa=2*abs(rowSums((x-xmean)*(y-ymean)))
        corind=((corsign<0)+0)
        kappaind=corind*kappa
        top=rowSums((x-y)^2)/n
        bot=xsig^2+ysig^2+(xmean-ymean)^2+kappaind
        lambda=1-(top/bot)
  # correlation
        corr=corsign
  # alpha
        alpha = 2/( (xsig/ysig) + (ysig/xsig) + (xmean-ymean)^2 / (xsig*ysig) )
}
return(list(l=lambda,c=corr,a=alpha))}

cat("Calculate lambda...\n")
myalpha=array(NA,c(nlon,nlat,nlay))
mylambda=array(NA,c(nlon,nlat,nlay))
mycorr=array(NA,c(nlon,nlat,nlay))
for (i in 1:nlon){
for (j in 1:nlat){
        testres=calc_decomp_lambda_lev(ptemp1[i,j,,],ptemp2[i,j,,])
        myalpha[i,j,]=testres$a
        mylambda[i,j,]=testres$l
        mycorr[i,j,]=testres$c
}}

cat("Calculate lambda from alpha and correlation...\n")
myacorr=mycorr*myalpha
mydlambda=mylambda-myacorr


cat("Write netcdf-file...\n")
x = seq(1,nlon,1)                         # latitude
y = seq(1,nlat,1)                         # longitude
nt= seq(1,ntim,1)                         # number of time steps
nl= seq(1,nlay,1)                         # number of layer 

# dimensions
lsmlat        = ncdim_def( "lsmlat","degree", as.double(y),unlim=FALSE)
lsmlon        = ncdim_def( "lsmlon","degree", as.double(x),unlim=FALSE)
nlev          = ncdim_def( "nlev","", as.double(nl),unlim=FALSE)
time          = ncdim_def( "time","days",nt)

lambda= ncvar_def("lambda","unitless",dim=list(lsmlon,lsmlat,nlev),missval=1.e30,longname="Duveiller lambda")
alpha= ncvar_def("alpha","unitless",dim=list(lsmlon,lsmlat,nlev),missval=1.e30,longname="Duveiller alpha")
corr= ncvar_def("corr","unitless",dim=list(lsmlon,lsmlat,nlev),missval=1.e30,longname="correlation")
acorr= ncvar_def("acorr","unitless",dim=list(lsmlon,lsmlat,nlev),missval=1.e30,longname="alpha*correlation")
dlambda= ncvar_def("dlambda","unitless",dim=list(lsmlon,lsmlat,nlev),missval=1.e30,longname="difference between lambda")
opath="/work/jicg43/jicg4301/jkeune/tsmp/TerrSysMP_cordex11_full/heterogeneous/heatwave/pdiffs/lambda_decomp"
ofile="lambda_decompm_4_fd5_fdQV_daymean.nc"
outfile=sprintf("%s/%s",opath,ofile)
sprintf("Writing: %s " ,outfile)
lambda.ex = nc_create( outfile , list(lambda,alpha,corr,acorr,dlambda))
ncvar_put(lambda.ex, lambda, mylambda)
ncvar_put(lambda.ex, alpha, myalpha)
ncvar_put(lambda.ex, corr, mycorr)
ncvar_put(lambda.ex, acorr, myacorr)
ncvar_put(lambda.ex, dlambda, mydlambda)

nc_close(lambda.ex)
print(sprintf("Successfully created: %s ",outfile))

