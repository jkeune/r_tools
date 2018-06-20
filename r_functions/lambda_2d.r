calc_decomp_lambda_lev = function(x,y){
# Calculation of lambda according to 
# Duveiller et al. (2016): 
# "Revisiting the concept of a symmetric index of agreement for continuous datasets"
# Equations (15) and (16) 
#
# Dimensions: 
# x,y: array of dimension nlev x ntim
#
# Returns a list of l=lambda, c=correlation and a=alpha
# 
# JKe 04.10.2016
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

