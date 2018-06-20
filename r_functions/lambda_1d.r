calc_lambda_1d = function(x,y){
# Calculation of lambda according to 
# Duveiller et al. (2016): 
# "Revisiting the concept of a symmetric index of agreement for continuous datasets"
# Equations (15) and (16) 
#
# Dimensions: 
# x,y: vectors of length ntim
#
# Returns a list of l=lambda, c=correlation and a=alpha
# 
# JKe 04.10.2016
n = length(x)
xmean=mean(x,na.rm=T)
ymean=mean(y,na.rm=T)
xsig=sd(x,na.rm=T)
ysig=sd(y,na.rm=T)
corsign = cor(x,y)
  # lambda
        kappa=2*abs(sum((x-xmean)*(y-ymean)))
        corind=((corsign<0)+0)
        kappaind=corind*kappa
        top=sum((x-y)^2)/n
        bot=xsig^2+ysig^2+(xmean-ymean)^2+kappaind
        lambda=1-(top/bot)
  # correlation
        corr=corsign
  # alpha
        alpha = 2/( (xsig/ysig) + (ysig/xsig) + (xmean-ymean)^2 / (xsig*ysig) )
return(list(l=lambda,c=corr,a=alpha))}

