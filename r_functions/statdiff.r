# Test for statistical significance
#  (a) non-paired t-test 
#      statistical difference between means 
  statdiff=function(x,y,value=FALSE,alpha=0.95,...){
    if(all(is.na(x))|all(is.na(y))){return(NA)}else{
    teststat=t.test(x,y,conf.int=alpha)
  return(((teststat$p.value)<(1-alpha))+0)}}
#  (b) paired t-test 
#      statistical difference for difference
  pstatdiff=function(x,y,value=FALSE,alpha=0.95,...){
    if(all(is.na(x))|all(is.na(y))){return(NA)}else{
    teststat=t.test(x,y,conf.int=alpha,paired=T)
  return(((teststat$p.value)<(1-alpha))+0)}}

# stat diff for 2 fields
  arstatdiff=function(ax,ay,value=FALSE,alpha=0.95,...){
    nx=dim(ax)[1]
    ny=dim(ay)[2]
    sdiff=array(NA,c(nx,ny))
        for (i in 1:nx){
        for (j in 1:ny){
        x=ax[i,j,]
        y=ay[i,j,]
            if(all(is.na(x))|all(is.na(y))){
                sdiff[i,j]=NA
            }else{
                teststat=t.test(x,y,conf.int=alpha,paired=T)
                sdiff[i,j]=(((teststat$p.value)<(1-alpha))+0)
            }
        }}
  return(sdiff)}

  warstatdiff=function(ax,ay,alpha=0.95,mymu=0,...){
    nx=dim(ax)[1]
    ny=dim(ay)[2]
    sdiff=array(NA,c(nx,ny))
        for (i in 1:nx){
        for (j in 1:ny){
        x=ax[i,j,]
        y=ay[i,j,]
            if(all(is.na(x))|all(is.na(y))){
                sdiff[i,j]=NA
            }else{
                # Wilcoxon signed rank test of the null that the distribution of x-y is symmetric around mu=0
                #teststat=wilcox.test(x,y,conf.int=alpha,paired=T,mu=mymu)
                # Wilcoxon rank sum test (equivalent to the Mann-Whitney test).  In this case, the null hypothesis is that the distributions of x and y differ by a location shift of mu and the alternative is that they differ by some other location shift (and the one-sided alternative greater is that x is shifted to the right of y.
                teststat=wilcox.test(x,y,conf.int=alpha,paired=F)
                sdiff[i,j]=(((teststat$p.value)<(1-alpha))+0)
            }
        }}
  return(sdiff)}

#wilcox.test
