anovaf=function(x,ret="f",alphalev=0.95){
# x: array of dim = n x J, i.e. n time steps, J ensemble realizations
# ret: what should be returned? 
# ret="f" : F according to von Storch & Zwiers (1999, p. 176, Eq. 9.6)
# ret="r" : Ra^2 according to von Storch & Zwiers (1999, p. 177)
# ret="ss": SSA, SSE and SST
# ret="rsig": only significant R^2 (r), non-significant values are set to NA
# ret="sign": significance (1/0) 
# ret="all": all (R^2,F,sig,R^2,sign.matrix)
# alphalev=0.95: significance level for hypothesis test using F
if(all(is.na(x))){return(NA)}else{
  n=dim(x)[1]
  J=dim(x)[2]
  yoj=apply(x[,],c(2),FUN=mean,na.rm=T)
  yoo=mean(x[,],na.rm=T)
  ssa=n*sum((yoj-rep(yoo,J))^2)
  sse=sum((c(x[,])-rep(yoj,each=n))^2)
  sst=ssa+sse
  f=ssa*(J*(n-1))/((J-1)*sse)
  r=(ssa-((J-1)/(J*(n-1))*sse))/(ssa+sse)
  fcomp=qf(alphalev,df1=(J-1),df2=(J*(n-1)),lower.tail=TRUE)    # F(J-1,J*(n-1))
  if((f>=fcomp)&(!is.na(f))){binsig=1}else{binsig=0 } # is the difference significant?
  if(binsig==0){rsig=NA}else{rsig=r}    # return only signifcant R
if(ret=="f"){f=ssa*(J*(n-1))/((J-1)*sse);return(f)}
if(ret=="r"){r=(ssa-sse*((J-1)/(J*(n-1))))/(sst);return(r)}
if(ret=="ssa"){return(ssa)}
if(ret=="sse"){return(sse)}
if(ret=="sst"){return(sst)}
if(ret=="sign"){return(binsig)}
if(ret=="rsig"){return(rsig)}
if(ret=="all"){return(list(r,f,rsig,binsig))}
}}

