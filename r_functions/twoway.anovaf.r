
twoway.anovaf=function(x,ret="f",alphalev=0.95){
# x: array of dim = I x J, i.e. I time steps, J ensemble realizations
# ret: what should be returned? 
# ret="fa" : F according to von Storch & Zwiers (1999, p. 182, Eq. 9.17) with treatment effect
# ret="fb" : F according to von Storch & Zwiers (1999, p. 182, Eq. 9.17) with block effect
# ret="rra" : coefficient of determination from SSA
# ret="rrb" : coefficient of determination from SSB
if(all(is.na(x))){return(NA)}else{
  I=dim(x)[1]	# I different treatments
  J=dim(x)[2]	# J blocks
  yoj=apply(x[,],c(2),FUN=mean,na.rm=T)	# == colMeans(x,na.rm=T)
  yio=apply(x[,],c(1),FUN=mean,na.rm=T)	# == rowMeans(x,na.rm=T)
  yoo=mean(x[,],na.rm=T)
  ssa=J*sum(((yio-rep(yoo,I))^2))
  ssb=I*sum(((yoj-rep(yoo,J))^2))
  sse=sum((c(x[,])-rep(yio,J)-rep(yoj,each=I)+rep(yoo,I*J))^2)
  sst=ssa+sse+ssb
  # treatment effect; treatment mean - overall mean => ssa
    fa=ssa*(J-1)/(sse)
    facomp=qf(alphalev,df1=(I-1),df2=((I-1)*(J-1)),lower.tail=TRUE)	
    if((fa>=facomp)&(!is.na(fa))){binsiga=1}else{binsiga=0 } # is the difference significant?
  # block effect; variance between blocks
    fb=ssb*((I-1))/(sse)
    fbcomp=qf(alphalev,df1=(J-1),df2=((I-1)*(J-1)),lower.tail=TRUE)	
    if((fb>=fbcomp)&(!is.na(fb))){binsigb=1}else{binsigb=0} # is the difference significant?
if(ret=="fa"){
  if(binsiga==0){return(NA)}else{return(fa)}	# return only signifcant R
}
if(ret=="fb"){
  if(binsigb==0){return(NA)}else{return(fb)}	# return only signifcant R
}
if(ret=="rra"){
  if(binsiga==0){return(NA)}else{return((ssa-sse/(J-1))/(ssa+sse+ssb))}#r=(ssa-((J-1)/(J*(n-1))*sse))/(ssa+sse)
}
if(ret=="rrb"){
  if(binsigb==0){return(NA)}else{return((ssb-sse/(I-1))/(ssa+ssb+sse))}
}
}}
