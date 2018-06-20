convdims=function(x){
xn=array(NA,dim(x))
if(length(dim(x))==3){
xn[1:180,,] = x[181:360,,]
xn[181:360,,] = x[1:180,,]}
if(length(dim(x))==2){
xn[1:180,] = x[181:360,]
xn[181:360,] = x[1:180,]}
return(xn)}

