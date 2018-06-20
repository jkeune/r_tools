sigcors = function(x,y,alpha=0.95){
    # this function checks for significant correlations between x and y 
    # x,y: 3-d arrays of dim(nlon,nlat,time)
    # result: array of dim(nlon,nlat) with significant correlations only
    # according to http://janda.org/c10/Lectures/topic06/L24-significanceR.htm
      res       = array(NA,c(dim(x)[1],dim(x)[2]))
      for (i in 1:(dim(x))[1]){
        for (j in 1:(dim(x))[2]){
	  if(all(is.na(x[i,j,]))|all(is.na(y[i,j,]))){res[i,j]=NA}else{
	    mycor = cor.test(x[i,j,],y[i,j,],use="pairwise.complete.obs")
	    if(!is.na(mycor$estimate)&(mycor$p.value<0.05)){res[i,j]= mycor$estimate}else{res[i,j]=NA}
	  }
      }}
return(res)}
