  calc_entropy = function(x,ctdim,n){
      s  = x/n * log(x/n)
      si = x/n * log(x*ctdim*ctdim/n)
      s[which(s=="NaN")]=0
      si[which(si=="NaN")]=0
  return(list(s=s,si=si))}

