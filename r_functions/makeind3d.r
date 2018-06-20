makeind3d = function(x,crit,op="lt"){
      # x: array 
      # crit: critical value
      # op: "lt" (less than), "gt" (greater than"), "le" (less or equal), "ge" (greater or equal) than the critical value
      mydim=dim(x)
      if(op=="lt"){myind=((x<crit)+0)}
      if(op=="gt"){myind=((x>crit)+0)}
      if(op=="le"){myind=((x<=crit)+0)}
      if(op=="ge"){myind=((x>=crit)+0)}
      myind[which(myind==0)]=NA
    return(array(myind,mydim))}
