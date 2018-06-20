aggreg_x = function(x,ne){xm = tapply(x, rep(1:(length(x)/ne), each = ne), sum,na.rm=FALSE);return(xm)}
