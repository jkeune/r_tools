## consecutive summer days (CSD [day] - maximum consecutive summer day length)
cwetday = function(x){
        #http://stackoverflow.com/questions/5012516/count-how-many-consecutive-values-are-true
         if (! all(is.na(x))){
          xx  = (x==1); rl = rle(xx); len= rl$lengths; v  = rl$values;
          if (any(v=="TRUE")){
          cmax = max(len[which(v==TRUE)])}else{cmax=0}
        }else{return(NA)}
return(cmax)}

