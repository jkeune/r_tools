mask3dfield  = function(ar,landmask=mask){
        arn=ar
        for (i in (1:(dim(ar)[3]))){
                ari     = ar[,,i]
                ari[which(landmask==0)]=NA
                arn[,,i]= ari
        }
return(arn)}

