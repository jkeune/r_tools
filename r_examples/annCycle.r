# calculate annual cycle from time series                
# fitte Jahres- und Halbjahreswelle
#
 library(nlme)
 annCycle <- function(y,cyclength=NULL,time=NULL)
 {
  y.ann <- array(NA,c(12))

  if ( time == NUL ) % Jahres- und Halbjahreswelle
  {
   ntmax = length(y) 
   H <- array(NA, c(ntmax,5))
   x <- 1:ntmax 

   H[1:ntmax,1] = sin(2*pi*x/cyclength) 
   H[1:ntmax,2] = cos(2*pi*x/cyclength) 
   H[1:ntmax,3] = sin(2*pi*x/cyclength*2) 
   H[1:ntmax,4] = cos(2*pi*x/cyclength*2) 
   H[1:ntmax,5] = 1 

   c = solve( t(H) %*% H ) %*% t(H) %*% y 
   y.ann = H %*% c
  }

  if ( cyclength == NULL ) % mittlerer Jahresgang (Monatsmittel)
  {
   for ( imon in 1:12 )
   {
    y.ann[imon] <- mean(y[which(date.mdy(time)$month==imon)])
   }
  }
  return (y.ann = y.ann) 
 }
