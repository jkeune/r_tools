# source("~/R/func/orss.r")
# calculates odds ratio skill score after Stephenson (2000)
# for binary events
# corrects pdf and calculates significance 
# x: predictor 
# y: predictant
#

 orss <- function(x,y,threshold=NULL)
 {
  if ( is.null(threshold) ) threshold <- seq(0.1,0.9,by=0.05)
  if ( is.null(length(threshold)) ) nth <- 1
  else                              nth <- length(threshold) 

  oddsrss <- array(NA,c(nth))
  oddssig <- array(NA,c(nth))
  lodds   <- array(NA,c(nth))
 
  q <- 1.960 
 
  for ( n in 1:nth )
  {

# contigency table true: >= threshold
#                 false: <  threshold

   a = length( which( x>=threshold[n] & y>=threshold[n] ) ) 
   d = length( which( x< threshold[n] & y< threshold[n] ) ) 
 
   b = length( which( x>=threshold[n] & y< threshold[n] ) ) 
   c = length( which( x< threshold[n] & y>=threshold[n] ) ) 

# hit rate H and false alarm rate F

   H = a/(a+c) 
   F = b/(b+d) 
   odds = H/(1-H) / ( F/(1-F) )

# log odds ratio

   if ( a<=5 | b<=5 | c<=5 | d<=5 ) fehler=1 ;end
 
   odds_std = sqrt(1/a+1/b+1/c+1/d) 
   logodds = log(odds) 
   sign = odds_std*q
 
# odd ratio skill score

   oddsrss[n] <- (odds-1)/(odds+1) 
   lodds[n]   <- logodds
   oddssig[n] <- sign
  }  # nth

  return( list( lodds=lodds, oddsrss=oddsrss ) )
 }
