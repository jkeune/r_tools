 CHI2test <- function(y,y.taus,tau)
 {
  n <- length(y)
  ntau <- length(tau)

  null.i  <- array(NA,c(length(tau)))
  fnull.i  <- array(NA,c(length(tau)))
  n.i  <- array(NA,c(length(tau)))
  p.i  <- array(NA,c(length(tau)))
  fn.i  <- array(NA,c(length(tau)))
  fp.i  <- array(NA,c(length(tau)))
  chi  <- array(NA,c(length(tau)))

  for ( itau in 1:(length(tau)) )
  {
   n.null <- which( y.taus[,itau] != 0 )
   n.tau     <- which( y[n.null] < y.taus[n.null,itau] )
   p.i[itau] <- tau[itau]
   n.i[itau] <- length(n.tau) / length(n.null)
   null.i[itau] <- length(n.null)

   fn.null <- which( y.taus[,itau] > 0 )
#   fn.null <- which( ( y.taus[,itau] == 0 & y == 0 )==F )
   fn.tau     <- which( y[fn.null] <= y.taus[fn.null,itau] )
   fp.i[itau] <- tau[itau]
   fn.i[itau] <- length(fn.tau) / length(fn.null)
   fnull.i[itau] <- length(fn.null)
  }
  chi    <- (fn.i - fp.i)^2/fp.i*fnull.i
  chisum <- sum(chi[1:ntau])
  chi95  <- qchisq(0.95, (length(tau)), ncp=0)

  return( list( fp.i=fp.i, fn.i=fn.i, chisum=chisum, p.i=p.i, n.i=n.i, chi95=chi95 ) )
 }
