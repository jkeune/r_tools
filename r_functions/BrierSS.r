 BrierSS <- function(y,y.for.bin,u=0)
 {
  n <- length(y)
  n.notfehl <- which( (y!=-9999) | (!is.na(y)) )
  y <- y[n.notfehl]
  y.for.bin <- y.for.bin[n.notfehl]

  y.bin <- y
  y.bin[y<=u] <- 0
  y.bin[y>u] <- 1

  p.clim <- length(which(y.bin == 1 ))/n
  bs.bin <- sum( (y.bin-y.for.bin)^2 )/n
  bs.clim <- sum( (y.bin-p.clim)^2 )/n
  bss <- 1-bs.bin/bs.clim
  return( list( bs=bs.bin, bs.clim=bs.clim, bss=bss ) )
 }
