# source("/user/data/gent/gvo000/gvo00090/vsc42383/tools/r_functions/matrix.poly.r")

# https://stackoverflow.com/questions/11736996/adding-stippling-to-image-contour-plot

# JKe 12-2017

#required function from www.menugget.blogspot.com
matrix.poly <- function(x, y, z=mat, n=NULL){
 if(missing(z)) stop("Must define matrix 'z'")
 if(missing(n)) stop("Must define at least 1 grid location 'n'")
 if(missing(x)) x <- seq(0,1,,dim(z)[1])
 if(missing(y)) y <- seq(0,1,,dim(z)[2])
 poly <- vector(mode="list", length(n))
 for(i in seq(n)){
  ROW <- ((n[i]-1) %% dim(z)[1]) +1
  COL <- ((n[i]-1) %/% dim(z)[1]) +1

  dist.left <- (x[ROW]-x[ROW-1])/2
  dist.right <- (x[ROW+1]-x[ROW])/2
  if(ROW==1) dist.left <- dist.right
  if(ROW==dim(z)[1]) dist.right <- dist.left

  dist.down <- (y[COL]-y[COL-1])/2
  dist.up <- (y[COL+1]-y[COL])/2
  if(COL==1) dist.down <- dist.up
  if(COL==dim(z)[2]) dist.up <- dist.down

  xs <- c(x[ROW]-dist.left, x[ROW]-dist.left, x[ROW]+dist.right, x[ROW]+dist.right)
  ys <- c(y[COL]-dist.down, y[COL]+dist.up, y[COL]+dist.up, y[COL]-dist.down)
  poly[[i]] <- data.frame(x=xs, y=ys)
 }
 return(poly)
}


# EXAMPLE
#  
## make vector of grids for hatching
# incl <- which(over==1)
#
## make polygons for each grid for hatching
# polys <- matrix.poly(1:12, 1:6, z=over, n=incl)
#
## plot 1 - hatching
# png("hatched_image.png")
# image(1:12, 1:6, data)
# for(i in seq(polys)){
#     polygon(polys[[i]], density=10, angle=45, border=NA)
#     polygon(polys[[i]], density=10, angle=-45, border=NA)
# }
# box()
# dev.off()
#
## plot 2 - points
# png("hatched_image2.png")
# image(1:12, 1:6, data)
# for(i in seq(polys)){
#     xran <- range(polys[[i]]$x)
#     yran <- range(polys[[i]]$y)
#     xs <- seq(xran[1], xran[2],,5)
#     ys <- seq(yran[1], yran[2],,5)
#     grd <- expand.grid(xs,ys)
#     points(grd, pch=19, cex=0.5)
# }
# box()
# dev.off()
