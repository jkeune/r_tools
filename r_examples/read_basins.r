rm(list=ls())
library(maptools)
library(ncdf)
library(rgeos)
library(sp)
library(fields)
library(rgdal)


# CORDEX grid
  mygrid        = open.ncdf("/perm/ms/spde/de5j/inputdata/grids/focus_mask_eur11_315x315.nc")
  clon  = get.var.ncdf(mygrid,"lon")
  clat  = get.var.ncdf(mygrid,"lat")
  lmask = get.var.ncdf(mygrid,"LANDMASK")
  dat   = data.frame(lon = c(clon), lat = c(clat), lmask=c(lmask))
  coordinates(dat) <- ~ lon + lat
  #proj4string(dat) <- CRS("+proj=longlat")
  proj4string(rivermaps) <- "+proj=longlat +datum=WGS84"

# Watersheds
  rivermaps = readShapeSpatial("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/r-functions/eu_bas_30s_beta.shp")
  proj4string(rivermaps) <- "+proj=longlat +datum=WGS84"
  plot(basinmaps)

  # only look at basins with km2>10000km2
  basinmaps = rivermaps[rivermaps$AREA_SQKM>100000,]
        # 1: Weichsel, 2: Oder, 3: Elbe, 4: Rhein, 5: Loire, 10: Donau
  basinmaps = rivermaps[rivermaps$AREA_SQKM>50000,]
        # 4: Nemunas, 5: Weichsel, 6: Oder, 7: Elbe, 8: Rhein[[2]], 9: Seine, 11: Loire, 16: Bug, 17: Dnjerpo[[2]], 18: Dnjestr, 19: Wolga, 22: Donau, 23: Garonne, 24: po, 25: Rhone. 28: Douro, 30: Mariza, 31: Ebro, 34: Tejo, 37: Guadiana[[2]], 38: Guadalouvir
  sbasinmaps=basinmaps[c(4,5,6,7,8,9,11,16,17,18,22,23,24,25,28,30,31,34,37,38),]
                #  indi=(which(rivermaps@data$AREA_SQKM>100000))
                #  # only look at X largest basins
                #  #inds=sort(rivermaps@data$AREA_SQKM,decreasing=T,index.return=T)
                #  #indi=inds$ix[1:200] 
                #  basins = vector("list", length(indi))
                #  for (i in 1:length(indi)){
                #      for (p in 1:length(rivermaps@polygons[[indi[i]]]@Polygons)){
                #      basins[[i]]      = rbind(basins[[i]],rivermaps@polygons[[indi[i]]]@Polygons[[p]]@coords)
                #  }}
                #  #basins = vector("list", length(indi))
                #  #for (i in 1:length(indi)){
                #  #      basins[[i]]   = (rivermaps@polygons[[indi[i]]]@Polygons[[1]]@coords)
                #  #}
                #  #colnames(basins[[i]]) = c("lon","lat")  # [,1] = lon, [,2] = lat


 # Test image
 poly.image(clon,clat,lmask,col=c("white","grey40"))
 plot(basinmaps,add=T)


 lines(basinmaps@polygons[[i]]@Polygons[[1]]@coords[,1],basinmaps@polygons[[i]]@Polygons[[1]]@coords[,2])

  #for (i in 1:length(indi)){
  #polygon(basins[[i]][,1],basins[[i]][,2])
  #}

 testdat= data.frame(lon = c(clon[150:250,150:250]), lat = c(clat[150:250,150:250]), lmask=c(lmask[150:250,150:250]))
  coordinates(testdat) <- ~ lon + lat
  proj4string(testdat) <- "+proj=longlat +datum=WGS84"

  testmean=over(basinmaps,testdat,fn=mean)

# check whether gridpoint falls into BASIN
# with >100000km2 i=10 is the Danube
#dat = spTransform(dat, proj4string(rivermaps))
#proj4string(rivermaps) <- proj4string(dat) 
#sp::over(dat,rivermaps@polygons[[indi[i]]])

