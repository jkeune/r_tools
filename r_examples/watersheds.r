# JKe - 2017
# Script to read watershed boundaries from HydroSHEDS
# and match it with the CORDEX EUR11 grid
# 

rm(list=ls())
library(fields)
library(ncdf)
library(RColorBrewer)
library(zoo)
library(abind)
Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")

# Country borders
library(maptools)
worldmaps = readShapeSpatial("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/r-functions/TM_WORLD_BORDERS-0.2.shp")

# Settings
setwd("/perm/ms/spde/de5j/tools/analysis-tools/r-programs")
plotpath="/perm/ms/spde/de5j/tools/analysis-tools/r-programs"
source("r_colors.r")
ipath="/scratch/ms/spde/de5j/terrsysmp_run/"

#grid 
gridfile=open.ncdf("/perm/ms/spde/de5j/inputdata/grids/focus_mask_eur11_315x315.nc")
lon=get.var.ncdf(gridfile,"lon")
lat=get.var.ncdf(gridfile,"lat")
#mask
maskfile= open.ncdf("/perm/ms/spde/de5j/inputdata/grids/focus_mask_eur11_315x315.nc")
mask    = get.var.ncdf(maskfile,"LANDMASK")
mask2dfield  = function(ar,landmask=mask){ar[which(landmask==0)]=NA;return(ar)}

readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}
##*********************************
## WATERSHED DATA
##*********************************

        # Watersheds
          rivermaps = readShapeSpatial("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/r-functions/eu_bas_30s_beta.shp")
          proj4string(rivermaps) <- "+proj=longlat +datum=WGS84"
          #plot(rivermaps)

        ##***********************
        ## 1. LARGE BASINS >50000
        ##***********************
        lbasins = rivermaps[rivermaps$AREA_SQKM>50000,]
        # 4: Nemunas, 5: Weichsel, 6: Oder, 7: Elbe, 8: Rhein[[2]]
        # 9: Seine, 11: Loire, 16: Bug, 17: Dnjerpo[[2]], 
        # 18: Dnjestr, (19: Wolga), 22: Donau, 23: Garonne, 
        # 24: po, 25: Rhone. 28: Douro, 30: Mariza, 31: Ebro, 
        # 34: Tejo, 37: Guadiana[[2]], 38: Guadalouvir
          myorder=c(38,37,34,28,31,23,11,9,25,24,8,7,6,5,4,22,18,17,16,30)
          lbasinnames=c("Guadalquivir","Guadiana","Tagus","Douro","Ebro","Garonne","Loire","Seine","Rhone","Po","Rhine","Elbe","Oder","Vistula","Neman","Danube","Dniester","Dnieper","Bug","Maritsa")
          lbasins=lbasins[myorder,]
          nlbasins=length(lbasinnames)

        ##***********************
        ## 2. MEDIUM BASINS <50000 but >25000
        ##***********************
        mbasins = rivermaps[(rivermaps$AREA_SQKM<50000)&(rivermaps$AREA_SQKM>25000),]
        # 1: Weser 
        # 2: Maas
          mbasinnames=c("Weser","Maas")
          mbasins=mbasins[c(1,2),]

        # write all basins into one rds file
        mybasins=rbind(lbasins,mbasins)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatersheds_eur11_gt250000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasins, savefile)
        mybasinnames=c(lbasinnames,mbasinnames)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatershednames_eur11_gt250000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasinnames, savefile)


        ##***********************
        ## 3. SMALLER BASINS <25000 (but >10000)
        ##***********************
        sbasins = rivermaps[(rivermaps$AREA_SQKM<25000)&(rivermaps$AREA_SQKM>10000),]
        # 6: Trent (UK)?, 10: Severn (UK), 11: Themse (UK)
        # 7: Ems, 23: Eastern Alps, 24: Dordogne, 28: Adour, 34: Tiber  
        # 9: Rhine?
        # 12: Scheidt?, 
        # 16: NW-France 
        # 1,2,3,4,5,8,19,32,33,37,38,39,40,43,44,46 (no name, but in domain)
        # 
          myorder=c(1,2,3,4,5,6,7,8,9,10,11,12,16,19,23,24,28,32,33,34,37,38,39,40,43,44,46)
          sbasinnames=c(NA,NA,NA,NA,NA,"Trent","Ems",NA,NA,"Severn","Themse","Scheidt",NA,NA,"Eastern Alps","Dorodogne","Adour",NA,NA,"Tiber",NA,NA,NA,NA,NA,NA,NA)
          sbasins=sbasins[myorder,]

        # write all basins into one rds file
        mybasins=rbind(lbasins,mbasins,sbasins)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatersheds_eur11_gt10000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasins, savefile)
        mybasinnames=c(lbasinnames,mbasinnames,sbasinnames)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatershednames_eur11_gt10000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasinnames, savefile)

        ##***********************
        ## 4. EVEN SMALLER BASINS <10000 (but >5000)
        ##***********************
        esbasins = rivermaps[(rivermaps$AREA_SQKM<10000)&(rivermaps$AREA_SQKM>5000),]
        # 
        myorder=c(1,2,3,4,5,6,7,8,9,10,16,21,23,26,27,29,30,31,34,35,36,37,38,39,40,42,43,45,46,47,48)
        esbasins=esbasins[myorder,]
        esbasinnames=rep(NA,length(myorder))


        # write all basins into one rds file
        mybasins=rbind(lbasins,mbasins,sbasins,esbasins)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatersheds_eur11_gt5000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasins, savefile)
        mybasinnames=c(lbasinnames,mbasinnames,sbasinnames,esbasinnames)
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatershednames_eur11_gt5000km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasinnames, savefile)


        ##***********************
        ## 5. EVEN EVEN SMALLER BASINS <5000 (but >500, corresponds to 3-4 gridcells at least)
        ##***********************
        eesbasins = rivermaps[(rivermaps$AREA_SQKM>100),]
        # 
        eesbasinnames=rep(NA,dim(eesbasins)[1])


        # write all basins into one rds file
        #mybasins=rbind(lbasins,mbasins,sbasins,esbasins,eesbasins)
        mybasins=eesbasins
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatersheds_eur11_gt100km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasins, savefile)
        #mybasinnames=c(lbasinnames,mbasinnames,sbasinnames,esbasinnames,eesbasinnames)
        mybasinnames=eesbasinnames
        savefile=paste("/perm/ms/spde/de5j/tools/analysis-tools/r-programs/ana_wateruse/mywatershednames_eur11_gt100km2.rds",sep="")
        cat(paste(" --> SAVING FILE:",savefile,"... "))
        saveRDS(mybasinnames, savefile)


# Image
#       poly.image(lon,lat,mask,col=c("white","grey95"))
#       plot(lbasins,add=T)
#       plot(mbasins,add=T,col="blue")
#       plot(sbasins,add=T,col="red")
#       plot(esbasins,add=T,col="green")
#
#       for(i in 1:(dim(esbasins)[1])){
#       print(sprintf("%s...",i))
#       plot(sbasins[i,],add=T,col="blue")
#       readkey()
#       }

# application example
#        datf                           = data.frame(lon = c(lon), lat = c(lat), mask=c(mask))  
#        coordinates(datf)              <- ~ lon + lat
#        proj4string(datf)              <- "+proj=longlat +datum=WGS84"
#        ngridpoints                    = over(lbasins,datf,fn=sum,na.rm=T)


