basinmean = function(mydata,basindata=mybasins,lon=lon,lat=lat){
    datf                           = data.frame(lon = c(lon), lat = c(lat), c(mydata))
    coordinates(datf)              <- ~ lon + lat
    proj4string(datf)              <- "+proj=longlat +datum=WGS84"
    bet                            = over(basindata,datf,fn=mean,na.rm=T)
return(bet)}

