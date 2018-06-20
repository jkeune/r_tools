source("/home/jkeune/SVA/tools/analysis-tools/r-functions/get_nearest.r")
get_n_nearest_gp = function(obs_lat,obs_lon,model_lon,model_lat,mod_nlon,mod_nlat,n){
  # n: number of gridpoints around station
  # obs_lat, obs_lon: lon,lat from observation stations
  # model_lon, model_lat: lat and lon from model (cosmo)
  # nlon_mod, nlat_mod : number of lon/ lat
  lat.min = array(NA,c(length(obs_lat),n))
  lon.min = array(NA,c(length(obs_lon),n))
  for (stat in 1:length(obs_lon)){
    adis = get_nearest(b1=obs_lat[stat],b2=model_lat,l1=obs_lon[stat],l2=model_lon)
    nearest_ngp = rep(NA,n)
    for (i in 1:n){
      nearest_ngp[i] = which(adis==sort(adis)[i])}
    lat.min[stat,] = trunc(nearest_ngp/mod_nlon)
    lon.min[stat,] = nearest_ngp-lat.min[stat,]*mod_nlon
    lat.min[stat,] = lat.min[stat,] + 1
  }
return(list(lat=lat.min,lon=lon.min))}

