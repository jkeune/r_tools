fill_timeseries <- function( timeseries, start_date, end_date, time.resolution = as.difftime(30, units="mins") ){
	coredata(timeseries)[ coredata(timeseries) == -9999.0] = NA		
	time 	= seq(start_date, end_date, by=time.resolution)
	z_full 	= zoo( x=rep(NA, length(time)), order.by=time )
	z_merged= merge( timeseries, z_full )
return( z_merged$timeseries )}
