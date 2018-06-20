calculate_averages <- function(timeseries , type){
    # daily mean
	if(type=="daily"){
	  days = format(time(timeseries), "%Y-%m-%d")
	  daily_mean = aggregate( timeseries, days, mean, na.rm=TRUE )	
	return(daily_mean)}
    # average diurnal cycle
	if(type=="diurnal"){
	  hours = format(time(timeseries), "%H:%M")
	  dirunal_mean = aggregate( timeseries, hours, mean, na.rm=TRUE )	
	return(dirunal_mean)}
}
