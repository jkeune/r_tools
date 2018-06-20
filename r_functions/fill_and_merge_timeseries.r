fill_and_merge_timeseries <- function( var, varid, start_date, end_date, time.resolution = as.difftime(30, units="mins") , ind = 1){
Sys.setenv(TZ = "UTC")
  if (ind==1){
    var	= fill_timeseries( zoo(eval(parse(text=paste("tab$", varid, sep = ""))),mydates), 	start_date = startdate, end_date = enddate, time.resolution = as.difftime(30, units="mins") )
  }else{
    varn = fill_timeseries( zoo(eval(parse(text=paste("tab$", varid, sep = ""))),mydates), 	start_date = startdate, end_date = enddate, time.resolution = as.difftime(30, units="mins") )
    var = c(var,varn)
    var = fill_timeseries( var, start_date=start(var), end_date=end(var),time.resolution=as.difftime(30, units="mins"))
  }
return( var )}
