# source("/home/jkeune/SVA/ParFlow/r-programs/R_kinsol_statistics.r")
##-------------------------------------------
## ParFlow-Statistics
#	This script reads ParFlow statistics from the kinsol script 
#	and plots them
#	Modify file path and names
#	To do: adapt axis (dates) and outline extremes?
#	(2013-10-10)
##-------------------------------------------

rm(list=ls())

# File paths and names
#data_dir	= "/home/jkeune/coup_oas_test/run"
data_dir	= "/home/jkeune/clmpfl_test2/pflout"
filename	= "cordex0.11.out.kinsol2.log"
file = sprintf("%s/%s",data_dir,filename)

# Startdate? (not implemented yet)
startdate = "2002-01-01 00:00"

##--------------------------------------------
## Read kinsol
cat("-------------------------------------- \n")
cat("Reading kinsol statistics: ",file," \n")

#Function to get strings from kinsol script
  get_kinsol = function(mystring,file=file){
    grepout = (unlist(try(system(paste("grep '",noquote(mystring),"' ",file,sep=""),intern=TRUE)),"\\ "))
    a = sub("[[:space:]]", "", grepout)
    b = unlist(strsplit(a,"[[:space:]]"))
    if (mystring=="Lin. Conv. Fails:"){
      b = b[b!=""&b!=mystring&b!=sub("[[:space:]]","", mystring)&b!="Lin.Conv."&b!="Fails:"]
    }else{
      b = b[b!=""&b!=mystring&b!=sub("[[:space:]]","", mystring)]
    }
    seq1 = seq(1,length(b),2)
    seq2 = seq(2,length(b),2)
    ret1 = b[seq1]
    ret2 = b[seq2]
  return(list(it=noquote(ret1),acc=noquote(ret2)))}

cat("Reading number of linear iterations... \n")
linit 	= get_kinsol(mystring="Lin. Its.:",file=file)
cat("Reading number of nonlinear iterations... \n")
nlinit 	= get_kinsol(mystring="Nonlin. Its.:",file=file)
cat("Reading number of function evaluation failures... \n")
nfe 	= get_kinsol(mystring="Func. Evals.:",file=file)
cat("Reading number of linear convergence failures... \n")
lcf 	= get_kinsol(mystring="Lin. Conv. Fails:",file=file)

##--------------------------------------------
## Plots
cat("Plotting... \n")
# time seq for axis
tsteps	= length(linit$it)
dateseq = seq(as.POSIXct("2002-01-01 00:00","%Y-%m-%d %H:%M"),by="hours",length.out=tsteps)
enddate = tail(dateseq,1)

X11(width=12,height=8)
par(mfrow=c(2,2))
plot(linit$it,t="l",xlab="time steps [h]",ylab="number of linear iterations",col="red",lwd=2)
  legend("topleft",c("linear iterations (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(nlinit$it,t="l",xlab="time steps [h]",ylab="number of nonlinear iterations",col="RoyalBlue",lwd=2)
  legend("topleft",c("nonlinear iterations (acc.)"),bty="n")
  #axis.Date(1, at = seq(as.POSIXct(startdate),enddate, by="day"), format= "%m-%d")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(nfe$it,t="l",xlab="time steps [h]",ylab="function evaluation failures",col="Green",lwd=2)
  legend("topleft",c("function evaluation failures (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(lcf$it,t="l",xlab="time steps [h]",ylab="linear convergence failures",col="DarkOrange",lwd=2)
  legend("topleft",c("linear convergence failures (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)

X11()
par(mfrow=c(2,2))
plot(linit$acc,t="l",xlab="time steps [h]",ylab="number of linear iterations (acc.)",col="red",lwd=2,lty=2)
  lines(linit$it,col="red",lwd=2)
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
  legend("topleft",c("linear iterations (acc.)"),bty="n")
plot(nlinit$acc,t="l",xlab="time steps [h]",ylab="number of nonlinear iterations (acc.)",col="RoyalBlue",lwd=2,lty=2)
  lines(nlinit$it,col="RoyalBlue",lwd=2)
  legend("topleft",c("nonlinear iterations (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(nfe$acc,t="l",xlab="time steps [h]",ylab="function evaluation failures (acc.)",col="Green",lwd=2,lty=2)
  lines(nfe$it,col="Green",lwd=2)
  legend("topleft",c("function evaluation failures (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(lcf$acc,t="l",xlab="time steps [h]",ylab="linear convergence failures (acc.)",col="DarkOrange",lwd=2,lty=2)
  lines(lcf$it,col="DarkOrange",lwd=2)
  legend("topleft",c("linear convergence failures (acc.)"),bty="n")
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)

message("Press Return To Continue")
invisible(readLines("stdin", n=1))


