##-------------------------------------------
## ParFlow-Statistics
#       This script reads ParFlow statistics from the kinsol script 
#       and plots them
#       Modify file path and names
#       To do: adapt axis (dates) and outline extremes?
#       (2013-10-10)
##-------------------------------------------

rm(list=ls())

# File paths and names
data_dir1               = "/scratch/ms/spde/de5j/terrsysmp_run/cordex_1.2.0MCT_clm-cos-pfl_MODIS2000_HFD2_3D_sn_v04l_ref"
data_dir2               = "/scratch/ms/spde/de5j/terrsysmp_run/cordex_1.2.0MCT_clm-cos-pfl_MODIS2000_HFD2_3D_sn_v04l_wateruseW_day"
filename                = "cordex0.11.out.kinsol.log"
file1                   = sprintf("%s/%s",data_dir1,filename)
file2                   = sprintf("%s/%s",data_dir2,filename)

cat("-------------------------------------- \n")

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

## File 1
cat("Reading kinsol statistics: ",file1," \n")
cat("Reading number of linear iterations... \n")
linit1   = get_kinsol(mystring="Lin. Its.:",file=file1)
cat("Reading number of nonlinear iterations... \n")
nlinit1  = get_kinsol(mystring="Nonlin. Its.:",file=file1)
cat("Reading number of function evaluation failures... \n")
nfe1     = get_kinsol(mystring="Func. Evals.:",file=file1)
cat("Reading number of linear convergence failures... \n")
lcf1     = get_kinsol(mystring="Lin. Conv. Fails:",file=file1)

## File 2
cat("Reading kinsol statistics: ",file2," \n")
cat("Reading number of linear iterations... \n")
linit2   = get_kinsol(mystring="Lin. Its.:",file=file2)
cat("Reading number of nonlinear iterations... \n")
nlinit2  = get_kinsol(mystring="Nonlin. Its.:",file=file2)
cat("Reading number of function evaluation failures... \n")
nfe2     = get_kinsol(mystring="Func. Evals.:",file=file2)
cat("Reading number of linear convergence failures... \n")
lcf2     = get_kinsol(mystring="Lin. Conv. Fails:",file=file2)

cat("Plotting... \n")
# time seq for axis
tsteps  = length(linit1$it)
dateseq = seq(as.POSIXct("2003-01-01 00:00","%Y-%m-%d %H:%M"),by="hours",length.out=tsteps)
enddate = tail(dateseq,1)

#X11(width=12,height=8)
ofile=sprintf("KINSOL_statistics_wateruse-vs-reference.pdf")
pdf(sprintf("plots/%s",ofile),width=10.5,height=7.5,onefile = TRUE, family = "sans", fonts = NULL, version = "1.4", pointsize=14,title=sprintf("KINSOL statistics wateruse"))
par(mfrow=c(2,2))
par(mar=c(3.0,3.0,0.25,0.25),oma=c(0,0,0,0),mgp=c(1.75,0.5,0))

plot(linit1$it,t="l",xlab="time steps [h]",ylab="number of linear iterations",col="red",lwd=2)
  lines(linit2$it,t="l",col="RoyalBlue",lwd=2)
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
  legend("topleft",c("without wateruse","incl. wateruse"),col=c("red","RoyalBlue"),lwd=c(2,2),lty=c(1,1),bty="n")
plot(nlinit1$it,t="l",xlab="time steps [h]",ylab="number of nonlinear iterations",col="red",lwd=2)
  lines(nlinit2$it,t="l",col="RoyalBlue",lwd=2)
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(nfe1$it,t="l",xlab="time steps [h]",ylab="function evaluation failures",col="red",lwd=2)
  lines(nfe2$it,t="l",col="RoyalBlue",lwd=2)
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
plot(lcf1$it,t="l",xlab="time steps [h]",ylab="linear convergence failures",col="red",lwd=2)
  lines(lcf2$it,t="l",col="RoyalBlue",lwd=2)
  axis(1,at=pretty(c(1,tsteps),n=tsteps/10),labels=FALSE,tcl=-0.4)
dev.off()


