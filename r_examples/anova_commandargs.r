#
# This script calculates a 1-way ANOVA for one variable of a set of simulations
#       - systematically for each treatment against everything else
#       - and considering time: here annual cycle
#       - optional: detrendend (detrend="true")
# 
# Set of simulations includes: 
#       - 2 CONFIGURATIONS      (3D, FD)
#       - 2 HYDROFACIES         (HFD1, HFD2) 
#       - 3 ENSEMBLE REALIZATIONS (3 INITIAL CONDITIONS, 2001-2002-2003)
#
# - based on the post-processed output (i.e. cut to focusdomain of 316x316)
# - considering only spectral nudging simulations
#
# 1. READ DATA
# 2. CALCULATE ANOVA
# 3. WRITE OUTPUT
# 
# for band-pass-filtered fields
# 
# with command line arguments: 
#  > Rscript calculate_1way-anova_month_bpfiltered_commandargs.r varname ilev
#  > Rscript calculate_1way-anova_month_bpfiltered_commandargs.r 'T' '50'
#
# JKe 24-01-2017

rm(list=ls())
library(ncdf)
library(fields)
library(RColorBrewer)
library(abind)

setwd("/perm/ms/spde/de5j/tools/analysis-tools/r-programs")
source("r_colors.r")
source("r-functions/anovaf.r")

# Settings
 nlon=316
 nlat=316
 nhfd=2
 nens=3
 model="cosmo"
 args = commandArgs(trailingOnly = TRUE)
 varname=args[1]
 ilev=args[2]
 #varname="T"
 #ilev=50
 yyyy="2003"
 season=c("06")
 # Input 
  #ipath="/scratch/ms/spde/de5j/terrsysmp_run/"
  ipath="/scratch/ms/spde/de5j/filtered_data_316x316_158x158_N8/"
 # Output 
  opath=sprintf("/scratch/ms/spde/de5j/analysis_output/ANOVA_sn")
  ofile=sprintf("ANOVA1_R2sig_snexp_JJA-%s_%s_sellev%s_bpfiltered_hourly.nc",yyyy,varname,ilev)

# ***** 1. READ DATA
#       Data should have the following structure: 
#               NLON x NLAT x NTIME x NENS x HFD x CONFIG

        # generic function to read data        
        read_ensemble=function(hfd,gw,nudg,fpath=ipath,var=varname,mod=model){
        for (mm in season){
         myvarens = array()
         iname=sprintf("tsmp_out_%s_d%s_%s-%s_focusdomain_316x316_sellev_%sd.nc",model,var,yyyy,mm,ilev)
         sprintf("Reading month %s",mm)
         for (iens in c(1,2,3)){
          ifile   = sprintf("%s/MODIS2000_%s_%s_%s_v0%sl/%s/bf_316x316_8_158x158_020x150_sublowpass/%s",fpath,hfd,gw,nudg,iens,var,iname)
          sprintf("Reading %s",ifile)
          ncfile  = open.ncdf(ifile)
          myvar   = get.var.ncdf(ncfile,sprintf("d%slb",var))
          if (iens==1){myvarens = array(NA,c(dim(myvar),3))}
          myvarens[,,,iens]=myvar
         }
         if(mm==season[1]){myvarensfull=myvarens}else{myvarensfull=abind(myvarensfull,myvarens,along=3)}
        }
        return(myvarensfull)}

        # TerrSysMP(3D)
        var_3d_hfd1     = read_ensemble("HFD1","3D","sn",var=varname)
        var_3d_hfd2     = read_ensemble("HFD2","3D","sn",var=varname)
        var_3d          = abind(var_3d_hfd1,var_3d_hfd2,along=5)
        # TerrSysMP(FD)
        var_1d_hfd1     = read_ensemble("HFD1","FD","sn",var=varname)
        var_1d_hfd2     = read_ensemble("HFD2","FD","sn",var=varname)
        var_1d          = abind(var_1d_hfd1,var_1d_hfd2,along=5)
        # full array
        var_array       = abind(var_3d,var_1d,along=6)
# ***** 2. APPLY ANOVA
#       Systematically for each treatment
#       i.e. config / hfd / ens / time 

        # ANOVA function
                anovaf=function(x,ret="f",alphalev=0.95){
                # x: array of dim = n x J, i.e. n time steps, J ensemble realizations
                # ret: what should be returned? 
                # ret="f" : F according to von Storch & Zwiers (1999, p. 176, Eq. 9.6)
                # ret="r" : Ra^2 according to von Storch & Zwiers (1999, p. 177)
                # ret="ss": SSA, SSE and SST
                # ret="all": all (SSE,SSA,SST,R^2,F)
                # alphalev=0.95: significance level for hypothesis test using F
                if(all(is.na(x))){return(NA)}else{
                  n=dim(x)[1]
                  J=dim(x)[2]
                  yoj=apply(x[,],c(2),FUN=mean,na.rm=T)
                  yoo=mean(x[,],na.rm=T)
                  ssa=n*sum((yoj-rep(yoo,J))^2)
                  sse=sum((c(x[,])-rep(yoj,each=n))^2)
                  sst=ssa+sse
                  f=ssa*(J*(n-1))/((J-1)*sse)
                  r=(ssa-((J-1)/(J*(n-1))*sse))/(ssa+sse)
                  fcomp=qf(alphalev,df1=(J-1),df2=(J*(n-1)),lower.tail=TRUE)    # F(J-1,J*(n-1))
                  if((f>=fcomp)&(!is.na(f))){binsig=1}else{binsig=0 } # is the difference significant?
                  if(binsig==0){rsig=NA}else{rsig=r}    # return only signifcant R
                if(ret=="f"){f=ssa*(J*(n-1))/((J-1)*sse);return(f)}
                if(ret=="r"){r=(ssa-sse*((J-1)/(J*(n-1))))/(sst);return(r)}
                if(ret=="ssa"){return(ssa)}
                if(ret=="sse"){return(sse)}
                if(ret=="sst"){return(sst)}
                if(ret=="sign"){return(binsig)}
                if(ret=="rsig"){return(rsig)}
                }}

                anova_array = function(dar){
                return( list(
                        #f=apply(dar,c(1,2),FUN=anovaf,ret="f"),
                        #r=apply(dar,c(1,2),FUN=anovaf,ret="r"),
                        #sse=apply(dar,c(1,2),FUN=anovaf,ret="sse"),
                        #ssa=apply(dar,c(1,2),FUN=anovaf,ret="ssa"),
                        #sst=apply(dar,c(1,2),FUN=anovaf,ret="sst"),
                        #sign=apply(dar,c(1,2),FUN=anovaf,ret="sign"),
                        rsig=apply(dar,c(1,2),FUN=anovaf,ret="rsig")
                ))}

        # Apply ANOVA
        # CONFIG
                cat("Calculating ANOVA using J=2 configurations as treatment...\n")
                myar=aperm(apply(var_array,c(1,2,6),FUN=cbind),c(2,3,1,4))
                var_confanova=anova_array(myar)       # J:  2 configurations (treatment)
        # HFD
                cat("Calculating ANOVA using J=2 hydrofacies as treatment...\n")
                myar=aperm(apply(var_array,c(1,2,5),FUN=cbind),c(2,3,1,4))
                var_hfdanova=anova_array(myar)        # J:  2 hydro facies (treatment)
        # ENSEMBLE
                cat("Calculating ANOVA using J=3 ensemble realizations as treatment...\n")
                myar=aperm(apply(var_array,c(1,2,4),FUN=cbind),c(2,3,1,4))
                var_ensanova=anova_array(myar)        # J:  3 realizations (treatment)
        # TIME 
                cat("Calculating ANOVA using J=14 days as treatment...\n")
                myar=aperm(apply(var_array,c(1,2,3),FUN=cbind),c(2,3,1,4))
                var_timeanova=anova_array(myar)       # J:  ntim timesteps (treatment)

#X11(width=8,height=3)
#par(mfrow=c(1,4))
#image.plot(var_confanova$rsig,col=lcolset)
#image.plot(var_hfdanova$rsig,col=lcolset)
#image.plot(var_timeanova$rsig,col=lcolset)
#image.plot(var_ensanova$rsig,col=lcolset)

# ***** 3. WRITE NETCDF FILE
#       with significant coefficient of determination 
#
library(ncdf4)

        # create output file
         # reading grid
          grid.nc       = nc_open("/perm/ms/spde/de5j/inputdata/grids/focus_mask_eur11_315x315.nc")
          nc.lon        = ncvar_get(grid.nc, "lon")[1,]
          nc.lat        = ncvar_get(grid.nc, "lat")[,1]
          nc_close(grid.nc)
          # define lat and lon as dimensions
          nc.lon.dim    = ncdim_def("lon", "degrees_east", as.double(nc.lon))
          nc.lat.dim    = ncdim_def("lat", "degrees_west", as.double(nc.lat))
          # create ncdf variable
          nc.r_conf     = ncvar_def(name="R_conf", units="-", dim=list(nc.lon.dim, nc.lat.dim), missval=NA, longname="Coefficient of determination (significant) - Configuration", prec="float")
          nc.r_hfd      = ncvar_def(name="R_hfd",  units="-", dim=list(nc.lon.dim, nc.lat.dim), missval=NA, longname="Coefficient of determination (significant) - HFD", prec="float")
          nc.r_ens      = ncvar_def(name="R_ens",  units="-", dim=list(nc.lon.dim, nc.lat.dim), missval=NA, longname="Coefficient of determination (significant) - Ensemble", prec="float")
          nc.r_time     = ncvar_def(name="R_time",  units="-", dim=list(nc.lon.dim, nc.lat.dim), missval=NA, longname="Coefficient of determination (significant) - Time", prec="float")
         # Create netcdf file
          out.nc        = nc_create(sprintf("%s/%s",opath,ofile), list(nc.r_conf,nc.r_hfd,nc.r_ens,nc.r_time),force_v4=TRUE)
          # add grid (lat/lon)
          ncatt_put(out.nc,"lon","axis","X")
          ncatt_put(out.nc,"lat","axis","Y")
          # add variables
          ncvar_put(out.nc, nc.r_conf, var_confanova$rsig)
          ncvar_put(out.nc, nc.r_hfd, var_hfdanova$rsig)
          ncvar_put(out.nc, nc.r_ens, var_ensanova$rsig)
          ncvar_put(out.nc, nc.r_time, var_timeanova$rsig)
         # close netcdf
         nc_close(out.nc)


sprintf("Successfully created: %s/%s .", opath, ofile)
sprintf("Done.")
                                                         
