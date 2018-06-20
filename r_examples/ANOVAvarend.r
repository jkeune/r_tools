## source("ANOVA5var.r")

graphics.off()
library(fields)
library(ncdf)
library(clim.pact)


## Fortranauslagerung?

 einlesen2.grib<-function(datum,lauf,ebene){
  kanal <- c(1,2,5,9)

## 64.594.601.280 neue Rechenoperationen von hier...
  daten<-array(0,dim=c(421,461,20,18))

  for(zeitzahl in 4:21){
    if(zeitzahl<10){
      zeit<-paste("0",zeitzahl,sep='')
    }else{
      zeit<-paste(zeitzahl,sep='')
    }
    for(memberzahl in 1:20){
      if(memberzahl<10){
        member<-paste("m00",memberzahl,sep='')
      }else{
        member<-paste("m0",memberzahl,sep='')
      }

## Pfade benennen, auf denen die Daten liegen
      cvar<-paste("/automount/data01/NWP/COSMO-DE-EPS-post/POST-PROCESSED/",datum,lauf,"/",member,"/lpff00",zeit,"0000.",member,"_24",sep='')

## Zusammenfügen der verschiedenen Quellen
      bdata <- read.grib2.recs(cfile=cvar,recs=kanal[ebene])

      for(la in 6:416){
        for(lo in 6:456){

          minlat<-la-5
          maxlat<-la+5
          minlot<-lo-5
          maxlot<-lo+5
 
          daten[la,lo,memberzahl,zeitzahl-3]<-var(as.vector(bdata[minlat:maxlat,minlot:maxlot,1]))

        }
      }
    }
  }
##...bis hier
## Theoretische Speicherung, um sp?ter nur lesen zu m?ssen
  #filevar<-paste("Datenvar2",datum,zeit,"UTC",lauf,"UTCLauf.txt",sep='')
  #write.table(data,file=filevar)

  return(daten)
 }





read.grib2.recs <- function(recs=c(1,2),cfile,nlon=421,nlat=461,cout="out.bin"){
  
  recsnr <- length(recs)
  recs_char <- "("
   for (i in 1:(recsnr-1)){
     recs_char <- paste(recs_char,"^",recs[i],":|",sep="")    
   }
  recs_char <- shQuote(paste(recs_char,"^",recs[recsnr],":)",sep=""))
  
  cmd <- paste("wgrib2",cfile,"| egrep",recs_char,"| wgrib2 -i -no_header",cfile,"-bin",cout) # rec ist die record number der Variable
  print(cmd)
 system(cmd)
  
  x <- array( readBin(con=cout,'numeric',n=nlon*nlat*recsnr,size=4) , c(nlon,nlat,recsnr) )
  
  system(paste("rm",cout))
  return(x)
  
}

read.grib2.recs.ind <- function(recs=c(1,2),cfile,ifile,nlon=421,nlat=461,cout="out.bin"){
    
    recsnr <- length(recs)
    recs_char <- "("
    for (i in 1:(recsnr-1)){
      recs_char <- paste(recs_char,"^",recs[i],":|",sep="")    
    }
    recs_char <- shQuote(paste(recs_char,"^",recs[recsnr],":)",sep=""))
    
    cmd <- paste("cat",ifile,"| egrep",recs_char,"| wgrib2 -i -no_header",cfile,"-bin",cout) # rec ist die record number der Variable
    print(cmd)
    system(cmd)
    
    x <- array( readBin(con=cout,'numeric',n=nlon*nlat*recsnr,size=4) , c(nlon,nlat,recsnr) )
    
    system(paste("rm",cout))
    return(x)
}


# startzeit<-function(lauf){
# ##F?r die Gribfiles mit nur einem Record, Zeitraum einschr?nken
# 
# laufzahl<-as.integer(lauf)
# mod<-laufzahl %% 3
# startzw<-laufzahl-mod
# if(mod == 0){
# startzw<-startzw-3
# }
# if(startzw<0){
# startzw<-0
# }
# start<-as.character(startzw)
# if(startzw<10){
# start<-paste("0",start,sep='')
# }
# return(start)
# }

 anovasave2.grib2<- function(datum,lauf,ebene){
  graphics.off()
  ntime <- 18
  nmem  <- 20
  nlat <- 421
  nlot <- 461

##source('ANOVAvarend.r')
##anovasave.grib2("20110622","00",1) für PV

##Berechnungen der Parameter

  save <-FALSE
  save1<-FALSE
 
  daten<-array(0,dim=c(nlat,nlot,nmem,ntime))
  daten<-einlesen2.grib(datum,lauf,ebene)

  if(save1==TRUE){
   anofilevar<-paste("Dateny00datavar",datum,lauf,"UTCLauf.txt",sep='')
   y00data<-read.table(anofilevar)
  }else{
   y00data<-Y00.make(daten)

# ##Jeweils Sicherheitsspeicherungen, falls Programmabbruch
#    anofilevar<-paste("Dateny00datavar",datum,lauf,"UTCLauf.txt",sep='')
#    write.table(y00data, file=anofilevar)
   }

  if(save==TRUE){
   anofilevar<-paste("Dateny0jdatavar",datum,lauf,"UTCLauf.txt",sep='')
   y0jdata<-read.table(anofilevar)
  }else{
   y0jdata<-Y0j.refresh(daten)
# ##Jeweils Sicherheitsspeicherungen, falls Programmabbruch
#    anofilevar<-paste("Dateny0jdatavar",datum,lauf,"UTCLauf.txt",sep='')
#    write.table(y0jdata, file=anofilevar)
   }

  if(save==TRUE){
   anofilevar<-paste("DatenSSAdatavar",datum,lauf,"UTCLauf.txt",sep='')
   SSAdata<-read.table(anofilevar)
  }else{
   SSAdata<-SSA.make(y00data,y0jdata)
# ##Jeweils Sicherheitsspeicherungen, falls Programmabbruch
#    anofilevar<-paste("DatenSSAdatavar",datum,lauf,"UTCLauf.txt",sep='')
#    write.table(SSAdata,file=anofilevar)
   }

  if(save==TRUE){
   anofilevar<-paste("DatenSSEdatavar",datum,lauf,"UTCLauf.txt",sep='')
   SSEdata<-read.table(anofilevar)
  }else{
   SSEdata<-SSE.make(y0jdata,daten)

# ##Jeweils Sicherheitsspeicherungen, falls Programmabbruch
#    anofilevar<-paste("DatenSSEdatavar",datum,lauf,"UTCLauf.txt",sep='')
#    write.table(SSEdata,file=anofilevar)
   }


  #f17<-1.65408760 #Fisher-f-Grenzwert bei 95%
  f17 <- qf(0.95,ntime-1,ntime*nmem-1)

##Schleifendurchgang in R
 anovadata<- array(0,c(nlat,nlot))
 for(la in 1:nlat){
   for(lo in 1:nlot){
     for(ebene in 1:1){
 if(SSEdata[la,lo]==0){
 anovadata[la,lo]<-0
 }else{
       anovadata[la,lo]<-(327./17.)*(SSAdata[la,lo]/SSEdata[la,lo])
 }
     }
   }
 }


load("gitter.RData") # lat,lon,lev


##Speicherung der Enddaten zwecks Zeitersparnis ab Version 1.5 (16.7.2013 18:56 MESZ)
anofilevar<-paste("DatenANOVAvar",datum,"UTC",lauf,"UTCLauf.txt",sep='')
write.table(anovadata, file=anofilevar)

##Ausgabe als Datei
unten<-paste(datum,",",lauf,"UTC-Lauf_ANOVA",sup='')
PVdatei<-paste("../Bilder/ANOVA_PV_var",datum,lauf,"UTCLauf.png",sup='')
Tpotdatei<-paste("../Bilder/ANOVA_Tpot_var",datum,lauf,"UTCLauf.png",sup='')
DSIdatei<-paste("../Bilder/ANOVA_DSI_var",datum,lauf,"UTCLauf.png",sup='')
Hdatei<-paste("../Bilder/ANOVA_H_var",datum,lauf,"UTCLauf.png",sup='')
# Regendatei<-paste("../Bilder/ANOVA_Regen",datum,".png",sup='')
# U10Mdatei<-paste("../Bilder/ANOVA_U10M",datum,".png",sup='')
# V10Mdatei<-paste("../Bilder/ANOVA_V10M",datum,".png",sup='')
# Boeendatei<-paste("../Bilder/ANOVA_Boeen",datum,".png",sup='')

max<-c(0,0,0,0)
for(i in 1:4){
if(max(anovadata[,,i])<f17){
max[i]<-2
}else{
max<-max(anovadata[,,i])
}
}
if(ebene==1){
png(PVdatei)
image.plot(lon,lat,anovadata[,],xlab="geogr. Laenge",ylab="geogr. Breite",main="PV",sub=unten, zlim=c(f17,max[1]))
addland()
dev.off()
}

if(ebene==2){
png(Tpotdatei)
image.plot(lon,lat,anovadata[,],xlab="geogr. Laenge",ylab="geogr. Breite",main="Potenzielle Temperatur",sub=unten, zlim=c(f17,max[2]))
addland()
dev.off()
}

if(ebene==3){
png(DSIdatei)
image.plot(lon,lat,anovadata[,],xlab="geogr. Laenge",ylab="geogr. Breite",main="DSI",sub=unten,zlim=c(f17,max[3]))
addland()
dev.off()
}

if(ebene==4){
png(Hdatei)
image.plot(lon,lat,anovadata[,],xlab="geogr. Laenge",ylab="geogr. Breite",main="H",sub=unten,zlim=c(f17,max[4]))
addland()
dev.off()
}
graphics.off()


}




Y00.make<-function(daten){
##Bestimmt den Mittelwert über Zeit und Member

 y00data<-array(0,dim=c(421,461,4))

for(memberzahl in 1:20){
       

   for(zeitzahl in 4:21){

    y00data<-y00data+daten[,,memberzahl,zeitzahl-3]

   }
 }
y00data<-y00data/360.0
return(y00data)
}

Y0j.make<-function(zeitzahl,daten){
##Bestimmt den Mittelwert über die Member zu einer gegebenen Zeit

 y0jdata<-array(0,dim=c(421,461))
for(memberzahl in 1:20){


    y0jdata<-y0jdata+daten[,,memberzahl,zeitzahl-3]


 }
y0jdata<-y0jdata/20.0
return(y0jdata)
}

Y0j.refresh<-function(daten){

y0jdata<-array(0,dim=c(421,461,18))
    for(zeitzahl in 4:21){



    y0jdata[,,zeitzahl-3]<-y0jdata[,,zeitzahl-3]+Y0j.make(zeitzahl,daten)

   }
return(y0jdata)
}



SSA.make<-function(y00data,y0jdata){
##Berechnet den Wert des SSA (Quadratische Signalsumme)
SSAdata<-array(0,dim=c(421,461))

   for(zeitzahl in 4:21){

    SSAdata<-SSAdata+(y0jdata[,,zeitzahl-3]-y00data)^2

   }

SSAdata<-SSAdata*20
return(SSAdata)
}


SSE.make<-function(y0jdata,daten){ 
##Berechnet den Wert des SSE (Quadratische Fehlersumme)

SSEdata<-array(0,dim=c(421,461))

for(memberzahl in 1:20){
   for(zeitzahl in 4:21){

    SSEdata<-SSEdata+(daten[,,memberzahl,zeitzahl-3]-y0jdata[,,zeitzahl-3])^2

   }
 }
return(SSEdata)
}

##Hier ein Beispiel
anovasave2.grib2("20110622","00",1)
anovasave2.grib2("20110605","00",1)

anovasave2.grib2("20110627","00",1)
