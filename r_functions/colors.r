library(RColorBrewer)

col5set=brewer.pal("Set1",n=5)
col5setb=adjustcolor(brewer.pal("Set1",n=5),0.3)
diffcols50=colorRampPalette(c(rev(brewer.pal("Reds",n=4)),brewer.pal("Blues",n=4)))(50)
diffcols10=colorRampPalette(c(rev(brewer.pal("Reds",n=4)),brewer.pal("Blues",n=4)))(10)

