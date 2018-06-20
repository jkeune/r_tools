# This functions plots a Hovmoeller-similar diagram 
# as a filled.contour plot
# JKe 2017
hovmoller=function(harray,xvar,yvar,xname,yname,fcol,addcon=FALSE,...){
        # harray: array of dim (x,y)
        # xvar:   variable of x-axis, e.g. time 1:12 months
        # xname:  variable name of xaxis, "months"
        # yvar:   variable of y-axis
        # yname:  variable name of y-axis
        # fcol:   color palette to use
        # ...     other options
        filled.contour(xvar,yvar,harray,col=fcol,xlab=xname,ylab=yname)
        if (addcon==TRUE){contour(xvar,yvar,harray,add=TRUE)}
}

