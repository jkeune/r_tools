  ## BIN DATA
  bindata = function(data,bins){
    # bins data (array or vector) into bins and assigns middle value
    mids = hist(data,breaks=bins,plot=FALSE)$mids
    intv = findInterval(data,bins)
    if(is.null(dim(data))){binneddata  = mids[intv]}
    if(length(dim(data))>1){binneddata = array(mids[intv],dim(data))}  
  return(binneddata)}
