source("/home/jkeune/SVA/tools/analysis-tools/r-functions/ctab_measures.r")
  contingency_table_2x2 = function(df,threshold){
    # estimates a contingency table of dataframe df until height of nlev and with threshold thresh_clc
    a = dim(subset(df , is.na(df$obs)==FALSE	& is.nan(df$obs)==FALSE	&  df$obs >= threshold	&  is.na(df$sim)==FALSE &  is.nan(df$sim)==FALSE 	& df$sim >= threshold))[1]
    b = dim(subset(df , is.na(df$obs)==FALSE	& is.nan(df$obs)==FALSE	&  df$obs <  threshold 	&  is.na(df$sim)==FALSE &  is.nan(df$sim)==FALSE	& df$sim >= threshold))[1]
    c = dim(subset(df , is.na(df$obs)==FALSE	& is.nan(df$obs)==FALSE	&  df$obs >= threshold 	&  is.na(df$sim)==FALSE	&  is.nan(df$sim)==FALSE	& df$sim < threshold))[1]
    d = dim(subset(df , is.na(df$obs)==FALSE	& is.nan(df$obs)==FALSE	&  df$obs <  threshold	&  is.na(df$sim)==FALSE	&  is.nan(df$sim)==FALSE	& df$sim < threshold))[1]
  n = (a+b+c+d)
  ctabm = ctab_measures(a,b,c,d)
  return(list(ctab = list(a=a,b=b,c=c,d=d), scores = ctabm, n = n))}

