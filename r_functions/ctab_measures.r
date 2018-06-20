 ## ROC CURVES
  ctab_measures = function(a,b,c,d){
    hit_rate=0
    fa_rate=0
    oddsr=0
    logor=0
    fa_ratio=0
    base_rate=0
    pss=0
    hss=0
    n = a+b+c+d
    if (n!=0){
	    hit_rate	= a / (a+c)		 
	    fa_rate	= b / (b+d)
	    fa_ratio 	= b / (a+b)
	    base_rate	= (a+c) / n
	    hss		= 2*base_rate*(1-base_rate)*(hit_rate-fa_rate)/(base_rate+base_rate*(1-2*base_rate)*hit_rate+(1-base_rate)*(1-2*base_rate)*fa_rate)
	    pss 	= hit_rate - fa_rate
	    oddsr	= (a*d)/(b*c)
	    logor = log(oddsr)}
  return(list(odds_ratio = oddsr, log_odds_ratio = logor, hit_rate = hit_rate*100, false_alarm_rate = fa_rate*100, heidke_skill_score = hss, base_rate = base_rate, peirces_skill_score = pss ))}
