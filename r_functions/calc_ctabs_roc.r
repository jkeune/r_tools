source("/home/jkeune/SVA/tools/analysis-tools/r-functions/contingency_table_2x2.r")
  # for ROC curves with different thresholds (thresh)
  calc_ctabs_roc = function(df,thresholds){
    false_alarm_rate 	= rep(NA,length(thresholds))
    hit_rate 		= rep(NA,length(thresholds))
    cta= ctb= ctc= ctd	= rep(NA,length(thresholds))
    pss= hss		= rep(NA,length(thresholds))
    oddsr=logor		= rep(NA,length(thresholds))
    for (i in 1:length(thresholds)){
	ithresh = thresholds[i]
        ctab= contingency_table_2x2(df=df,threshold=ithresh)
	false_alarm_rate[i]	= ctab$scores$false_alarm_rate
	hit_rate[i] 		= ctab$scores$hit_rate
	cta[i]			= ctab$ctab$a
	ctb[i]			= ctab$ctab$b
	ctc[i]			= ctab$ctab$c
	ctd[i]			= ctab$ctab$d
	hss[i]			= ctab$scores$heidke_skill_score
	pss[i]			= ctab$scores$peirces_skill_score
	logor[i]		= ctab$scores$log_odds_ratio
	oddsr[i]		= ctab$scores$odds_ratio
    }
  return(list(false_alarm_rate=false_alarm_rate,hit_rate=hit_rate,a=cta,b=ctb,c=ctc,d=ctd,peirces_skill_score=pss,heidke_skill_score=hss,odds_ratio=oddsr,log_odds_ratio=logor))}
