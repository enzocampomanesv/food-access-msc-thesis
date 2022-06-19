## VIF Calculation
## T.A. GROEN (GROEN@ITC.NL)
## JUNE 2013
VIFcalc<-function(d)
{
  result<-data.frame(var=c(names(d)),
                     VIF=numeric(length(d[1,])))
  for(i in (1:length(d[1,]))) 
    {
    result$VIF[i] <-1/(1-summary(lm(d[,i] ~ .,data=d[,names(d)!=names(d)[i]]))$r.squared)
     }
  result<-result[sort(result$VIF,decreasing=T,index.return=T)$ix,]
  return(result)
}
