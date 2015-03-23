library(nlme)
library(lme4)


CO2$Plant=factor(CO2$Plant,ordered=FALSE)
lm1=lm(uptake~conc+Treatment,data=CO2)
lme1=lme(uptake~conc+Treatment,random=~1|Type/Plant,data=CO2)
lmer1=lmer(uptake~conc+Treatment+(1|Type/Plant),data=CO2)


fitAll(lm1,CO2)
fitAll(lme1,CO2)
fitAll(lmer1,CO2)


cvAll(lm1,CO2)
cvAll(lm1,CO2,finalfit=FALSE)

cvAll(lme1,CO2)
cvAll(lme1,CO2,finalfit=FALSE)

cvAll(lmer1,CO2)
cvAll(lmer1,CO2,finalfit=FALSE)

## testing with transformed variable
library(compositions)
CO2$comp=acomp(CO2[,c("conc","uptake",'conc')])
lm0=lm(uptake~alr(comp)+Type,data=CO2)
fitAll(lm0,CO2)