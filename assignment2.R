
library("probemod")
## dichotomous moderator
modFit1=lm(kids~age*othrural)
summary(modFit1)

rural=matrix(0,nrow=1129)

for(i in 1:1129){
  if(othrural[i]==0){
    rural[i]=1
  }
}
fit=lm(kids~age*rural)
summary(fit)
##spotlight
data$educH=scale(educ,scale=F)-sd(educ)
data$educL=scale(educ,scale=F)+sd(educ)
modFit2=lm(kids~age*data$educH)
summary(modFit2)
modFit3=lm(kids~age*data$educL)
summary(modFit3)


##flood light
modFit4=lm(kids~age*educ)
flood = jn(modFit4, dv = "kids", iv = "age", mod = "educ")
flood
plot.jn(flood)
