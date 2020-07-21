#LAB FOR 417
#ACACIA MORAN
library(survival)
time<- c(5,8,8,9,10,10,10)
censor <- c(1,1,1,1,0,0,0)
install.packages()
KM.obj <- survfit(Surv(time,censor)~1, conf.type="none")
Surv(time,censor)
summary(KM.obj)

help(ovarian)
ovarian
KM.obj<- survfit(Surv(futime, fustat)~1, data=ovarian, conf.typ="log-log")
summary(KM.obj)
plot(KM.obj, xlab="Years", ylab="Survival Probability", main="KM Curve")

halibut<-read.table("/Users/acaciamoran/Downloads/Halibut.txt", header=TRUE)
KM.obj<- survfit(Surv(TIME, CENSOR)~1, data=halibut, conf.typ="log-log")
summary(KM.obj)
plot(KM.obj, xlab="hours", ylab="Survival Probability", main="KM Curve")
print(KM.obj)

graduate<-read.table("/Users/acaciamoran/Downloads/Graduate", header=TRUE)
plot.haz <- function(KM.obj,plot="TRUE") {
  ti <- summary(KM.obj)$time
  di <- summary(KM.obj)$n.event
  ni <- summary(KM.obj)$n.risk
  
#Est Hazard Function
  est.haz <- 1:(length(ti))
  for (i in 1:(length(ti)-1))
    est.haz[i] <- di[i]/(ni[i]*(ti[i+1]-ti[i]))
  est.haz[length(ti)] <- est.haz[length(ti)-1]
  
  if (plot=="TRUE") {
    plot(ti,est.haz,type="s",xlab="Time",ylab="Hazard Rate",main=expression(paste(hat(h),(t)[KM])))
  }
  return(list(est.haz=est.haz,time=ti))
}
par(mfrow=c(1,2))
plot.haz(KM.obj)



