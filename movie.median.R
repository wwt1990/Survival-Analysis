library(survival)
library(KMsurv)
args(Surv)

movie<-read.csv("Movie.csv",head=T)
head(movie)

data(movie)
moviedata <- movie[, c("time", "status")]
sobjmovie <- Surv(time = moviedata$time, event = moviedata$status)


# survival probabilities
survmovie <- survfit(sobjmovie ~ 1, type="kaplan-meier", error="greenwood", 
                   conf.type="plain")
print(summary(survmovie))

# median by Z1
median.hat <- min(survmovie$time[survmovie$surv <= 0.5])
ci <- data.frame(time=survmovie$time, z1=(survmovie$surv - 0.5) / survmovie$std.err)
print(ci)
indx <- max(which(ci$z1 > 1.96, arr.ind=T))
lower <- survmovie$time[indx+1]
indx <- min(which(ci$z1 < -1.96, arr.ind=T))
upper <- survmovie$time[indx-1]

source('.../survival.R')

plotKM(survmovie, mark.event=T, mark.censor=T, conf=T,
       pch=16, cex=0.7, col="royalblue", ylim=c(0,1),
       xlab="Time to reach 50 million (days)", ylab="Kaplan-Meier estimate")
abline(h=0.5, lty=3)



# median by Z2 log-log transformation
median.hat <- min(survmovie$time[survmovie$surv <= 0.5])
ci <- data.frame(time=survmovie$time, z2=(log(-log(survmovie$surv))- log(-log(0.5)))*(survmovie$surv*log(survmovie$surv))/(survmovie$surv*survmovie$std.err))
print(ci)

indx <- max(which(ci$z2 > 1.96, arr.ind=T))
lower <- survmovie$time[indx+1]
lower

indx <- min(which(ci$z2 < -1.96, arr.ind=T))
upper <- survmovie$time[indx-1]
upper

source('.../survival.R')

plotKM(survmovie, mark.event=T, mark.censor=T, conf=T,
       pch=16, cex=0.7, col="royalblue", ylim=c(0,1),
       xlab="Time to reach 50 million (days)", ylab="Kaplan-Meier estimate")
abline(h=0.5, lty=3)
