library(survival)
library(KMsurv)
args(Surv)

allmovie<-read.csv("Movie.csv",head=T)
head(allmovie)

data(allmovie)
moviedata <- movie[, c("month", "time", "status", "gross", "genre")]
#monthdata <- moviedata[ with(moviedata, order(month, -status)), ]
sobjmovie <- Surv(time = moviedata$time, event = moviedata$status)


# survival probabilities
survmovie <- survfit(sobjmovie ~ 1, type="kaplan-meier", error="greenwood", 
                     conf.type="plain")
print(summary(survmovie))

##########################################
# Fitting the Proportional Hazards Model #
##########################################
attach(moviedata)
movie <- moviedata[complete.cases(moviedata), ]
movie.s <- with(movie, Surv(time, status==1))

movie.ph <- coxph(movie.s ~ month + genre + gross, data=movie, x=T)
print(movie.ph)

#--------------------
# Cox-Snell Residuals
#--------------------
# compute the residuals
r.status <- as.numeric(movie$status == 1)
r.cs <- r.status - residuals(movie.ph)
# estimate the cumulative hazard function of the residuals
r.cs.surv <- survfit(Surv(r.cs, r.status) ~ 1)
source('.../survival.R')
r.cs.H <- cumHazard(r.cs.surv)

#pdf('coxsnell.pdf', h=5, w=5)
#par(mar=c(4,4,1,1))
plotNA(r.cs.H, mark.event=F, col="blue2", xlim=c(0,2), ylim=c(0,2),
       xlab="Cox-Snell residual", ylab="Estimated Cumulative Hazard of residual")
abline(a=0,b=1, lty=3, col="red")  # a specifies intercept and b specifies slope
abline(v=1.05, lty=3)              # number at risk less than 20 after 1.05
#dev.off()

r.cs.H <- r.cs.H[r.cs.H$time < 1.05,]
plotNA(r.cs.H, mark.event=F, col="blue2", xlim=c(0,1.1), ylim=c(0,1.1),
       xlab="Cox-Snell residual", ylab="Estimated Cumulative Hazard of residual")
abline(a=0,b=1, lty=3, col="red")

#---------------------------
# Scaled Schoenfeld Residual
#---------------------------
r.ss <- cox.zph(movie.ph, transform="identity")
plot(r.ss)
plot(r.ss[1], ylab="Coefficient for Treatment") # little control over resulting plot

# if you are a control freak...
uncensored <- as.numeric(row.names(r.ss$y))
par(mar=c(5,5,1,1))
plot(uncensored, r.ss$y[,1], pch=16, cex=0.8, col="lightblue3",
     xlab="Survival Time (days)", 
     ylab=expression(paste(beta(t), " for treatment (placebo)")))
lines(lowess(uncensored, r.ss$y[,1], iter=0), col="red")

par(mar=c(5,5,2,2))
plot(uncensored, r.ss$y[,3], pch=16, cex=0.8, col="lightblue3",
     xlab="Survival Time (days)", 
     ylab=expression(paste(beta(t), " for age")))
lines(lowess(uncensored, r.ss$y[,3], iter=0), col="red")