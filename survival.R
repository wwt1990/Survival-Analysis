################################################################
# plots estimated survival function
# required argument: survobj = an object returned by survfit
# optional arguments: 
#   - add: if F creates a new plot, if T adds to existing plot
#   - mark.event: if T adds points for uncensored times
#   - mark.censor: if T marks the censored times
###############################################################
plotKM <- function(survobj, add=F, mark.event=T, mark.censor=F, ...) {
  x <- c(0, survobj$time)
  y <- c(1, survobj$surv)
  nx <- length(x)
  if (! add)
    plot(x,y,type="n", ...)
  segments(x[-nx],y[-nx],x[-1],y[-nx], ...) 
  segments(x0=x[-1], y0=y[-nx], y1=y[-1], lty=2, col='gray')
  
  if (mark.event) {
    x <- c(0, survobj$time[survobj$n.event > 0])
    y <- c(1, survobj$surv[survobj$n.event > 0])
    points(x,y, ...)
  }
  
  if (mark.censor) {
    x <- survobj$time[survobj$n.censor > 0]
    y <- survobj$surv[survobj$n.censor > 0]
    points(x,y, pch='|', cex=0.5)
  }
}
