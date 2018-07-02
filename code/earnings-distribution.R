## Reginald Edwards
## 16 June 2018
## 25 June 2018
## Analyze distribution of earnings beats/small profits and losses
###############################################################################

rm(list=ls())
gc()

load("data/comp_fundq.RData")
X <- comp.fundq

summary(X$epspxq)
hist(X$epspxq)
percentiles <- quantile(X$epspxq, probs = seq(0,1,.01))
p01 <- percentiles[["1%"]]
p99 <- percentiles[["99%"]]

## ECDF
## Full-sample mean of EPS
eps.mean <- round(mean(X$epspxq),2)
plot(ecdf(X$epspxq), xlim = c(p01,p99))
abline(h = .5, col = 'blue', lty = 3)
abline(v = median(X$epspxq), col = 'blue', lty = 3)
abline(v = eps.mean, col = 'blue', lty = 3)
text(x = median(X$epspxq), y = .4, median(X$epspxq))
text(x = eps.mean, y = .7, eps.mean)


## Winsorize and histogram
epspxq.1 <- X$epspxq
epspxq.1 <- pmin(epspxq.1, p99*1.1)
epspxq.1 <- pmax(epspxq.1, p01*0.9)
hist(epspxq.1)

###############################################################################
## Winsorize EPS by year
###############################################################################

###############################################################################
## Earnings management measures
###############################################################################

## Small losses/gains
n <- nrow(X)
small.losses <- which(X$epspxq == -.01)
small.gains <- which(X$epspxq == .01)
zero.eps <- which(X$epspxq == 0)

100*length(small.losses)/n
100*length(small.gains)/n
100*length(zero.eps)/n

X$small.loss <- 1*(X$epspxq == -0.01)
X$small.gain <- 1*(X$epspxq == 0.01)
X$zero.eps <- 1*(X$epspxq == 0)

small.losses.by.qtr <- aggregate(X$small.loss, by = list(X$datacqtr), FUN = mean)
names(small.losses.by.qtr) <- c("datacqtr", "small.losses")

small.gains.by.qtr <- aggregate(X$small.gain, by = list(X$datacqtr), FUN = mean)
names(small.gains.by.qtr) <- c("datacqtr", "small.gains")

zero.eps.by.qtr <- aggregate(X$zero.eps, by = list(X$datacqtr), FUN = mean)
names(zero.eps.by.qtr) <- c("datacqtr", "zero.eps")

size.by.quarter <- aggregate(X$atq, by = list(X$datacqtr), FUN = median)
names(size.by.quarter) <- c("datacqtr", "med.atq")
threshholds.by.qtr <- merge(small.losses.by.qtr, small.gains.by.qtr)
threshholds.by.qtr <- merge(threshholds.by.qtr, zero.eps.by.qtr)
threshholds.by.qtr <- merge(threshholds.by.qtr, size.by.quarter)

plot(threshholds.by.qtr$small.losses, type = 'l', 
     xlim = c(50, 200), ylim = c(0,.03), col = 'red')
lines(threshholds.by.qtr$small.gains, col = 'green')
lines(threshholds.by.qtr$zero.eps, col = rgb(0,0,1,.3), lty = 3)



## Transition matrix of EPS from one quarter to the next.
## Given a certain EPS last quarter, how likely are you to have different
## realizations of EPS next quarter?