## Reginald Edwards
## 25 June 2018
##
## Analyze Apple's Earnings
###############################################################################

rm(list=ls())
gc()
library(ggplot2)
library(ggfortify)
library(zoo)
load("data/comp_fundq.RData")
source("../0_code/useful-fn.R")
X <- comp.fundq[comp.fundq$gvkey == "001690", ]


b <- X[seq(1, nrow(X), 10), "datacqtr"]
ggplot(data = X) + 
  geom_line(aes(x = datacqtr, y = niq, group = 1)) +
  scale_x_discrete("datacqtr", breaks = b, labels = b)

X <- lead.df(X, "niq", "gvkey", "qtr")
X <- lag.df(X, "niq", "gvkey", "qtr")
## Create a stationary earnings series through differencing
X$niq.ch <- X$niq - X$niq.lag1
ggplot(data = X) + 
  geom_line(aes(x = datacqtr, y = niq.ch, group = 1)) +
  scale_x_discrete("datacqtr", breaks = b, labels = b)

X$niq <- ts(X$niq, start=c(1980,4), frequency=4)
autoplot(X$niq) +
  ggtitle("Apple Earnings, 1980-2018") +
  xlab("Quarter") +
  ylab("Net Income")
plot(decompose(niq))
