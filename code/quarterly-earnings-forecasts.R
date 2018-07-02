## Reginald Edwards
## 16 June 2018
## 
## Forecasting earnings treating each firm as a time-series, using only lagged
## fundamentals for that firm,


rm(list=ls())
gc()

load("data/comp_fundq.RData")
X <- comp.fundq

###############################################################################
## Linear Models
###############################################################################

## Linear Regression

linreg1 <- lm(epspxq.lead1 ~ epspxq, data = comp.fundq)
summary(linreg1)
