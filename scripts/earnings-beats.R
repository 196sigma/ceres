rm(list=ls())
library(ROCR)
library(haven)
source('0_code/useful-fn.R')
X <- as.data.frame(haven::read_sas('0_datasets/ibescrspcomp.sas7bdat'))
names(X) <- tolower(names(X))
X1 <- X[, c('gvkey', 'fyear', 'datadate', 'sich', 'at', 'ceq', 'xrd',
            'prcc_f', 'csho', 'ib', 'ibcom', 'ni', 'statpers',
           'fpedats', 'anndats_act','actual', 'medest', 'meanest','stdev', 'fpi', 'numdown', 'numup', 'numest', 'highest', 'lowest')]
X1$datadate <- as.Date(X1$datadate, origin = "1960-01-01")
X1$statpers <- as.Date(X1$statpers, origin="1960-01-01")
X1$fpedats <- as.Date(X1$fpedats, origin="1960-01-01")
X1$anndats_act <- as.Date(X1$anndats_act, origin="1960-01-01")
X1$error <- X1$actual - X1$medest
X1$beat <- 1*(X1$error > 0)
X1$miss <- 1*(X1$error < 0)
X1$days <- X1$anndats_act - X1$statpers
X1$year <- substr(X1$fpedats,1,4)
X1 <- X1[X1$ni > 0, ]
X1 <- X1[X1$at > 0, ]
X1 <- X1[X1$ceq > 0, ]

summary(X1)
beat <- aggregate(X1$beat, by = list(X1$fyear), FUN = sum)
names(beat) <- c('fyear', 'nbeat')
plot(beat$nbeat, type='l', xaxt = 'n')
axis(1, at=1:37, labels=beat[,'fyear'])

missed <- aggregate(X1$miss, by = list(X1$fyear), FUN = sum)
names(missed) <- c('fyear', 'nmissed')
plot(missed$nmissed, type='l', xaxt = 'n')
axis(1, at=1:37, labels=missed[,'fyear'])

plot(beat$nbeat, type='l', xaxt = 'n', ylim = c(0, 3500))
points(missed$nmissed, type='l', xaxt = 'n')
axis(1, at=1:37, labels=beat[,'fyear'])

## Lag features
## Remove firms with only one year of observation
year.counts <- aggregate(I(X1$fyear>0), by = list(X1$gvkey), FUN = sum)
names(year.counts) <- c('gvkey', 'n')
o <- which(year.counts$n <= 1)
gvkeys <- year.counts[-o, 'gvkey']
X1 <- X1[which(X1$gvkey %in% gvkeys), ]
## lag BEAT
X1 <- X1[order(X1$fyear), ]
X1 <- X1[order(X1$gvkey), ]
X1$beat_lag1 <- lag.variable(X1, 'beat', 'gvkey')
cor(X1$beat, X1$beat_lag1, use = "pairwise.complete.obs")

##

## Lag AT
X1$at_lag1 <- lag.variable(X1, 'at', 'gvkey')
## Lag ROA
X1$roa <- X1$ni/X1$at
X1$roa_lag1 <- lag.variable(X1, 'roa', 'gvkey')
## Lag ACTUAL
X1$actual_lag1 <- lag.variable(X1, 'actual', 'gvkey')

## Lag R&D
X1$rd <- X1$xrd/X1$at_lag1
X1$rd_lag1 <- lag.variable(X1, 'rd', 'gvkey')

###############################################################################
## OLS Models
m1 <- lm(beat ~ beat_lag1 + actual_lag1 + roa_lag1 + rd_lag1 + I(fyear) + 
           I(sich), data = X1); summary(m1)

X1$lmpred <- predict(m1, X1)
summary(X1$lmpred)
X1$lmpred <- pmax(0, X1$lmpred)
summary(X1$lmpred)
X1$lmpred <- pmin(1, X1$lmpred)
summary(X1$lmpred)

###############################################################################
## Logistic models
m2 <- glm(beat ~ beat_lag1 + actual_lag1 + roa_lag1 + at_lag1, 
          data = X1, family = binomial); summary(m2)
m3 <- MASS::lda(beat ~ beat_lag1 + actual_lag1 + roa_lag1 + at_lag1, data = X1)

## Accuracy
X1$log.pred <- predict(m2, X1, type = 'response')
X1$log.pred.class <- ifelse(X1$log.pred > .5, 1, 0)
o <- complete.cases(X1[,c('beat', 'log.pred.class')])
n <- nrow(X1[o, ])
table(X1$beat, X1$log.pred.class)/n

X1$lda.pred <- predict(m3, X1)$posterior[,"1"]
X1$lda.pred.class <- predict(m3, X1)$class

## ROC Curve
pred <- ROCR::prediction(X1$log.pred, X1$beat)
pred.lda <- ROCR::prediction(X1$lda.pred, X1$beat)
roc.perf = ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
roc.perf.lda = ROCR::performance(pred.lda, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
plot(roc.perf.lda, add= TRUE, col='red')
abline(a=0, b=1)

###############################################################################
## SVM
X2 <- X1[, c('fyear', 'gvkey', 'beat', 'rd_lag1', 'roa_lag1')]
X2$beat <- as.factor(X2$beat)
X2 <- na.omit(X2)
summary(X2)

rd.p1 <- quantile(X2$rd_lag1, probs = seq(0,1,.01))[["1%"]]
rd.p99 <- quantile(X2$rd_lag1, probs = seq(0,1,.01))[["99%"]]

X2 <- X2[X2$rd_lag1 < rd.p99, ]
X2 <- X2[X2$rd_lag1 > rd.p1, ]

roa.p1 <- quantile(X2$roa_lag1, probs = seq(0,1,.01))[["1%"]]
roa.p99 <- quantile(X2$roa_lag1, probs = seq(0,1,.01))[["99%"]]

X2 <- X2[X2$roa_lag1 < roa.p99, ]
X2 <- X2[X2$roa_lag1 > roa.p1, ]
X2 <- X2[sample(nrow(X2), 100, replace = FALSE), ]
plot(X2$rd_lag1, X2$roa_lag1, type = 'n')
points(X2[X2$beat == 1, 'rd_lag1'], X2[X2$beat == 1, 'roa_lag1'], col = 'red')
points(X2[X2$beat == 0, 'rd_lag1'], X2[X2$beat == 0, 'roa_lag1'], col = 'blue')


  ###############################################################################