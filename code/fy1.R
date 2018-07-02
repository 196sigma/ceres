rm(list=ls())
library(sas7bdat)
library(xtable)

X <- read.sas7bdat("../data/fy1_modeling.sas7bdat")

X.names <- names(X)
names(X) <- tolower(X.names)
length(unique(X$tic))

## Limit to non-short firms
X <- X[which(X$nyears >= 10), ]

firms <- unique(as.character(X$tic))
n.firms <- length(firms)
X <- X[order(X$tic, X$fyear), ]

#n.firms <- 1
#ibm <- X[which(X$tic=='IBM'), ]

indep.variables <- c("actual_lag1",
                     "totalassets_lag1",
                     "logtotalassets_lag1", 
                     "acc_lag1", 
                     "dividends_lag1", 
                     "dividendpayer_lag1",  
                     "epsgrowth_lag1",
                     "neg_actual_lag1", 
                     "stockprcchg", 
                     "gdp_pc1", 
                     "roe_lag1", 
                     "unemployment", 
                     "ppipct", 
                     "pe_lag1",
                     "sale_lag1", 
                     "ap_lag1", 
                     "rect_lag1",
                     "logsale_lag1", 
                     "logap_lag1", 
                     "logrect_lag1",
                     "chgsale_lag1",
                     "chgap_lag1",
                     "chgrect_lag1",
                     "stockret_lag1")

ibes.variables <- c("meanest", 
                    "medest", 
                    "numest", 
                    "stdev")

###################################################################################################
# Summary Stats
###################################################################################################

get.summary.stats <- function(x){
  c(mean(x, na.rm=TRUE), median(x, na.rm=TRUE),
    min(x, na.rm=TRUE), max(x, na.rm=TRUE), sd(x, na.rm=TRUE))
}

summary.stats <- data.frame(matrix(NA, nrow=0, ncol=5))
names(summary.stats) <- c("mean","med","min","max","stdev")

summary.stats["EPS", ] <- get.summary.stats(X$actual)
summary.stats["EPS Growth", ] <- get.summary.stats(X$epsgrowth)
summary.stats["Total Assets", ] <- get.summary.stats(X$totalassets_lag1)
summary.stats["Accruals", ] <- get.summary.stats(X$acc_lag1)
summary.stats["Dividends", ] <- get.summary.stats(X$dividends_lag1)
summary.stats["Dividend Payer", ] <- get.summary.stats(X$dividendpayer_lag1)
summary.stats["Negative Earnings", ] <- get.summary.stats(X$neg_actual_lag1)
summary.stats["Delta Price", ] <- get.summary.stats(X$stockprcchg)
summary.stats["return", ] <- get.summary.stats(X$stockret_lag1)
summary.stats["PE Ratio", ] <- get.summary.stats(X$pe_lag1)
summary.stats["GDP", ] <- get.summary.stats(X$gdp_pc1)
summary.stats["ROE", ] <- get.summary.stats(X$roe_lag1)
summary.stats["Unemployment", ] <- get.summary.stats(X$unemployment)
summary.stats["Inflation (PPI)", ] <- get.summary.stats(X$ppipct)
summary.stats["Sales", ] <- get.summary.stats(X$sale_lag1)
summary.stats["AR", ] <- get.summary.stats(X$rect_lag1)
summary.stats["AP", ] <- get.summary.stats(X$ap_lag1)
summary.stats["Age", ] <- get.summary.stats(X$nyears)
summary.stats["Delta Sales", ] <- get.summary.stats(X$chgsale_lag1)
summary.stats["Delta AR", ] <- get.summary.stats(X$chgrect_lag1)
summary.stats["Delta AP", ] <- get.summary.stats(X$chgap_lag1)
summary.stats["Age", ] <- get.summary.stats(X$nyears)

summary.stats.xtable <- xtable(summary.stats, digits=3, caption=".", label="summary-stats")

print(summary.stats.xtable, type="latex", file="../tables/summary-stats.tex")

###################################################################################################
# Bivariate analysis
###################################################################################################
univariate.coeffs <- data.frame(matrix(NA, nrow=0, ncol=length(indep.variables)))
names(univariate.coeffs) <- indep.variables


i <- 1
while(i <= n.firms){
  current.ticker <- firms[i]
  current.firm <- X[which(X$tic==current.ticker), ]
  current.firm.train <- current.firm[which(current.firm$fyear < max(current.firm$fyear)), ]
  current.firm.test <- current.firm[which(current.firm$fyear == max(current.firm$fyear)), ]
  
  m.actual_lag1 <- tryCatch(lm(actual ~ actual_lag1, data=current.firm.train)$coeff[["actual_lag1"]], error = function(e) NA)    
  m.totalassets <- tryCatch(lm(actual ~ actual_lag1 + totalassets_lag1, data=current.firm.train)$coeff[["totalassets_lag1"]], error = function(e) NA)
  m.logtotalassets <- tryCatch(lm(actual ~ actual_lag1 + logtotalassets_lag1, data=current.firm.train)$coeff[["logtotalassets_lag1"]], error = function(e) NA)
  m.acc <- tryCatch(lm(actual ~ actual_lag1 + acc_lag1, data=current.firm.train)$coeff[["acc_lag1"]], error = function(e) NA)
  m.dividends <- tryCatch(lm(actual ~ actual_lag1 + dividends_lag1, data=current.firm.train)$coeff[["dividends_lag1"]], error = function(e) NA)
  m.neg_actual <- tryCatch(lm(actual ~ actual_lag1 + neg_actual_lag1, data=current.firm.train)$coeff[["neg_actual_lag1"]], error = function(e) NA)        
  m.stockprcchg <- tryCatch(lm(actual ~ actual_lag1 + stockprcchg, data=current.firm.train)$coeff[["stockprcchg"]], error = function(e) NA)
  m.stockret <- tryCatch(lm(actual ~ actual_lag1 + stockret_lag1, data=current.firm.train)$coeff[["stockret_lag1"]], error = function(e) NA)
  m.gdp <- tryCatch(lm(actual ~ actual_lag1 + gdp_pc1, data=current.firm.train)$coeff[["gdp_pc1" ]], error=function(e) NA)              
  m.roe <- tryCatch(lm(actual ~ actual_lag1 + roe_lag1, data=current.firm.train)$coeff[["roe_lag1"]], error = function(e) NA)                      
  m.unemployment <- tryCatch(lm(actual ~ actual_lag1 + unemployment, data=current.firm.train)$coeff[["unemployment"]], error = function(e) NA)
  m.ppi <- tryCatch(lm(actual ~ actual_lag1 + ppipct, data=current.firm.train)$coeff[["ppipct"]], error = function(e) NA)
  m.pe <- tryCatch(lm(actual ~ actual_lag1 + pe_lag1, data=current.firm.train)$coeff[["pe_lag1"]], error = function(e) NA)
  m.sale <- tryCatch(lm(actual ~ actual_lag1 + logsale_lag1, data=current.firm.train)$coeff[["logsale_lag1"]], error = function(e) NA)
  m.rect <- tryCatch(lm(actual ~ actual_lag1 + logrect_lag1, data=current.firm.train)$coeff[["logrect_lag1"]], error = function(e) NA)
  m.ap <- tryCatch(lm(actual ~ actual_lag1 + logap_lag1, data=current.firm.train)$coeff[["logap_lag1"]], error = function(e) NA)
  m.chgsale <- tryCatch(lm(actual ~ actual_lag1 + chgsale_lag1, data=current.firm.train)$coeff[["chgsale_lag1"]], error = function(e) NA)
  m.chgrect <- tryCatch(lm(actual ~ actual_lag1 + chgrect_lag1, data=current.firm.train)$coeff[["chgrect_lag1"]], error = function(e) NA)
  m.chgap <- tryCatch(lm(actual ~ actual_lag1 + chgap_lag1, data=current.firm.train)$coeff[["chgap_lag1"]], error = function(e) NA)

    
  univariate.coeffs[current.ticker, c("actual_lag1", 
                                      "totalassets_lag1", 
                                      "logtotalassets_lag1", 
                                      "acc_lag1", 
                                      "dividends_lag1",
                                      "neg_actual_lag1", 
                                      "stockprcchg", 
                                      "stockret_lag1",
                                      "gdp_pc1", 
                                      "roe_lag1", 
                                      "unemployment", 
                                      "ppipct", 
                                      "logsale_lag1", 
                                      "logrect_lag1", 
                                      "logap_lag1",
                                      "chgsale_lag1", 
                                      "chgrect_lag1", 
                                      "chgap_lag1",
                                      "pe_lag1")] <- c(m.actual_lag1, 
                                                       m.totalassets, 
                                                       m.logtotalassets, 
                                                       m.acc, 
                                                       m.dividends, 
                                                       m.neg_actual, 
                                                       m.stockprcchg, 
                                                       m.stockret,
                                                       m.gdp, 
                                                       m.roe, 
                                                       m.unemployment, 
                                                       m.ppi, 
                                                       m.sale, 
                                                       m.rect, 
                                                       m.ap, 
                                                       m.chgsale, 
                                                       m.chgrect, 
                                                       m.chgap, 
                                                       m.pe)
  
  i <- i+1
}



## Create summary stats of univariate coefficients
summary.stats <- data.frame(matrix(NA, nrow=0, ncol=5))
names(summary.stats) <- c("mean","med","min","max","stdev")

summary.stats["EPS_{t-1}", ] <- get.summary.stats(univariate.coeffs$actual_lag1)
summary.stats["Total Assets", ] <- get.summary.stats(univariate.coeffs$totalassets_lag1)
summary.stats["Log Assets", ] <- get.summary.stats(univariate.coeffs$logtotalassets_lag1)
summary.stats["Accruals", ] <- get.summary.stats(univariate.coeffs$acc_lag1)
summary.stats["Dividends", ] <- get.summary.stats(univariate.coeffs$dividends_lag1)
summary.stats["Negative Earnings", ] <- get.summary.stats(univariate.coeffs$neg_actual_lag1)
summary.stats["Delta Price", ] <- get.summary.stats(univariate.coeffs$stockprcchg)
summary.stats["return", ] <- get.summary.stats(univariate.coeffs$stockret_lag1)
summary.stats["PE Ratio", ] <- get.summary.stats(univariate.coeffs$pe_lag1)
summary.stats["GDP", ] <- get.summary.stats(univariate.coeffs$gdp_pc1)
summary.stats["ROE", ] <- get.summary.stats(univariate.coeffs$roe_lag1)
summary.stats["Unemployment", ] <- get.summary.stats(univariate.coeffs$unemployment)
summary.stats["Inflation (PPI)", ] <- get.summary.stats(univariate.coeffs$ppipct)
summary.stats["Sales", ] <- get.summary.stats(univariate.coeffs$logsale_lag1)
summary.stats["AR", ] <- get.summary.stats(univariate.coeffs$logrect_lag1)
summary.stats["AP", ] <- get.summary.stats(univariate.coeffs$logap_lag1)
summary.stats["Delta Sales", ] <- get.summary.stats(univariate.coeffs$chgsale_lag1)
summary.stats["Delta AR", ] <- get.summary.stats(univariate.coeffs$chgrect_lag1)
summary.stats["Delta AP", ] <- get.summary.stats(univariate.coeffs$chgap_lag1)


summary.stats.xtable <- xtable(summary.stats, digits=4, caption=".", label="univariate-stats-eps")

print(summary.stats.xtable, type="latex", file="../tables/univariate-stats-eps.tex")

###################################################################################################
# Bivariate R-squared
###################################################################################################

univariate.rsquared <- data.frame(matrix(NA, nrow=0, ncol=length(indep.variables)))
names(univariate.rsquared) <- indep.variables

i <- 1
while(i <= n.firms){
  current.ticker <- firms[i]
  current.firm <- X[which(X$tic==current.ticker), ]
  
  current.firm.train <- current.firm[which(current.firm$fyear < max(current.firm$fyear)), ]
  current.firm.test <- current.firm[which(current.firm$fyear == max(current.firm$fyear)), ]

  m.actual_lag1 <- tryCatch(summary(lm(actual ~ actual_lag1, data=current.firm.train))$r.squared, error = function(e) NA)    
  m.totalassets <- tryCatch(summary(lm(actual ~ actual_lag1 + totalassets_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.logtotalassets <- tryCatch(summary(lm(actual ~ actual_lag1 + logtotalassets_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.acc <- tryCatch(summary(lm(actual ~ actual_lag1 + acc_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.dividends <- tryCatch(summary(lm(actual ~ actual_lag1 + dividends_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.neg_actual <- tryCatch(summary(lm(actual ~ actual_lag1 + neg_actual_lag1, data=current.firm.train))$r.squared, error = function(e) NA)        
  m.stockprcchg <- tryCatch(summary(lm(actual ~ actual_lag1 + stockprcchg, data=current.firm.train))$r.squared, error = function(e) NA)
  m.stockret <- tryCatch(summary(lm(actual ~ actual_lag1 + stockret_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.gdp <- tryCatch(summary(lm(actual ~ actual_lag1 + gdp_pc1, data=current.firm.train))$r.squared, error=function(e) NA)              
  m.roe <- tryCatch(summary(lm(actual ~ actual_lag1 + roe_lag1, data=current.firm.train))$r.squared, error = function(e) NA)                      
  m.unemployment <- tryCatch(summary(lm(actual ~ actual_lag1 + unemployment, data=current.firm.train))$r.squared, error = function(e) NA)
  m.ppi <- tryCatch(summary(lm(actual ~ actual_lag1 + ppipct, data=current.firm.train))$r.squared, error = function(e) NA)
  m.pe <- tryCatch(summary(lm(actual ~ actual_lag1 + pe_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.sale <- tryCatch(summary(lm(actual ~ actual_lag1 + logsale_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.rect <- tryCatch(summary(lm(actual ~ actual_lag1 + logrect_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.ap <- tryCatch(summary(lm(actual ~ actual_lag1 + logap_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.chgsale <- tryCatch(summary(lm(actual ~ actual_lag1 + chgsale_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.chgrect <- tryCatch(summary(lm(actual ~ actual_lag1 + chgrect_lag1, data=current.firm.train))$r.squared, error = function(e) NA)
  m.chgap <- tryCatch(summary(lm(actual ~ actual_lag1 + chgap_lag1, data=current.firm.train))$r.squared, error = function(e) NA)

  univariate.rsquared[current.ticker, c("actual_lag1", 
                                      "totalassets_lag1", 
                                      "logtotalassets_lag1", 
                                      "acc_lag1", 
                                      "dividends_lag1",
                                      "neg_actual_lag1", 
                                      "stockprcchg", 
                                      "stockret_lag1",
                                      "gdp_pc1", 
                                      "roe_lag1", 
                                      "unemployment", 
                                      "ppipct", 
                                      "logsale_lag1", 
                                      "logrect_lag1", 
                                      "logap_lag1",
                                      "chgsale_lag1", 
                                      "chgrect_lag1", 
                                      "chgap_lag1",
                                      "pe_lag1")] <- c(m.actual_lag1, m.totalassets, m.logtotalassets, m.acc, m.dividends,  
                                                       m.neg_actual, m.stockprcchg, m.stockret, m.gdp, m.roe, 
                                                       m.unemployment, m.ppi, m.sale, m.rect, m.ap, m.chgsale, m.chgrect, m.chgap, m.pe)
  

  i <- i+1
}


## Create summary stats of univariate R-Squared
summary.stats <- data.frame(matrix(NA, nrow=0, ncol=5))
names(summary.stats) <- c("mean","med","min","max","stdev")

summary.stats["EPS_{t-1}", ] <- get.summary.stats(univariate.rsquared$actual_lag1)
summary.stats["Total Assets", ] <- get.summary.stats(univariate.rsquared$totalassets_lag1)
summary.stats["Log Assets", ] <- get.summary.stats(univariate.rsquared$logtotalassets_lag1)
summary.stats["Accruals", ] <- get.summary.stats(univariate.rsquared$acc_lag1)
summary.stats["Dividends", ] <- get.summary.stats(univariate.rsquared$dividends_lag1)
summary.stats["Negative Earnings", ] <- get.summary.stats(univariate.rsquared$neg_actual_lag1)
summary.stats["Delta Price", ] <- get.summary.stats(univariate.rsquared$stockprcchg)
summary.stats["return", ] <- get.summary.stats(univariate.rsquared$stockret_lag1)
summary.stats["PE Ratio", ] <- get.summary.stats(univariate.rsquared$pe_lag1)
summary.stats["GDP", ] <- get.summary.stats(univariate.rsquared$gdp_pc1)
summary.stats["ROE", ] <- get.summary.stats(univariate.rsquared$roe_lag1)
summary.stats["Unemployment", ] <- get.summary.stats(univariate.rsquared$unemployment)
summary.stats["Inflation (PPI)", ] <- get.summary.stats(univariate.rsquared$ppipct)
summary.stats["Sales", ] <- get.summary.stats(univariate.rsquared$logsale_lag1)
summary.stats["AR", ] <- get.summary.stats(univariate.rsquared$logrect_lag1)
summary.stats["AP", ] <- get.summary.stats(univariate.rsquared$logap_lag1)
summary.stats["Delta Sales", ] <- get.summary.stats(univariate.rsquared$chgsale_lag1)
summary.stats["Delta AR", ] <- get.summary.stats(univariate.rsquared$chgrect_lag1)
summary.stats["Delta AP", ] <- get.summary.stats(univariate.rsquared$chgap_lag1)


summary.stats.xtable <- xtable(summary.stats, digits=4, caption=".", label="univariate-stats-eps-rsquared")

print(summary.stats.xtable, type="latex", file="../tables/univariate-stats-eps-rsquared.tex")

###################################################################################################
## Forecast
###################################################################################################

forecasts <- data.frame(matrix(NA, nrow=0, ncol=10))
names(forecasts) <- c("tic", "statpers", "fyear", "actual", "actual_lag1", ibes.variables, 
                      "statest")
i <- 1
while(i <= n.firms){
  current.ticker <- firms[i]
  current.firm <- X[which(X$tic==current.ticker), ]
  current.firm.train <- current.firm[which(current.firm$fyear < max(current.firm$fyear)), ]
  current.firm.test <- current.firm[which(current.firm$fyear == max(current.firm$fyear)), ]
  m <- tryCatch(lm(actual ~ actual_lag1 + 
                logtotalassets_lag1 + 
                #acc_lag1 + 
                #dividends_lag1 + 
                neg_actual_lag1 + 
                #1*I(stockprcchg>0) + 
                stockret_lag1 + 
                #stockprcchg + 
                gdp_pc1 + 
                roe_lag1 + 
                unemployment + 
                #ppipct + 
                pe_lag1 +
                #logsale_lag1 +
                chgsale_lag1 +
                chgap_lag1+
                chgrect_lag1, data=current.firm.train), error = function(e) NA)
  if(is.na(m)){i <- i+1} else{
    statest <- as.numeric(predict(m, current.firm.test))
    
    # bayesian model averaging
    lambda <- .5
    bma.statest <- lambda*current.firm.test$actual_lag1 + (1-lambda)*statest
    
    forecasts[i, ] <- c(as.character(current.firm.test$tic), 
                        as.character(current.firm.test$statpers), 
                        current.firm.test$fyear, 
                        current.firm.test[, c("actual", "actual_lag1", ibes.variables)], 
                        bma.statest)
    
    i <- i+1
  }
}
forecasts$actual <- as.numeric(forecasts$actual)
forecasts$spe.statest <- (forecasts$statest - forecasts$actual)**2
forecasts$spe.medest <- (forecasts$medest - forecasts$actual)**2
forecasts$spe.meanest <- (forecasts$meanest - forecasts$actual)**2
forecasts <- forecasts[-which(is.na(forecasts$actual)), ]
summary(forecasts)
write.csv(x=forecasts, file='../data/forecasts-fy1.csv', quote=FALSE)

# take out worse 10% of both analyst and stat forecasts
n.forecasts <- nrow(forecasts)
o1 <- order(forecasts$spe.meanest, decreasing=TRUE)[1:round(n.forecasts/10)]
o2 <- order(forecasts$spe.statest, decreasing=TRUE)[1:round(n.forecasts/10)]
o <- union(o1,o2)
forecasts1 <- forecasts[-o, ]
summary(forecasts1)

forecasts <- forecasts1
summary(forecasts)


###################################################################################################
# Evaluate forecasts
###################################################################################################
# merge forecasts with modeling data
keeps <- c("logtotalassets", "siccd", "zscore", "tic", "fyear")
forecasts <- merge(forecasts, X[, keeps], by=c('tic','fyear'))
write.csv(forecasts, '../data/forecasts-eps-fy1.csv')

####################################################################################################
## Accuracy
####################################################################################################
# how often is statest equal or better than consesnus?
x1 <- ifelse(forecasts$spe.statest <= forecasts$spe.meanest, 1, 0); summary(x1)
# how often is statest too high compared to meanest?
x2 <- ifelse(forecasts$statest > forecasts$actual, 1, 0); summary(x2)
x3 <- ifelse(forecasts$meanest > forecasts$actual, 1, 0); summary(x3)
x4 <- ifelse(forecasts$meanest > forecasts$statest, 1, 0); summary(x4)

# split by short history firms
forecasts$short <- ifelse(forecasts$nyears <= 14, 1, 0)
forecasts1 <- forecasts[which(forecasts$short==0), ]
forecasts2 <- forecasts[which(forecasts$short==1), ]
summary(ifelse(forecasts1$spe.statest <= forecasts1$spe.meanest, 1, 0))
summary(ifelse(forecasts2$spe.statest <= forecasts2$spe.meanest, 1, 0))
summary(forecasts1$spe.statest)
summary(forecasts1$spe.meanest)
summary(forecasts2$spe.statest)
summary(forecasts2$spe.meanest)

####################################################################################################
## Accuracy over time
####################################################################################################
years <- unique(forecasts$fyear)
n.years <- length(years)
spe.by.year <- data.frame(matrix(NA, nrow=n.years, ncol=4))
names(spe.by.year) <- c('year', 'statest', 'meanest', 'medest')
for(i in 1:n.years){
  year <- years[i]
  year.forecasts <- forecasts[which(forecasts$fyear==year), ]
  spe.by.year[i,'year'] <- year
  spe.by.year[i, 'statest'] <- mean(year.forecasts$spe.statest)
  spe.by.year[i, 'meanest'] <- mean(year.forecasts$spe.meanest)
  spe.by.year[i, 'medest'] <- mean(year.forecasts$spe.medest)
}

spe.by.year <- spe.by.year[order(spe.by.year$year),]
max.est <- max(spe.by.year[,c('statest', 'meanest', 'medest')])
min.est <- min(spe.by.year[,c('statest', 'meanest', 'medest')])
jpeg('../graphics/eps-by-year-fy1.jpeg', quality=100)
plot(spe.by.year$year, spe.by.year$statest, type='n', ylim=c(0, max.est), 
     main='EPS Forecast Accuracy Over Time',
     xlab='Target Year', ylab='Mean Squared Prediction Error')
points(spe.by.year$year, spe.by.year$statest, type='l', col='red', lwd=3, lty=2)
points(spe.by.year$year, spe.by.year$meanest, type='l', col='blue', lwd=3, lty=3)
points(spe.by.year$year, spe.by.year$medest, type='l', col='green', lwd=3, lty=4)
legend('topleft', c('model','meanest','medest'), lty=c(2,3,4), col=c('red','blue','green'), lwd=3)
dev.off()
spe.by.year.xtable <- xtable(spe.by.year, digits=3, caption=".", label="spe-by-year-table-fy1")

print(spe.by.year.xtable, type="latex", 
      file=paste(tables.dir, "spe-by-year-table-fy1.tex", sep="/"))
###################################################################################################
# accuracy by ex ante uncertainty
###################################################################################################
# by historical volatility of earnings

# Accuracy by dispersion level
quantile(forecasts$stdev, probs=seq(0,1,.01), na.rm=TRUE)
hi.stdev <- .1
forecasts$hi.dispersion <- ifelse(forecasts$stdev>hi.stdev, 1, 0)
# FY1 - Model - low dispersion
round(mean(forecasts[which(forecasts$hi.dispersion==0), 'spe.statest']), 3)
# FY1 - Consensus - low dispersion
round(mean(forecasts[which(forecasts$hi.dispersion==0), 'spe.meanest']), 3)
# FY1 - Model - high dispersion
round(mean(forecasts[which(forecasts$hi.dispersion==1), 'spe.statest']), 3)
# FY1 - Consensus - low dispersion
round(mean(forecasts[which(forecasts$hi.dispersion==1), 'spe.meanest']), 3)

#mean(forecasts[which(forecasts$hi.dispersion==0), 'spe.medest'])
#mean(forecasts[which(forecasts$hi.dispersion==1), 'spe.medest'])


###################################################################################################
## Reg-FD Split
###################################################################################################
regfd.year <- 2000
forecasts$post.regfd <- ifelse(forecasts$fyear > regfd.year+1, 1, ifelse(forecasts$fyear < regfd.year, 0, NA))

## Set consensus to mean analyst estimate
# FY1 - model - Pre-RegFD
round(mean(forecasts[which(forecasts$post.regfd==0), 'spe.statest']),3)
# FY1 - Consensus - Pre-RegFD
round(mean(forecasts[which(forecasts$post.regfd==0), 'spe.meanest']),3)
# FY1 - model - Post-RegFD
round(mean(forecasts[which(forecasts$post.regfd==1), 'spe.statest']),3)
# FY1 - consensus - Post-RegFD
round(mean(forecasts[which(forecasts$post.regfd==1), 'spe.meanest']),3)



#round(mean(forecasts[which(forecasts$post.regfd==1), 'spe.medest']),3)
#round(mean(forecasts[which(forecasts$post.regfd==0), 'spe.medest']),3)

###################################################################################################
## Performance by industry
###################################################################################################
forecasts <- merge(forecasts, X[,c('tic','statpers','fyear','siccd')], by=c('tic','statpers'))
sic.to.industry <- function(siccd) {
  industry <- "other"
  if(siccd>=100 & siccd<=999) industry <- 'Consumer NonDurables'
  if(siccd>=2000 & siccd<=2399) industry <- 'Consumer NonDurables'
  if(siccd>=2700 & siccd<=2749) industry <- 'Consumer NonDurables'
  if(siccd>=2770 & siccd<=2799) industry <- 'Consumer NonDurables'
  if(siccd>=3100 & siccd<=3199) industry <- 'Consumer NonDurables'
  if(siccd>=3940 & siccd<=3989) industry <- 'Consumer NonDurables'
  if(siccd>=2500 & siccd<=2519) industry <- 'Consumer Durables'
  if(siccd>=2590 & siccd<=2599) industry <- 'Consumer Durables'
  if(siccd>=3630 & siccd<=3659) industry <- 'Consumer Durables'
  if(siccd>=3710 & siccd<=3711) industry <- 'Consumer Durables'
  if(siccd>=3714 & siccd<=3714) industry <- 'Consumer Durables'
  if(siccd>=3716 & siccd<=3716) industry <- 'Consumer Durables'
  if(siccd>=3750 & siccd<=3751) industry <- 'Consumer Durables'
  if(siccd>=3792 & siccd<=3792) industry <- 'Consumer Durables'
  if(siccd>=3900 & siccd<=3939) industry <- 'Consumer Durables'
  if(siccd>=3990 & siccd<=3999) industry <- 'Consumer Durables'
  if(siccd>=2520 & siccd<=2589) industry <- 'Manufacturing'
  if(siccd>=2600 & siccd<=2699) industry <- 'Manufacturing'
  if(siccd>=2750 & siccd<=2769) industry <- 'Manufacturing'
  if(siccd>=2800 & siccd<=2829) industry <- 'Manufacturing'
  if(siccd>=2840 & siccd<=2899) industry <- 'Manufacturing'
  if(siccd>=3000 & siccd<=3099) industry <- 'Manufacturing'
  if(siccd>=3200 & siccd<=3569) industry <- 'Manufacturing'
  if(siccd>=3580 & siccd<=3621) industry <- 'Manufacturing'
  if(siccd>=3623 & siccd<=3629) industry <- 'Manufacturing'
  if(siccd>=3700 & siccd<=3709) industry <- 'Manufacturing'
  if(siccd>=3712 & siccd<=3713) industry <- 'Manufacturing'
  if(siccd>=3715 & siccd<=3715) industry <- 'Manufacturing'
  if(siccd>=3717 & siccd<=3749) industry <- 'Manufacturing'
  if(siccd>=3752 & siccd<=3791) industry <- 'Manufacturing'
  if(siccd>=3793 & siccd<=3799) industry <- 'Manufacturing'
  if(siccd>=3860 & siccd<=3899) industry <- 'Manufacturing'
  if(siccd>=1200 & siccd<=1399) industry <- 'Enrgy Oil, Gas, and Coal Extraction and Products'
  if(siccd>=2900 & siccd<=2999) industry <- 'Enrgy Oil, Gas, and Coal Extraction and Products'
  if(siccd>=3570 & siccd<=3579) industry <- 'High-tech'
  if(siccd>=3622 & siccd<=3622) industry <- 'High-tech'
  if(siccd>=3660 & siccd<=3692) industry <- 'High-tech'
  if(siccd>=3694 & siccd<=3699) industry <- 'High-tech'
  if(siccd>=3810 & siccd<=3839) industry <- 'High-tech'
  if(siccd>=7370 & siccd<=7372) industry <- 'High-tech'
  if(siccd>=7373 & siccd<=7373) industry <- 'High-tech'
  if(siccd>=7374 & siccd<=7374) industry <- 'High-tech'
  if(siccd>=7375 & siccd<=7375) industry <- 'High-tech'
  if(siccd>=7376 & siccd<=7376) industry <- 'High-tech'
  if(siccd>=7377 & siccd<=7377) industry <- 'High-tech'
  if(siccd>=7378 & siccd<=7378) industry <- 'High-tech'
  if(siccd>=7379 & siccd<=7379) industry <- 'High-tech'
  if(siccd>=7391 & siccd<=7391) industry <- 'High-tech'
  if(siccd>=8730 & siccd<=8734) industry <- 'High-tech'
  if(siccd>=4800 & siccd<=4899) industry <- 'Telcm Telephone and Television Transmission'
  if(siccd>=5000 & siccd<=5999) industry <- 'Shops Wholesale, Retail, and Some Services'
  if(siccd>=7200 & siccd<=7299) industry <- 'Shops Wholesale, Retail, and Some Services'
  if(siccd>=7600 & siccd<=7699) industry <- 'Shops Wholesale, Retail, and Some Services'
  if(siccd>=2830 & siccd<=2839) industry <- 'Healthcare'
  if(siccd>=3693 & siccd<=3693) industry <- 'Healthcare'
  if(siccd>=3840 & siccd<=3859) industry <- 'Healthcare'
  if(siccd>=8000 & siccd<=8099) industry <- 'Healthcare'
  if(siccd>=4900 & siccd<=4949) industry <- 'Utilities'
  return(industry)
}

for(i in 1:nrow(forecasts)) forecasts[i, 'industry'] <- sic.to.industry(forecasts[i, 'siccd'])
industries <- unique(forecasts$industry)
n.industries <- length(industries)
IND <- data.frame(matrix(NA, nrow=n.industries, ncol=3))
names(IND) <- c('Industry','Model', 'Consensus')
IND$Industry <- industries
for(i in 1:n.industries) {
  ind <- industries[i]
  mspe.statest <-round(mean(forecasts[which(forecasts$industry==ind), 'spe.statest']),3)
  mspe.meanest <-round(mean(forecasts[which(forecasts$industry==ind), 'spe.meanest']),3)
  IND[i,c('Model', 'Consensus')] <- c(mspe.statest, mspe.meanest)
}

spe.by.industry.xtable <- xtable(IND, digits=3, caption=".", label="spe-by-industry-table-fy1")

print(spe.by.industry.xtable, type="latex", file="../spe-by-industry-table-fy1.tex")

###################################################################################################
## Performance by Analyst following
###################################################################################################
