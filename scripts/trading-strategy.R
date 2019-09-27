## trading-strategy.R
rm(list=ls())
source('background-data.R')
###################################################################################################
## A bunch of stuff happens in sas, see "get returns.sas"
###################################################################################################
forecasts.returns <- read.delim('../data/returns-all.txt')
n.all.firms.returns <- nrow(forecasts.returns)

###################################################################################################
## 1-year BAH returns
###################################################################################################

forecasts.returns <- forecasts.returns[order(forecasts.returns$diff1), ]
diff1.deciles <- quantile(forecasts.returns$diff1, probs=seq(0,1,.1), na.rm=TRUE); diff1.deciles
for(i in 1:n.all.firms.returns){
  if(forecasts.returns[i,'diff1'] < diff1.deciles['10%']){ forecasts.returns[i,'diff1.decile'] <- 1 }else
    if(forecasts.returns[i,'diff1'] < diff1.deciles['20%']){ forecasts.returns[i,'diff1.decile'] <- 2 }else
      if(forecasts.returns[i,'diff1'] < diff1.deciles['30%']){ forecasts.returns[i,'diff1.decile'] <- 3 }else
        if(forecasts.returns[i,'diff1'] < diff1.deciles['40%']){ forecasts.returns[i,'diff1.decile'] <- 4 }else
          if(forecasts.returns[i,'diff1'] < diff1.deciles['50%']){ forecasts.returns[i,'diff1.decile'] <- 5 }else
            if(forecasts.returns[i,'diff1'] < diff1.deciles['60%']){ forecasts.returns[i,'diff1.decile'] <- 6 }else
              if(forecasts.returns[i,'diff1'] < diff1.deciles['70%']){ forecasts.returns[i,'diff1.decile'] <- 7 }else
                if(forecasts.returns[i,'diff1'] < diff1.deciles['80%']){ forecasts.returns[i,'diff1.decile'] <- 8 }else
                  if(forecasts.returns[i,'diff1'] < diff1.deciles['90%']){ forecasts.returns[i,'diff1.decile'] <- 9 }else
                    forecasts.returns[i,'diff1.decile'] <- 10
}

A <- forecasts.returns
A$buy <- ifelse(A$diff1 >0, 1, 0)
A$bahr1.longshort <- A$bahr1*A$buy
m1.diff1 <- lm(bahr1 ~ diff1, data=A[which(A$diff1.decile == 10), ]); summary(m1.diff1)

blue1 <- rgb(0,0,1,.5)

bahr1.decile <- c(a1 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==1),'bahr1']),
                  a2 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==2),'bahr1']),
                  a3 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==3),'bahr1']),
                  a4 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==4),'bahr1']),
                  a5 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==5),'bahr1']),
                  a6 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==6),'bahr1']),
                  a7 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==7),'bahr1']),
                  a8 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==8),'bahr1']),
                  a9 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==9),'bahr1']),
                  a10 <- mean(forecasts.returns[which(forecasts.returns$diff1.decile==10),'bahr1']))
jpeg('../graphics/bahr-diff-decile.jpeg', quality=100)
barplot(bahr1.decile, col=blue1, border=NA,
        main='Buy-and-hold Returns by DIFF', 
        xlab='DIFF decile', ylab='FY1 Buy-and-hold returns')
abline(h=0)
dev.off()

## less granular
bahr1.decile2 <- c(b1 <- round(mean(forecasts.returns[which(forecasts.returns$diff1.decile < 3),'bahr1']),3),
                  b2 <- round(mean(forecasts.returns[which(forecasts.returns$diff1.decile %in% 3:8),'bahr1']),3),
                  b3 <- round(mean(forecasts.returns[which(forecasts.returns$diff1.decile > 8),'bahr1']),3))

jpeg('../graphics/bahr-diff-decile2.jpeg', quality=100)
barplot(bahr1.decile2, col=blue1, border=NA,
        main='Buy-and-hold Returns by DIFF', 
        ylim=c(0, b3+.2), 
        xlab='DIFF decile group', ylab='FY1 Buy-and-hold returns')
abline(h=0)
text(x=c(.5,2,3), y=c(b1, b2, b3) + 0.05, 
     labels = c( paste('Group 1', b1, sep='\n'),
                 paste('Group 2', b2, sep='\n'),
                 paste('Group 3', b3, sep='\n')))
dev.off()

m1.diff1 <- lm(bahr1 ~ diff1, data=forecasts.returns); summary(m1.diff1)
m2.diff1 <- lm(bahr2 ~ diff1, data=forecasts.returns); summary(m2.diff1)

m1.diff2 <- lm(bahr1 ~ diff2, data=forecasts.returns); summary(m1.diff2)
m2.diff2 <- lm(bahr2 ~ diff2, data=forecasts.returns); summary(m2.diff2)

m1.diff3 <- lm(bahr1 ~ diff3, data=forecasts.returns); summary(m1.diff3)
m2.diff3 <- lm(bahr2 ~ diff3, data=forecasts.returns); summary(m2.diff3)
