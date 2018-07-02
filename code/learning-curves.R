## Reginald Edwards
## 09 April 2018
##
## Compute learning curves for earnings forecasts
rm(list=ls())
library(ggplot2)

## Load annual Compustat data
load("data/compa.RData")
compa <- compa[-which(is.na(compa$roa)),  ]
compa <- compa[-which(is.na(compa$roa.lead1)),  ]
compa <- compa[-which(is.na(compa$ni)),  ]
compa <- compa[-which(is.na(compa$ni.lead1)),  ]
compa <- compa[-which(is.na(compa$accruals)),  ]


n <- nrow(compa)
n.train <- floor(n*0.6)
train <- sample(n, n.train, replace = FALSE)
compa.train <- compa[train, ]
compa.test <- compa[-train, ]

learning.curve <- data.frame(matrix(NA, nrow = 0, ncol = 3))
names(learning.curve) <- c("m", "training.error", "test.error")
i <- 1
for(m in seq(1, n.train, 1000)){
  X <- compa.train[1:m, ]
  m1 <- lm(ni.lead1 ~ ni + roa + accruals, data = X)
  p <- predict(m1, compa.test)
  learning.curve[i, "m"] <- m
  learning.curve[i, "training.error"] <- mean(m1$residuals**2, na.rm = TRUE)
  learning.curve[i, "test.error"] <- mean((compa.test$roa.lead1 - p)**2, na.rm = TRUE)
  i <- i+1
}
X <- compa.test[, c("gvkey", "fyear", "roa.lead1")]
X$p <- p

plot(test.error ~ m, data = learning.curve, col='blue', type = 'l')
lines(training.error ~ m, data = learning.curve, col='red')
