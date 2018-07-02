## Reginald Edwards
## 06 March 2018
##
## DESCRIPTION: Variable Importance

rm(list=ls())
gc()
load('data/compa.RData')

model.vars <- c("roa",
                "ps",
                "pcf",
                "evm",
                "bm",
                "dpr",
                "npm",
                "opmbd",
                "opmad",
                "gpm",
                "ptpm",
                "cfm",
                "roe",
                "roce",
                "capei",
                "equity_invcap",
                "debt_invcap",
                "totdebt_invcap",
                "int_debt",
                "int_totdebt",
                "cash_lt",
                "invt_act",
                "rect_act",
                "debt_at",
                "short_debt",
                "curr_debt",
                "lt_debt",
                "fcf_ocf",
                "adv_sale",
                "profit_lct",
                "debt_ebitda",
                "ocf_lct",
                "lt_ppent",
                "dltt_be",
                "liabs_assets",
                "debt_capital",
                "de_ratio",
                "intcov",
                "cash_ratio",
                "quick_ratio",
                "curr_ratio",
                "capital_ratio",
                "cash_debt",
                "inv_turn",
                "at_turn",
                "rect_turn",
                "pay_turn",
                "sale_invcap",
                "sale_equity",
                "sale_nwc",
                "rd_sale",
                "accruals",
                "gprof",
                "cash_conversion",
                "intcov_ratio")

#X <- compa[c('gvkey', 'fyear', 'roa.lead1', model.vars)]
o <- sample(nrow(compa), size = 10e3, replace = FALSE)
X <- compa[o, c('roa.lead1', model.vars)]

n <- nrow(X)
train <- sample(x = n, size = n*.8, replace = FALSE)
X.train <- X[train, ]
X.test <- X[-train, ]

rmse <- function(actual, predicted) sqrt(mean(((actual - predicted)**2), na.rm = TRUE))

## get the full model loss
roa.lead1 <- X.test$roa.lead1
linreg.full <- lm(roa.lead1 ~ ., data = X.train)
rmse.full <- rmse(roa.lead1, predict(linreg.full, X.test))

n.shuffles <- 10

rmse.df <- data.frame(matrix(NA, nrow = length(model.vars), ncol = 2))
names(rmse.df) <- c('variable', 'rmse.new')

for(i in 1:length(model.vars)){
  v <- model.vars[i]
  
  rmse.new <- 0
  
  df <- X.train
  
  shuffled.variable <- X[, names(X) == v]
  
  for(j in 1:n.shuffles){
    shuffled.variable <- shuffled.variable[sample(train, size = length(train), replace = FALSE)]
    df[, names(df) == v] <- shuffled.variable
    linreg.v <- lm(roa.lead1 ~ ., data = df)
    rmse.new <- rmse.new + rmse(roa.lead1, predict(linreg.v, X.test))
  }
  rmse.new <- rmse.new/n.shuffles
  rmse.df[i, 'variable'] <- v
  rmse.df[i, 'rmse.new'] <- rmse.new
}

rmse.df$rmse.increase <- round(rmse.df$rmse.new/rmse.full - 1, 2)
summary(rmse.df)

rmse.df <- rmse.df[order(rmse.df$rmse.new, decreasing = TRUE), ]
barplot(rmse.df$rmse.new, 
        horiz = TRUE,
        beside = TRUE)
