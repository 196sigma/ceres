## AUTHOR: Reginald Edwards
## CREATED: 31 July 2018
## DESCRIPTION: Query Compustat data from WRDS

library(dplyr)
library(RPostgres)
source('../0_datasets/wrds_login.R')
wrds <- dbConnect(Postgres(), 
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=wrds_user,
                  password=wrds_pw,
                  sslmode='require',
                  dbname='wrds')

res <- dbSendQuery(wrds, 
                   "SELECT * from comp.funda where indfmt = 'INDL' 
                   and datafmt = 'STD'
                   and popsrc = 'D'
                   and consol = 'C'
                   and final = 'Y'")
comp.funda <- dbFetch(res)
dbClearResult(res)
save(comp.funda, file = '0_datasets/comp_funda.RData')

###############################################################################
## Compute Annual Financial Ratios
###############################################################################

keep.vars <- c("gvkey", "datadate", "fyear", "fyr", "tic", "prcc_f", "seq", 
               "ceq", "txditc", "txdb", "itcb", "pstkrv", "pstkl", "pstk", 
               "prcc_f", "csho", 
               "epsfx", "epsfi", "oprepsx", "opeps", "ajex", "ebit", "spi", 
               "nopi", "sale", 
               "ibadj", "dvc", "dvp", "ib", "oibdp", "dp", "oiadp", "gp", 
               "revt", "cogs", 
               "pi", "ibc", "dpc", "at", "ni", "ibcom", "icapt", "mib", 
               "ebitda", "xsga",
               "xido", "xint", "mii", "ppent", "act", "lct", "dltt", "dlc", 
               "che", "invt", 
               "lt", "rect", "xopr", "oancf", "txp", "txt", "ap", "xrd", 
               "xad", "xlr", "capx")

comp.funda <- comp.funda[keep.vars]
comp.funda <- comp.funda[order(comp.funda$gvkey, comp.funda$fyear), ]

## Data filters
comp.funda <- comp.funda[comp.funda$at > 10, ]
comp.funda <- comp.funda[comp.funda$seq > 0, ]
comp.funda <- comp.funda[which(comp.funda$sale > 0), ]
comp.funda <- comp.funda[which(comp.funda$prcc_f > 2), ]

## label first year/observation for each firm (gvkey)
first.fyear <- aggregate(comp.funda$fyear, by  = list(comp.funda$gvkey), FUN = min)
names(first.fyear) <- c('gvkey', 'fyear')
first.fyear$first <- 1
comp.funda <- merge(comp.funda, first.fyear, all.x = TRUE)
comp.funda$first <- ifelse(is.na(comp.funda$first), 0, comp.funda$first)

# year gap between consecutive records
comp.funda$fyear.lag1 <- lag(comp.funda$fyear)
comp.funda$fyear.lag1 <- ifelse(comp.funda$first == 1, NA, comp.funda$fyear.lag1)
comp.funda$gap <- comp.funda$fyear - comp.funda$fyear.lag1

## create market value variable
comp.funda$mktval <- comp.funda$prcc_f*comp.funda$csho

# set assets (at) and revenue (sale) to missing if they are negative
comp.funda$at <- ifelse(comp.funda$at <=  0, NA, comp.funda$at)
comp.funda$sale <- ifelse(comp.funda$sale <=  0, NA, comp.funda$sale)

# preferred stock
comp.funda$pstk_new <- dplyr::coalesce(comp.funda$pstkrv, comp.funda$pstkl, comp.funda$pstk)

# Shareholder's Equity
x <- coalesce(comp.funda$txditc, rowSums(comp.funda[, c("txdb", "itcb")], na.rm = TRUE))
comp.funda$be = rowSums( data.frame(comp.funda$seq, x, -comp.funda$pstk_new), na.rm = TRUE);
comp.funda$be <- ifelse(comp.funda$be <= 0, NA, comp.funda$be)

# book-to-market
comp.funda$bm <- comp.funda$be/(comp.funda$mktval)
comp.funda$bm <- ifelse(comp.funda$bm <= 0, NA, comp.funda$bm)

# invested capital
comp.funda$icapt <- coalesce(comp.funda$icapt, rowSums(comp.funda[ , c("dltt", "pstk", "mib", "ceq")], na.rm = TRUE))

# operating cash-flow
comp.funda$ch_act <- c(NA, diff(comp.funda$act))
comp.funda$ch_che <- c(NA, diff(comp.funda$che))
comp.funda$ch_lct <- c(NA, diff(comp.funda$lct))
comp.funda$ch_dlc <- c(NA, diff(comp.funda$dlc))
comp.funda$ch_txp <- c(NA, diff(comp.funda$txp))
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ch_act"] <- NA
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ch_che"] <- NA
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ch_lct"] <- NA
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ch_dlc"] <- NA
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ch_txp"] <- NA

comp.funda$ocf <- coalesce(comp.funda$oancf, 
                           comp.funda$ib - (comp.funda$ch_act - comp.funda$ch_che - comp.funda$ch_lct + 
                                              comp.funda$ch_dlc + comp.funda$ch_txp), 
                           -comp.funda$dp)
## remove lagged generated variable if it is the first year/observation
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "ocf"] <- NA


#### Annual Valuation Ratios
# Enterprise Value Multiple:
# (market value of common stock + market value of preferred equity + market
# value of debt + minority interest - cash and investments) divided by EBITDA
comp.funda$evm <- rowSums(comp.funda[, c("dltt", "dlc", "mib", "pstk_new", 
                                         "mktval")], na.rm = TRUE)/coalesce(comp.funda$ebitda, comp.funda$oibdp, comp.funda$sale -
                                                                              comp.funda$cogs - comp.funda$xsga) 

# price-to-operating EPS, excl. EI (basic)
comp.funda$pe_op_basic <- comp.funda$prcc_f/(comp.funda$opeps/comp.funda$ajex)

# price-to-operating EPS, excl. EI (diluted)
comp.funda$pe_op_dil <- comp.funda$prcc_f/(comp.funda$oprepsx/comp.funda$ajex)

# price-to-earnings, excl. EI (diluted)
comp.funda$pe_exi <- comp.funda$prcc_f/(comp.funda$epsfx/comp.funda$ajex)

# price-to-earnings, incl. EI (diluted)
comp.funda$pe_inc <- comp.funda$prcc_f/(comp.funda$epsfi/comp.funda$ajex)

# price-to-sales ratio
comp.funda$ps <- comp.funda$prcc_f/comp.funda$sale

# price-to-cash flow
comp.funda$pcf <-comp.funda$prcc_f/comp.funda$ocf

# dividend payout ratio
comp.funda$dpr <- ifelse(comp.funda$ibadj > 0, comp.funda$dvc/comp.funda$ibadj, 0)

###############################################################################
## Profitability Ratios and Rates of Return

# net profit margin
comp.funda$npm <- comp.funda$ib/comp.funda$sale

# operating profit margin before depreciation
comp.funda$opmbd <- coalesce(comp.funda$oibdp, comp.funda$sale - 
                               comp.funda$xopr, comp.funda$revt - comp.funda$xopr)/comp.funda$sale  

# operating profit margin after depreciation
comp.funda$opmad <- coalesce(comp.funda$oiadp, comp.funda$oibdp - comp.funda$dp, 
                             comp.funda$sale - comp.funda$xopr - comp.funda$dp, comp.funda$revt - 
                               comp.funda$xopr - comp.funda$dp)/comp.funda$sale;

# gross profit margin
comp.funda$gpm <- coalesce(comp.funda$gp, comp.funda$revt - comp.funda$cogs, 
                           comp.funda$sale - comp.funda$cogs)/comp.funda$sale

# pretax profit margin
comp.funda$ptpm <- coalesce(comp.funda$pi, comp.funda$oiadp - comp.funda$xint + 
                              comp.funda$spi + comp.funda$nopi)/comp.funda$sale

# cash flow margin
comp.funda$cfm <- coalesce(comp.funda$ibc + comp.funda$dpc, comp.funda$ib + 
                             comp.funda$dp)/comp.funda$sale

# Return on Assets
#comp.funda$roa <- coalesce(comp.funda$oibdp, comp.funda$sale - comp.funda$xopr,
#  comp.funda$revt - comp.funda$xopr)/((comp.funda$at+lag(comp.funda$at))/2)
comp.funda$roa <- comp.funda$ib/((comp.funda$at+lag(comp.funda$at))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "roa"] <- NA

# Return on Equity
comp.funda$roe <- comp.funda$ib/((comp.funda$be+lag(comp.funda$be))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "roe"] <- NA

# Return on Capital Employed
comp.funda$roce <- coalesce(comp.funda$ebit, comp.funda$sale - 
                              comp.funda$cogs - comp.funda$xsga-comp.funda$dp)/((comp.funda$dltt + 
                                                                                   lag(comp.funda$dltt) + comp.funda$dlc + lag(comp.funda$dlc) + 
                                                                                   comp.funda$ceq + lag(comp.funda$ceq))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "roce"] <- NA

# effective tax rate
comp.funda$efftax <- pmax(0, comp.funda$txt/coalesce(comp.funda$pi, 
                                                     comp.funda$oiadp - comp.funda$xint + comp.funda$spi + comp.funda$nopi))

# after tax return on average common equity
comp.funda$aftret_eq <- coalesce(comp.funda$ibcom,comp.funda$ib - 
                                   comp.funda$dvp)/((comp.funda$ceq+lag(comp.funda$ceq))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), 
           "aftret_eq"] <- NA

# after tax return on invested capital
comp.funda$aftret_invcapx <- rowSums(comp.funda[, c("ib", "xint", "mii")], na.rm = TRUE)/
  lag((comp.funda$icapt + comp.funda$txditc - comp.funda$mib))
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), 
           "aftret_invcapx"] <- NA

# after tax return on total stock holder's equity  
comp.funda$aftret_equity <- comp.funda$ib/((comp.funda$seq + 
                                              lag(comp.funda$seq))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "aftret_equity"] <- NA

# pretax return on net operating assets
comp.funda$pretret_noa <- coalesce(comp.funda$oiadp, comp.funda$oibdp - 
                                     comp.funda$dp, comp.funda$sale - comp.funda$xopr - comp.funda$dp, 
                                   comp.funda$revt - comp.funda$xopr - comp.funda$dp)/((lag(comp.funda$ppent + 
                                                                                              comp.funda$act - comp.funda$lct) + (comp.funda$ppent + comp.funda$act - 
                                                                                                                                    comp.funda$lct))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "pretret_noa"] <- NA

# pretax return on total earning assets
comp.funda$pretret_earnat <- coalesce(comp.funda$oiadp, comp.funda$oibdp - 
                                        comp.funda$dp, comp.funda$sale - comp.funda$xopr - comp.funda$dp, 
                                      comp.funda$revt - comp.funda$xopr - comp.funda$dp)/((lag(comp.funda$ppent + 
                                                                                                 comp.funda$act) + (comp.funda$ppent + comp.funda$act))/2)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1),
           "pretret_earnat"] <- NA

# gross profitability as % of total assets
comp.funda$gprof <- coalesce(comp.funda$gp, comp.funda$revt - comp.funda$cogs, 
                             comp.funda$sale - comp.funda$cogs)/comp.funda$at

## Capitalization ratios
# Common Equity as % of invested capital
comp.funda$equity_invcap <-  comp.funda$ceq/comp.funda$icapt   

# Long-term debt as % of invested capital
comp.funda$debt_invcap <-  comp.funda$dltt/comp.funda$icapt    

# Total Debt as % of invested capital
comp.funda$totdebt_invcap <- (comp.funda$dltt +  comp.funda$dlc)/comp.funda$icapt  

# capitaliz comp.funda$ation ratio  
comp.funda$capital_ratio <-  comp.funda$dltt/(comp.funda$dltt + 
                                                rowSums(comp.funda[, c("ceq", "pstk_new")], na.rm = TRUE)) 

## Financial Soundness ratios
# interest as % of average long-term debt
comp.funda$int_debt <-  comp.funda$xint/((comp.funda$dltt + lag(comp.funda$dltt))/2) 
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "int_debt"] <- NA

# interest as % of average total debt
comp.funda$int_totdebt <-  comp.funda$xint/((comp.funda$dltt + 
                                               lag(comp.funda$dltt) +  comp.funda$dlc + lag(comp.funda$dlc))/2) 
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "int_totdebt"] <- NA

# Cash balance to Total Liabilities
comp.funda$cash_lt <- comp.funda$che/comp.funda$lt

# inventory as % of current assets
comp.funda$invt_act <-  comp.funda$invt/comp.funda$act

# receivables as % of current assets
comp.funda$rect_act <-  comp.funda$rect/comp.funda$act

# total debt as % of total assets
comp.funda$debt_at <- (comp.funda$dltt +  comp.funda$dlc)/comp.funda$at

# gross debt to ebitda
comp.funda$debt_ebitda <- (comp.funda$dltt + comp.funda$dlc)/
  coalesce(comp.funda$ebitda, comp.funda$oibdp, comp.funda$sale - 
             comp.funda$cogs - comp.funda$xsga)

# short term term as % of total debt
comp.funda$short_debt <-  comp.funda$dlc/(comp.funda$dltt +  comp.funda$dlc) 

# current liabilities as % of total liabilities
comp.funda$curr_debt <-  comp.funda$lct/comp.funda$lt 

# long-term debt as % of total liabilities
comp.funda$lt_debt <-  comp.funda$dltt/comp.funda$lt

# profit before D&A to current liabilities
comp.funda$profit_lct <- coalesce(comp.funda$oibdp, comp.funda$sale - comp.funda$xopr)/comp.funda$lct 

# operating cash flow to current liabilities
comp.funda$ocf_lct <- comp.funda$ocf/comp.funda$lct

# operating cash flow to total debt
comp.funda$cash_debt <- comp.funda$ocf/coalesce(comp.funda$lt, comp.funda$dltt +  comp.funda$dlc)

# Free Cash Flow/Operating Cash Flow
comp.funda$fcf_ocf <- pmax(0, (comp.funda$ocf - comp.funda$capx)/comp.funda$ocf)

# total liabilities to total tangible assets
comp.funda$lt_ppent <- comp.funda$lt/comp.funda$ppent 
comp.funda$dltt_be <- comp.funda$dltt/comp.funda$be # long-term debt to book equity

## Solvency ratios
# Debt-to-assets
comp.funda$liabs_assets <- comp.funda$lt/comp.funda$at 

# debt-to-capital
comp.funda$debt_capital <- (comp.funda$ap + (comp.funda$dlc + 
                                               comp.funda$dltt))/(comp.funda$ap + (comp.funda$dlc + comp.funda$dltt) + 
                                                                    (comp.funda$ceq + comp.funda$pstk_new))

# debt to shareholders' equity ratio
comp.funda$de_ratio <- comp.funda$lt/(comp.funda$ceq + comp.funda$pstk_new) 

# after tax interest coverage
comp.funda$intcov <- (comp.funda$xint +  comp.funda$ib)/comp.funda$xint

# interest coverage ratio
comp.funda$intcov_ratio <- coalesce(comp.funda$ebit, comp.funda$oiadp, 
                                    comp.funda$sale - comp.funda$cogs - comp.funda$xsga - comp.funda$dp)/
  comp.funda$xint

## Liquidity ratios
# cash ratio
comp.funda$cash_ratio <- comp.funda$che/comp.funda$lct
comp.funda$cash_ratio <- ifelse(comp.funda$lct > 0, comp.funda$cash_ratio, NA)

# quick ratio (acid test)
comp.funda$quick_ratio <- coalesce(comp.funda$act - comp.funda$invt, 
                                   comp.funda$che + comp.funda$rect)/comp.funda$lct 
comp.funda$quick_ratio <- ifelse(comp.funda$lct > 0, comp.funda$quick_ratio, NA)

# current ratio
comp.funda$curr_ratio <- coalesce(comp.funda$act, comp.funda$che + 
                                    comp.funda$rect + comp.funda$invt)/comp.funda$lct 
comp.funda$curr_ratio <- ifelse(comp.funda$lct > 0, comp.funda$curr_ratio, NA)

# cash conversion cycle
comp.funda$cash_conversion <- ((comp.funda$invt + lag(comp.funda$invt))/2)/
  (comp.funda$cogs/365) + ((comp.funda$rect + lag(comp.funda$rect))/2)/
  (comp.funda$sale/365) - ((comp.funda$ap + lag(comp.funda$ap))/2)/
  (comp.funda$cogs/365) 
comp.funda$cash_conversion <- ifelse(comp.funda$cash_conversion < 0, NA, 
                                     comp.funda$cash_conversion)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "cash_conversion"] <- NA

## Activity/Efficiency ratios
# inventory turnover
comp.funda$inv_turn <-  comp.funda$cogs/((comp.funda$invt + lag(comp.funda$invt))/2)  
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "inv_turn"] <- NA

# asset turnover
comp.funda$at_turn <- comp.funda$sale/((comp.funda$at + lag(comp.funda$at))/2)   
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "at_turn"] <- NA

# receivables turnover 
comp.funda$rect_turn <- comp.funda$sale/((comp.funda$rect + lag(comp.funda$rect))/2) 
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "rect_turn"] <- NA

# payables turnover
comp.funda$pay_turn <- (comp.funda$cogs + c(NA, diff(comp.funda$invt)))/((comp.funda$ap + lag(comp.funda$ap))/2) 
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "pay_turn"] <- NA

# Miscallenous ratios
#  sale per $ invested capital 
comp.funda$sale_invcap <- pmax(0, comp.funda$sale/comp.funda$icapt)

#  sales per $ total stockholders' equity
comp.funda$sale_equity <-  comp.funda$sale/comp.funda$seq 

#  sales per $ working capital  
comp.funda$sale_nwc <-  pmax(0, comp.funda$sale/(comp.funda$act - comp.funda$lct))

# R&D as % of  comp.funda$sales
comp.funda$xrd <- ifelse(is.na(comp.funda$xrd), 0, comp.funda$xrd)
comp.funda$rd_sale <- comp.funda$xrd/comp.funda$sale

# advertising as % of  comp.funda$sales
comp.funda$xad <- ifelse(is.na(comp.funda$xad), 0, comp.funda$xad)
comp.funda$adv_sale <- comp.funda$xad/comp.funda$sale

# labor expense as % of  comp.funda$sales
comp.funda$staff_sale <- pmax(comp.funda$xlr, 0)/comp.funda$sale 

## Accruals
comp.funda$avg_at <- (comp.funda$at + lag(comp.funda$at))/2 
x <- -(comp.funda$ch_act - comp.funda$ch_che - comp.funda$ch_lct + comp.funda$ch_dlc + 
         comp.funda$ch_txp - comp.funda$dp)
comp.funda$accruals  <-  coalesce(comp.funda$oancf - comp.funda$ib, x)/(comp.funda$avg_at)
comp.funda[which(comp.funda$first == 1 | comp.funda$gap != 1), "accruals"] <- NA

## Schiller's capei
comp.funda$capei <- ((comp.funda$ib + lag(comp.funda$ib, 2) + lag(comp.funda$ib, 3) + lag(comp.funda$ib, 4) + lag(comp.funda$ib, 5))/5)/comp.funda$prcc_f

o <- order(comp.funda$gvkey, comp.funda$datadate, comp.funda$fyr)
comp.funda <- comp.funda[o, ]
rm(o)