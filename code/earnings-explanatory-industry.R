## AUTHOR: Reginald Edwards
## CREATED: 09 April 2018
## MODIFIED: 10 April 2018
## DESCRIPTION: Explanatory power of lagged earnings for future earnings by 
## industry

rm(list=ls())
library(ggplot2)

## Load annual Compustat data
load('../0_datasets/comp_funda.RData')
load("data/compa.RData")
compa <- compa[-which(is.na(compa$roa)),  ]
compa <- compa[-which(is.na(compa$roa.lead1)),  ]
compa <- compa[-which(is.na(compa$ni)),  ]
compa <- compa[-which(is.na(compa$ni.lead1)),  ]
compa <- merge(compa, comp.funda[c("gvkey", "fyear", "sich")])

compa <- compa[-which(is.na(compa$sich)),  ]
compa$ff48 <- unlist(lapply(compa$sich, get.ff48))
compa <- compa[-which(is.na(compa$ff48)),  ]

## industries with too few firms
industry.freq <- data.frame(table(compa$ff48))
names(industry.freq) <- c("industry", "n")
industry.freq <- industry.freq[order(industry.freq$n, decreasing = TRUE), ]
#5, 26, 25, 29, 3
compa <- compa[!(compa$ff48 %in% c(5, 26, 25, 29, 3)), ]

n <- nrow(compa)
n.train <- floor(n*0.6)
train <- 1:n.train
compa.train <- compa[train, ]
compa.test <- compa[-train, ]

industries <- unique(compa$ff48)


###############################################################################
## In-sample R^2
###############################################################################

rsq.by.industry <- data.frame(matrix(NA, nrow = 0, ncol = 2))
names(rsq.by.industry) <- c("ff48", "rsq")

i <- 1
for(ind in industries){
  X <- compa.train[compa.train$ff48 == ind, ]
  earnings.model1 <- lm(roa.lead1 ~ roa, data = X)
  earnings.model1.summary <- summary(earnings.model1)
  earnings.model1.rsq <- earnings.model1.summary$adj.r.squared
  rsq.by.industry[i, "ff48"] <- ind
  rsq.by.industry[i, "rsq"] <- earnings.model1.rsq
  i <- i+1
}

## Add Fama-French 48 industry descriptions
rsq.by.industry$desc <- unlist(lapply(as.numeric(rsq.by.industry$ff48), ff48.descriptions))

rsq.by.industry <- rsq.by.industry[order(rsq.by.industry$rsq, decreasing = TRUE), ]

## Cleveland dot plot
library(ggthemes)

ggplot(rsq.by.industry, aes(x = rsq, y = reorder(desc, rsq))) + 
  labs(title = "Earnings predictability by indudtry", 
       subtitle = "R^2 values from regressions of current ROA on future ROA,\ngrouped by Fama-French 48 Industries (1982 - 2016)",
       caption = "Source: COMPUSTAT data; author's calculations; Ken French's website",
  x = NULL, y = NULL) + 
  geom_segment(aes(yend = desc), xend = 0, colour = "blue") +
  geom_point(size = 3, color = "blue") +
  theme_economist()+
  scale_color_economist()+
  scale_y_discrete(position = "right") +
  theme(text=element_text(size=10),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "white", linetype = "solid"),
        panel.grid.major.y = element_blank())
ggsave("output/figures/rsq-by-industry.png")


###############################################################################
## OOS RMSE
###############################################################################

rmse <- data.frame(matrix(NA, nrow = 0, ncol = 2))
names(rmse) <- c("ff48", "rmse")
i <- 1
for(ind in industries){
  X <- compa.train[compa.train$ff48 == ind, ]
  earnings.model1 <- lm(roa.lead1 ~ roa, data = X)
  p <- predict(earnings.model1, compa.test)
  rmse[i, "ff48"] <- ind
  rmse[i, "rmse"] <- sqrt(mean((p - compa.test$roa.lead1)**2, na.rm = TRUE))
  i <- i+1
}

## Add Fama-French 48 industry descriptions
rmse$desc <- unlist(lapply(as.numeric(rmse$ff48), ff48.descriptions))

ggplot(rmse, aes(x = rmse, y = reorder(desc, -rmse))) + 
  labs(title = "Earnings predictability by industry, out-of-sample",
       subtitle = "RMSE values from forecasts of future ROA based on current ROA,\ngrouped by Fama-French 48 Industries (1982 - 2016)",
       caption = "Source: COMPUSTAT data; author's calculations; Ken French's website",
       x = NULL, y = NULL) + 
  geom_segment(aes(yend = desc), xend = 0, colour = "blue") +
  geom_point(size = 3, color = "blue") +
  theme_economist() +
  scale_color_economist() + 
  scale_y_discrete(position = "right") +
  theme(text = element_text(size = 10),
        panel.grid.major.x = element_line(color = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())
ggsave("output/figures/rmse-by-industry.png")
