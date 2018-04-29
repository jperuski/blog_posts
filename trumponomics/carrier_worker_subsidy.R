
################################### Packages ###########################################

library(data.table)
library(foreach)
library(ggplot2)

################################### Carrier Case Analysis ##############################

## Basic Case
jobs.saved <- 1000
subsidy <- 7000000

us.man.employ <- 12258000

risk.factor.base <- jobs.saved / us.man.employ
jobs.at.risk <- us.man.employ * risk.factor.base

## Amortization
years <- 3
cost.of.protect <- jobs.at.risk * (subsidy / jobs.saved) / years

us.corp.tax <- 455100000000

perc.of.receipts <- cost.of.protect / us.corp.tax

## Worst annual loss rate
geom.return <- exp(1 / 10 * log(11.46 / 17.284)) - 1
years <- 3
jobs.at.risk <- us.man.employ * abs(geom.return)
cost.of.protect <- jobs.at.risk * (subsidy / jobs.saved) / years

################################### Whole Economy ######################################

carrier.cost.table <- foreach(risk.factor = seq(0.01, 0.99, 0.01), .combine = "rbind") %do% {
  
  jobs.at.risk <- us.man.employ * risk.factor
  
  cost.of.protect <- jobs.at.risk * (subsidy / jobs.saved) / years

  perc.of.receipts <- cost.of.protect / us.corp.tax
  
  return(data.table(risk.factor, jobs.at.risk, cost.of.protect, perc.of.receipts))
}

carrier.cost.table[, cost.in.billions := cost.of.protect / 1000000000]

ggplot(data = carrier.cost.table, aes(x = risk.factor, y = cost.in.billions)) + 
  geom_line() +
  geom_vline(xintercept = abs(geom.return), linetype = 2, colour = "red") +
  ggtitle("Cost of Subsidy ($ Billions) Depending on Jobs at Risk")

################################# Upper Bound #########################################

us.wage <- 20.62
mex.wage <- 5
annual.worker.diff <- (us.wage - mex.wage) * 40 * 50

years <- 1

upper.cost.table <- foreach(risk.factor = seq(0.01, 0.99, 0.01), .combine = "rbind") %do% {
  
  jobs.at.risk <- us.man.employ * risk.factor
  
  cost.of.protect <- jobs.at.risk * annual.worker.diff / years
  
  perc.of.receipts <- cost.of.protect / us.corp.tax
  
  return(data.table(risk.factor, jobs.at.risk, cost.of.protect, perc.of.receipts))
}

upper.cost.table[, cost.in.billions := cost.of.protect / 1000000000]

ggplot(data = upper.cost.table, aes(x = risk.factor, y = cost.in.billions)) + 
  geom_line() +
  ggtitle("Cost of Subsidy ($ Billions) Depending on Jobs at Risk")

################################# Lower Bound #########################################

# Let's say they have no intention of offshoring, but want to piggyback on benefit
# Call this the residual threat case, we'll move if you don't pay up

piggyback.subsidy <- 500

years <- 1

lower.cost.table <- foreach(risk.factor = seq(0.01, 0.99, 0.01), .combine = "rbind") %do% {
  
  jobs.at.risk <- us.man.employ * risk.factor
  
  cost.of.protect <- jobs.at.risk * piggyback.subsidy / years
  
  perc.of.receipts <- cost.of.protect / us.corp.tax
  
  return(data.table(risk.factor, jobs.at.risk, cost.of.protect, perc.of.receipts))
}

lower.cost.table[, cost.in.billions := cost.of.protect / 1000000000]

ggplot(data = lower.cost.table, aes(x = risk.factor, y = cost.in.billions)) + 
  geom_line() +
  ggtitle("Cost of Subsidy ($ Billions) Depending on Jobs at Risk")

################################ Combined Visualization #############################

total.cost.plot.data <- merge(x = merge(x = carrier.cost.table[, list(risk.factor, carrier.case.billions = cost.in.billions)]
                                        , y = lower.cost.table[, list(risk.factor, lower.case.billions = cost.in.billions)]
                                        , by = "risk.factor")
                              , y = upper.cost.table[, list(risk.factor, upper.case.billions = cost.in.billions)]
                              , by = "risk.factor")

ggplot(data = total.cost.plot.data, aes(x = risk.factor)) + 
  geom_line(aes(y = carrier.case.billions)) +
  geom_line(aes(y = lower.case.billions)) +
  geom_line(aes(y = upper.case.billions)) +
  geom_vline(xintercept = abs(geom.return), linetype = 2, colour = "red") +
  ggtitle("Cost of Subsidy ($ Billions) Depending on Jobs at Risk")

percent.receipt.plot.data <- merge(x = merge(x = carrier.cost.table[, list(risk.factor
                                                                           , carrier.case.percent = perc.of.receipts)]
                                             , y = lower.cost.table[, list(risk.factor
                                                                           , lower.case.percent = perc.of.receipts)]
                                             , by = "risk.factor")
                                   , y = upper.cost.table[, list(risk.factor
                                                                 , upper.case.percent = perc.of.receipts)]
                                   , by = "risk.factor")

ggplot(data = percent.receipt.plot.data, aes(x = risk.factor)) + 
  geom_line(aes(y = carrier.case.percent)) +
  geom_line(aes(y = lower.case.percent)) +
  geom_line(aes(y = upper.case.percent)) +
  geom_vline(xintercept = abs(geom.return), linetype = 2, colour = "red") +
  ggtitle("Cost of Subsidy As a % of Tax Receipts")
