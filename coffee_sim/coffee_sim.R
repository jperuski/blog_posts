###################################### Load any necesary packages ########################################

library(foreach)
library(data.table)
library(ggplot2)
library(gridExtra)

################################## Declaring and Initializing Variables ###################################

## Declare and define costs, duration, and economic assumptions
years <- 20
coffees.sacrificed.mo <- function() round(runif(1,15,20))
home.brew <- 0.5
avg.coffee.cost <- 2.5
## Fed target
inflation <- 0.02

## Miscellaneous declarations
total.months <- years * 12
## Assume normal distribution of returns, monthly return and deviation sourced from somewhere I don't recall
get.return <- function(){ rnorm(1, 0.0127, 0.044) }

############################ Iterate through all months in time horizon #####################################

## simulation params
n.sim <- 100
seed.seq <- round(runif(n.sim, 1,10000))

## simulate for n reps over t months
results <- foreach(i = 1:n.sim, .combine = "rbind") %do% {
  
  set.seed(seed.seq[i])
  
  ## Initialize cumulative counters
  total.saved <- 0
  coffee.value <- 0
  
  foreach(mnth = 1:total.months, .combine = "rbind") %do% {
    
    invest.ret <- get.return()
    
    total.saved <- total.saved * (1 + invest.ret)
    
    new.coffee.cost <- (avg.coffee.cost * (1 + inflation) ^ (mnth %/% 12)) - (home.brew * (1 + inflation) ^ (mnth %/% 12))
    monthly.savings <- coffees.sacrificed.mo() * new.coffee.cost
    
    total.saved <- total.saved + monthly.savings
    coffee.value <- coffee.value + monthly.savings
    
    out <- data.table(year = mnth %/% 12 + 1
                      , month = mnth
                      , monthly.return.rate = invest.ret
                      , monthly.return = invest.ret * (total.saved - monthly.savings)
                      , coffees.sacrificed = monthly.savings
                      , cumulative.savings = total.saved
                      , cumulative.coffee = coffee.value)
    
    return(out)
  }
}

############################################# Process Results ############################################

avg.results <- results[, lapply(.SD, mean)
                       , .SDcols = c("monthly.return.rate"
                                     , "monthly.return"
                                     , "coffees.sacrificed"
                                     , "cumulative.savings"
                                     , "cumulative.coffee")
                       , by = c("year", "month")]
avg.results[, stat:= "mean"]

sd.results <- results[, lapply(.SD, sd)
                      , .SDcols = c("monthly.return.rate"
                                    , "monthly.return"
                                    , "coffees.sacrificed"
                                    , "cumulative.savings"
                                    , "cumulative.coffee")
                      , by = c("year", "month")]

emp.bounds.results <- results[, list(lb = as.numeric(lapply(.SD, quantile, probs = c(0.05)))
                                     ,ub = as.numeric(lapply(.SD, quantile, probs = c(0.95))))
                      , .SDcols = c("monthly.return.rate")
                      , by = c("year", "month")]

lcb.results <- cbind(avg.results[, 1:2, with = F]
                     , avg.results[, 3:7, with = F] - qnorm(0.975) * sd.results[, 3:7, with = F] / sqrt(100))
lcb.results[, stat:= "lcb"]


ucb.results <- cbind(avg.results[, 1:2, with = F]
                     , avg.results[, 3:7, with = F] + qnorm(0.975) * sd.results[, 3:7, with = F] / sqrt(100))
ucb.results[, stat:= "ucb"]

total.summary <- rbind(avg.results
                       , lcb.results
                       , ucb.results)

savings.data <- total.summary[, list(year, month, cumulative.savings, stat)]
savings.data <- dcast.data.table(data = savings.data
                                 , formula = year + month ~ stat
                                 , value.var = "cumulative.savings"
                                 , FUN = NULL
                                 , fill = 0)

return.data <- total.summary[stat == "mean", list(year, month, monthly.return.rate, stat)]
return.data <- dcast.data.table(data = return.data
                                , formula = year + month ~ stat
                                , value.var = "monthly.return.rate"
                                , FUN = NULL
                                , fill = 0)
return.data <- merge(x = return.data, y = emp.bounds.results, by = c("year", "month"))

################################################ Visualization #############################################

savings.plot1 <- ggplot(savings.data[month <= 120], aes(x = month, mean)) +
  geom_line(colour = "blue4") +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.15) +
  geom_line(data = avg.results[month <= 120], aes(x = month, y = cumulative.coffee), colour = "red4") +
  ylab("Cost ($)") + xlab("Months") + ggtitle("10 year Cost + Returns")

savings.plot2 <- ggplot(savings.data, aes(x = month, mean)) +
  geom_line(colour = "blue4") +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.15) +
  geom_line(data = avg.results, aes(x = month, y = cumulative.coffee), colour = "red4")+
  ylab("Cost ($)") + xlab("Months") + ggtitle("20 year Cost + Returns")

return.plot <- ggplot(return.data, aes(x = month, mean)) +
  geom_line(colour = "green4") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.15) +
  ylab("RoR (%)") + xlab("Months") + ggtitle("Rate of  Returns")


grid.arrange(savings.plot1, savings.plot2, return.plot, nrow = 3, ncol = 1)
