library(foreach)
library(data.table)

years <- 5
total.months <- years * 12

get.return <- function(x) rnorm(x, 0.057, 0.044) 

inflation <- 0.02
coffees.sacrificed.mo <- 20
home.brew <- 0.25
avg.coffee.cost <- 2.07

total.saved <- 0
coffee.value <- 0

set.seed(123)
seed.seq <- round(runif(total.months, 1,10000))
n.reps <- 250

results <- foreach(mnth = 1:total.months, .combine = "rbind") %do% {
  
  set.seed(seed.seq[mnth])
  invest.ret <- get.return(n.reps)
  
  total.saved <- total.saved * (1 + invest.ret)
  
  new.coffee.cost <- (avg.coffee.cost * (1 + inflation) ^ (mnth %/% 12)) - (home.brew * (1 + inflation) ^ (mnth %/% 12))
  monthly.savings <- coffees.sacrificed.mo * new.coffee.cost
  
  total.saved <- total.saved + monthly.savings
  coffee.value <- coffee.value + monthly.savings
  
  out <- data.table(year = mnth %/% 12 + 1
                    , monthly.return.rate = mean(invest.ret)
                    , monthly.return = mean(invest.ret) * (total.saved - monthly.savings)
                    , coffees.sacrificed = monthly.savings
                    , cummulative.savings = total.saved
                    , cummulative.coffee = coffee.value)
  
  out
}