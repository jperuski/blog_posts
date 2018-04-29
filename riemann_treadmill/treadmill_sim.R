
############################# Attach required packages ###########################

library(ggplot2)
library(data.table)
library(foreach)

############################# Build converging approximation #####################

## Func: running.hill.func
## run.time - how long did you run (int)
## pace.func - formula for speed must be func of time.seq
## intervals - vector of distance between observational windows

running.hill.func <- function(run.time
                              , pace.func
                              , intervals){
  
  ## attach within function
  require(data.table)
  require(foreach)
  
  out.table <- foreach(interval = intervals, .combine = "rbind") %do% {
    
    ## generate time seq
    time.seq <- seq(0, run.time - 1, interval)
    
    ## simulate pace
    pace <- pace.func(time.seq)
    
    ## format output structure
    pace.table <- data.table(time.int = interval
                             , int.start = time.seq
                             , int.end = time.seq + interval - 1e-6
                             , pace)
    
    pace.table <- melt.data.table(data = pace.table
                                  , id.vars = c("time.int", "pace")
                                  , measure.vars = c("int.start", "int.end")
                                  , value.name = "time.series")
    
    return(pace.table)
  }
  
  return(out.table)
}

run.routine.1 <- function(time.seq) {
  0.5 * sin(time.seq / 5) + 1
}

time.intervals.1 <- c(1, 0.5, 0.25, 0.1, 0.05)

run.1 <- running.hill.func(run.time = 30
                           , pace.func = run.routine.1
                           , intervals = time.intervals.1)

ggplot(data = run.1, aes(x = time.series, y = pace)) + 
  geom_area(alpha = 0.5) + facet_grid(facets = .~time.int)
