
process <- function(d) sample(x = c(-1, 1), size = d, replace = T)

## generate random distances in 2 dimensions
## convert to coordinates
## store as matrix
walk_algo <- function(n, d = 2) {
  temp_dist <- replicate(n, expr = process(d))
  temp_coord <- apply(X = temp_dist, MARGIN = 1, FUN = cumsum)
  temp_coord <- rbind(matrix(rep(0, d), 1, d), temp_coord)
  return(temp_coord)
}

## plot in stripped down base graphics
algo_plot <- function(n, col_i = "black") plot(walk_algo(n), type = "l", xlab = NA, ylab = NA, tcl = 0, col = col_i)

## consistent step size, variable step count
set.seed(1234)
par(mfrow = c(3,3), xaxt = "n", yaxt = "n", bty = "n", mar=c(0.5,0.5,0.5,0.5))
for (i in 1:9) algo_plot(n = sample(c(30,40,50,60,70), 1))
par(mfrow = c(1,1))

## binding walks into a grid
set.seed(12345)
par(mfrow = c(3,3), xaxt = "n", yaxt = "n", bty = "n", mar=c(0.5,0.5,0.5,0.5))
for (i in 1:9) algo_plot(n = 500)
par(mfrow = c(1,1))

## plotting utility with noise overlays
algo_plot2 <- function(n, d) {
  walk_mat <- walk_algo(n, d)
  plot(walk_mat + rbind(matrix(rep(0, d), 1, d), matrix(rnorm(n * d, 0, 0.1), n, d))
       , type = "l", xlab = NA, ylab = NA, tcl = 0, col = "grey86")
  lines(walk_mat + rbind(matrix(rep(0, d), 1, d), matrix(rnorm(n * d, 0, 0.1), n, d))
        , type = "l", col = "grey43")
  lines(walk_mat, col = "black")
}

process <- function(d) rnorm(d, mean = 0, sd = 0.5)
set.seed(12345)
par(mfrow = c(3,3), xaxt = "n", yaxt = "n", bty = "n", mar=c(0.5,0.5,0.5,0.5))
for (i in 1:9) algo_plot(n = 15000, col = "grey50")
par(mfrow = c(1,1))

## binding noise overlay plots
set.seed(1234)
par(mfrow = c(3,3), xaxt = "n", yaxt = "n", bty = "n", mar=c(0.5,0.5,0.5,0.5))
for (i in 1:9) algo_plot2(n = 500, d = 2)
par(mfrow = c(1,1))

## Cover plot
colors <- c("grey15", "grey30", "grey45", "grey60", "grey70", "grey80")
set.seed(345)
par(mfrow = c(2,6), xaxt = "n", yaxt = "n", bty = "n", mar=c(0.5,0.5,0.5,0.5))
for (i in 1:12) algo_plot(n = 500, col_i = colors[(i-1)%%6+1])
par(mfrow = c(1,1))

