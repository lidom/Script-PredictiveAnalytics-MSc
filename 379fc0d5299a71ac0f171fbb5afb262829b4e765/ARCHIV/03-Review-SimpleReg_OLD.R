## ---- fig.align="center"----------------------------------------------------------------
n     <- 25 # sample size
## simulate data
set.seed(3)
X     <- runif(n, min = 1, max = 10)
error <- rnorm(n, mean = 0, sd = 5)
beta0 <- 1
beta1 <- 2
Y     <- beta0 + beta1 * X + error
## save simulated data as data frame
data_sim <- data.frame("Y" = Y, "X" = X)
## OLS fit
lm_obj   <- lm(Y~X, data = data_sim)
##
## Plot
par(family = "serif")
plot(x = data_sim$X, y = data_sim$Y, main="", axes=FALSE, 
     pch = 16, cex = 0.8, xlab = "X", ylab = "Y")
axis(1, tick = FALSE)
axis(2, tick = FALSE, las = 2)
abline(lm_obj, lty=2, lwd = 1.3, col="darkorange")
abline(a = beta0, b = beta1, lwd=1.3, col="darkblue")
legend("topleft",
       col=c("darkorange", "darkblue"), 
       legend = c("Sample Regression Line", 
                  "Population Regression Line"), 
       lwd=1.3, lty=c(2,1), bty="n")

## Estimates 
coef(lm_obj)


## ---------------------------------------------------------------------------------------
## Sample sizes
n_small      <-  10 # small sample size
n_large      <- 100 # large sample size

## True parameter values
beta0 <- 1
beta1 <- 1

## Generate explanatory variables (random design)
X_n_small  <- runif(n_small, min = 1, max = 10)
X_n_large  <- runif(n_large, min = 1, max = 10)

## Monte-Carlo (MC) Simulation 
## 1. Generate data
## 2. Compute and store estimates
## Repeat steps 1. and 2. many times
set.seed(3)
## Number of Monte Carlo repetitions
## How many samples to draw from the models
rep          <- 1000

## Containers to store the lm-results
n_small_list <- vector(mode = "list", length = rep)
n_large_list <- vector(mode = "list", length = rep)

for(r in 1:rep){
## Sampling from the model conditionally on X_n_small
error_n_small     <- rnorm(n_small, mean = 0, sd = 5)
Y_n_small         <- beta0 + beta1 * X_n_small + error_n_small
n_small_list[[r]] <- lm(Y_n_small ~ X_n_small)  
## Sampling from the model conditionally on X_n_large
error_n_large     <- rnorm(n_large, mean = 0, sd = 5)
Y_n_large         <- beta0 + beta1 * X_n_large + error_n_large
n_large_list[[r]] <- lm(Y_n_large ~ X_n_large)  
}

## Reading out the parameter estimates
beta0_estimates_n_small <- rep(NA, rep)
beta1_estimates_n_small <- rep(NA, rep)
beta0_estimates_n_large <- rep(NA, rep)
beta1_estimates_n_large <- rep(NA, rep)
for(r in 1:rep){
beta0_estimates_n_small[r] <- n_small_list[[r]]$coefficients[1]
beta1_estimates_n_small[r] <- n_small_list[[r]]$coefficients[2]
beta0_estimates_n_large[r] <- n_large_list[[r]]$coefficients[1]
beta1_estimates_n_large[r] <- n_large_list[[r]]$coefficients[2]
}


## ---- fig.width=6, fig.height=4.5, out.width='\\textwidth', fig.align='center'----------
## Plotting the results
library("scales") # alpha() produces transparent colors

## Define a common y-axis range
y_range <- range(beta0_estimates_n_small,
                 beta1_estimates_n_small)*1.1

## Generate the plot
par(family = "serif") # Serif fonts
## Layout of plotting area
layout(matrix(c(1:6), 2, 3, byrow = TRUE), widths = c(3,1,1))
## Plot 1
plot(x=0, y=0, axes=FALSE, xlab="X", ylab="Y", type="n",
     xlim=c(1,10), ylim=c(-5,35), main="Small Sample (n=10)")
axis(1, tick = FALSE); axis(2, tick = FALSE, las = 2)
for(r in 1:rep){
abline(n_small_list[[r]], lty=2, lwd = 1.3, col="darkorange")
}
abline(a = beta0, b = beta1, lwd=1.3, col="darkblue")
legend("topleft", col=c("darkorange", "darkblue"), legend=c(
"Sample regression lines from\nrepeated samples (cond. on X)", 
                  "Population regression line"), 
       lwd=1.3, lty=c(2,1), bty="n")
## Plot 2
plot(x=rep(0,rep), y=beta0_estimates_n_small, axes=FALSE, 
     xlab="", ylab="", pch=19, cex=1.2, ylim=y_range,
  main=expression(hat(beta)[0]~'|'~X), col=alpha("red",0.2))
points(x = 0, y=beta0, pch="-", cex = 1.2, col="black")
text(x=0, y=beta0, labels = expression(beta[0]), pos = 4)
## Plot 3
plot(x=rep(0,rep), y=beta1_estimates_n_small, axes=FALSE, 
     xlab="", ylab="", pch=19, cex=1.2, ylim=y_range,
  main=expression(hat(beta)[1]~'|'~X), col=alpha("red",0.2))
points(x = 0, y=beta1, pch="-", cex = 1.2, col="black")
text(x=0, y=beta1, labels = expression(beta[1]), pos = 4)
## Plot 4
plot(x=0, y=0, axes=FALSE, xlab="X", ylab="Y", type="n",
     xlim=c(1,10), ylim=c(-5,35), main="Large Sample (n=100)")
axis(1, tick = FALSE); axis(2, tick = FALSE, las = 2)
for(r in 1:rep){
abline(n_large_list[[r]], lty=2, lwd = 1.3, col="darkorange")
}
abline(a = beta0, b = beta1, lwd=1.3, col="darkblue")
## Plot 5
plot(x=rep(0,rep), y=beta0_estimates_n_large, axes=FALSE, 
     xlab="", ylab="", pch=19, cex=1.2, ylim=y_range,
  main=expression(hat(beta)[0]~'|'~X), col=alpha("red",0.2))
points(x = 0, y=beta0, pch="-", cex = 1.2, col="black")
text(x=0, y=beta0, labels = expression(beta[0]), pos = 4)
## Plot 6
plot(x=rep(0,rep), y=beta1_estimates_n_large, axes=FALSE, 
     xlab="", ylab="", pch=19, cex=1.2, ylim=y_range,
  main=expression(hat(beta)[1]~'|'~X), col=alpha("red",0.2))
points(x=0, y=beta1, pch="-", cex = 1.2, col="black")
text(x=0, y=beta1, labels = expression(beta[1]), pos = 4)

