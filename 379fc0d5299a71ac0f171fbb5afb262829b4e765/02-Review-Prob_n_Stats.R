## ---------------------------------------------------------------------------------------
set.seed(869)
## 1 (fair) coin-flip:
results <- sample(x = c("H", "T"), size = 5, replace = TRUE)
## Relative frequency of "H" in 5 coin-flips
length(results[results=="H"])/5

## 10 (fair) coin-flips:
results <- sample(x = c("H", "T"), size = 50, replace = TRUE)
## Relative frequency of "H" in 50 coin-flips
length(results[results=="H"])/50

## 100000 (fair) coin-flips:
results <- sample(x = c("H", "T"), size = 5000, replace = TRUE)
## Relative frequency of "H" in 5000 coin-flips
length(results[results=="H"])/5000


## ---- fig.align="center"----------------------------------------------------------------
## Install the package if not installed yet
# install.packages("mnormt")

library(mnormt)

x     <- seq(-5, 5, 0.25) 
y     <- seq(-5, 5, 0.25)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
f     <- function(x, y) pmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "blue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


## ---- fig.align="center"----------------------------------------------------------------
## Load the package
library(mnormt)

x     <- seq(-5, 5, 0.25) 
y     <- seq(-5, 5, 0.25)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "blue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


## ---- fig.align="center", echo=FALSE, fig.width=5, fig.height=5-------------------------
library("scales")
par(lwd=2,mgp=c(1,1,0), family="serif")
# Modified to extract diagonal.
bivariate.normal <- function(x, mu, Sigma) 
  exp(-.5 * diag(t(x-mu) %*% solve(Sigma) %*% (x-mu))) / sqrt(2 * pi * det(Sigma))

mu    <- c(0,0)
Sigma <- matrix(c(1,.8,.8,1), nrow=2)

x1 <- seq(-3, 3, length.out=50)
x2 <- seq(-3, 3, length.out=50)

# plot(1:10,axes=FALSE, frame.plot=TRUE, lwd=1)

# z can now be calculated much easier.
z     <- bivariate.normal(t(expand.grid(x1,x2)),mu,Sigma)
dim(z)<-c(length(x1),length(x2))
contour(x1, x2, z, col="#4545FF", drawlabels=FALSE, nlevels=5,
        xlab=expression(X[1]), ylab=expression(X[2]),  lwd=1,xlim=c(-3,4),ylim=range(x2),frame.plot=FALSE,axes=FALSE,xaxs = "i", yaxs = "i")
image(x1, x2, z, col=gray(seq(1,.2,len=10)), add=TRUE)
contour(x1, x2, z, col=alpha("darkblue", 0.5), drawlabels=FALSE, nlevels=5, add=TRUE)
#box(lwd=.5)
axis(1,at = -3:3, labels=FALSE,lwd.ticks=1)
axis(2,labels=FALSE,lwd.ticks=1)

# Marginal probability distribution: http://mpdc.mae.cornell.edu/Courses/MAE714/biv-normal.pdf
# Please check this, I'm not sure it is correct.
marginal.x1<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[1,2]^2)) / (Sigma[1,2]*sqrt(2*pi))
marginal.x2<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[2,1]^2)) / (Sigma[2,1]*sqrt(2*pi))

# Left side solid
x.s  <- seq(from=min(x1),to=max(x1),by=0.1)
vals<-marginal.x2(x.s)
lines(vals-abs(min(x1)),x.s,lty=1,lwd=1, col="darkred")

# Bottom side solid
vals<-marginal.x1(x.s)
lines(x.s,vals-abs(min(x2)),lty=1,lwd=1, col="darkorange")
legend("topleft", col=c("darkred","darkorange"), lty=1, legend = c(expression(f[1]),expression(f[2])), bty="n",
       title = "Marginal Densities")
legend("topright", col=c("darkblue"), lty=1, legend = expression(f(x[1],x[2])), bty="n",
       title = "Joint Density")


## ---- fig.align="center", echo=FALSE, fig.width=5, fig.height=5-------------------------
par(lwd=2,mgp=c(1,1,0), family="serif")
# Modified to extract diagonal.
bivariate.normal <- function(x, mu, Sigma) 
  exp(-.5 * diag(t(x-mu) %*% solve(Sigma) %*% (x-mu))) / sqrt(2 * pi * det(Sigma))

mu    <- c(0,0)
Sigma <- matrix(c(1,.8,.8,1), nrow=2)

x1 <- seq(-3, 3, length.out=50)
x2 <- seq(-3, 3, length.out=50)

# plot(1:10,axes=FALSE, frame.plot=TRUE, lwd=1)

# z can now be calculated much easier.
# z     <- bivariate.normal(t(expand.grid(x1,x2)),mu,Sigma)
# dim(z)<-c(length(x1),length(x2))
# contour(x1, x2, z, col="#4545FF", drawlabels=FALSE, nlevels=5,
#         xlab=expression(X[1]), ylab=expression(X[2]),  lwd=1,xlim=c(-3,4),ylim=range(x2),frame.plot=FALSE,axes=FALSE,xaxs = "i", yaxs = "i")


z     <- bivariate.normal(t(expand.grid(x1,x2)),mu,Sigma)
dim(z)<-c(length(x1),length(x2))
contour(x1, x2, z, col="#4545FF", drawlabels=FALSE, nlevels=5,
        xlab=expression(X[1]), ylab=expression(X[2]),  lwd=1,xlim=c(-3,4),ylim=range(x2),frame.plot=FALSE,axes=FALSE,xaxs = "i", yaxs = "i")
image(x1, x2, z, col=gray(seq(1,.2,len=10)), add=TRUE)
contour(x1, x2, z, col=alpha("darkblue", 0.5), drawlabels=FALSE, nlevels=5, add=TRUE)

#box(lwd=.5)
axis(1,labels=FALSE,lwd.ticks=1)
axis(2,labels=FALSE,lwd.ticks=1)
abline(v=.7, col=1, lwd=1.1, lty=2)
text(2, -2, labels=expression(x[1]==0.7))

# Dotted
f    <- function(x1,x2) bivariate.normal(t(cbind(x1,x2)),mu,Sigma)
x.s  <- seq(from=min(x1),to=max(x1),by=0.1)
vals <- f(x1=0.7,x2=x.s)
lines(vals-abs(min(x1)),x.s,lty=2,lwd=1.2, col="darkgreen")


marginal.x1<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[1,2]^2)) / (Sigma[1,2]*sqrt(2*pi))
marginal.x2<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[2,1]^2)) / (Sigma[2,1]*sqrt(2*pi))

# Left side solid
vals<-marginal.x2(x.s)
lines(vals-abs(min(x1)),x.s,lty=1,lwd=1, col=gray(.5, 0.5))
 
# Bottom side solid
vals<-marginal.x1(x.s)
lines(x.s,vals-abs(min(x2)),lty=1,lwd=1, col=gray(.5, 0.5))

# legend("topright", col=c("darkred","darkorange"), lty=1, legend = c(expression(f[1]),expression(f[2])), bty="n",
#        title = "Marginal Densities")
legend("topleft", col="darkgreen", lty=2.1, lwd=.98, legend = expression(f(x[2]~ "|" ~ x[1]==0.7)), bty="n",
       title = "Conditional Density")


## ---------------------------------------------------------------------------------------
set.seed(51)
## Set the parameter k
k <- 10
## Draw one realization from the discrete uniform distribution
sample(x = 1:k, size = 1, replace = TRUE)


## ---------------------------------------------------------------------------------------
set.seed(51)
## Set the parameter p
p <- 0.25
## Draw n realization from the discrete uniform distribution
n <- 5
sample(x = c(0,1), size = n, prob = c(1-p, p), replace=TRUE)

## Alternatively:
## (Bernoulli(p) equals Binomial(1,p))
rbinom(n = n, size = 1, prob = p)


## ---------------------------------------------------------------------------------------
set.seed(51)
## Set the parameters n and p
size <-   10 # number of trials
p    <- 0.25 # prob of success

## Draw n realization from the binomial distribution:
n <- 5
rbinom(n = n, size = size, prob = p)


## ---------------------------------------------------------------------------------------
## Drawing from the uniform distribution:
n <- 10
a <- 0
b <- 1
runif(n = n, min = a, max = b)


## ---- echo = T, eval = T, message = F, warning = F, fig.align='center'------------------
# draw a plot of the N(0,1) PDF
curve(dnorm(x),
      xlim = c(-3.5, 3.5),
      ylab = "Density", 
      main = "Standard Normal Density Function") 


## ---------------------------------------------------------------------------------------
## Drawing from the uniform distribution:
n     <- 12
mu    <- 0
sigma <- 1
rnorm(n = n, mean = mu, sd = sigma) 


## ---- echo = T, eval = T, message = F, warning = F, fig.align='center'------------------
# plot the PDF
curve(dchisq(x, df = 3), 
      xlim = c(0, 10), 
      ylim = c(0, 1), 
      col = "blue",
      ylab = "",
      main = "pdf and cdf of Chi-Squared Distribution, M = 3")

# add the CDF to the plot
curve(pchisq(x, df = 3), 
      xlim = c(0, 10), 
      add = TRUE, 
      col = "red")

# add a legend to the plot
legend("topleft", 
       c("PDF", "CDF"), 
       col = c("blue", "red"), 
       lty = c(1, 1))


## ---- echo = T, eval = T, message = F, warning = F, fig.align='center'------------------
# plot the density for M=1
curve(dchisq(x, df = 1), 
      xlim = c(0, 15), 
      xlab = "x", 
      ylab = "Density", 
      main = "Chi-Square Distributed Random Variables")

# add densities for M=2,...,7 to the plot using a 'for()' loop 
for (M in 2:7) {
  curve(dchisq(x, df = M),
        xlim = c(0, 15), 
        add = T, 
        col = M)
}

# add a legend
legend("topright", 
       as.character(1:7), 
       col = 1:7 , 
       lty = 1, 
       title = "D.F.")


## ---- echo = T, eval = T, message = F, warning = F, fig.align='center'------------------
# plot the standard normal density
curve(dnorm(x), 
      xlim = c(-4, 4), 
      xlab = "x", 
      lty = 2, 
      ylab = "Density", 
      main = "Densities of t Distributions")

# plot the t density for M=2
curve(dt(x, df = 2), 
      xlim = c(-4, 4), 
      col = 2, 
      add = T)

# plot the t density for M=4
curve(dt(x, df = 4), 
      xlim = c(-4, 4), 
      col = 3, 
      add = T)

# plot the t density for M=25
curve(dt(x, df = 25), 
      xlim = c(-4, 4), 
      col = 4, 
      add = T)

# add a legend
legend("topright", 
       c("N(0, 1)", "M=2", "M=4", "M=25"), 
       col = 1:4, 
       lty = c(2, 1, 1, 1))


## ---- echo = T, eval = T, message = F, warning = F--------------------------------------
pf(2, df1 = 3, df2 = 14, lower.tail = F)


## ---- echo = T, eval = T, message = F, warning = F, fig.align='center'------------------
# define coordinate vectors for vertices of the polygon
x <- c(2, seq(2, 10, 0.01), 10)
y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
curve(df(x ,3 ,14), 
      ylim = c(0, 0.8), 
      xlim = c(0, 10), 
      ylab = "Density",
      main = "Density Function")

# draw the polygon
polygon(x, y, col = "orange")

