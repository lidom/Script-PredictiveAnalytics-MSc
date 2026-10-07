## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
# When fixing rate (lambda) and changing shape (r) for Gamma Distribution,
# When the shape (r) increases, based on the formula, 
# the mean increases (shift to the right),
# the variance increases
# the skewness decreases
# the excess kurtosis decreases

### F Distribution
# Plot 1: Fix df2 and changing df1
par(mfrow=c(1,2))
curve(expr = df(x = x, df1 = 3, df2 = 5),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(0, 4),
      ylim = c(0, 1))

for (i in 1:2) {
  curve(expr = df(x = x, df1  = 3, 
                  df2=c(15,500)[i]),
        lwd = 2, col = (2:3)[i], add = TRUE)
}  
legend(x = "topright", legend = c("F(3,5)", "F(3,15)", "F(3,500)"),
       lwd = 2, col = 1:3)
curve(expr = df(x = x, df1 = 1, df2 = 30),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(0, 4),
      ylim = c(0, 1))

for (i in 1:2) {
  curve(expr = df(x = x, df1  = c(3,15)[i], 
                  df2=30),
        lwd = 2, col = (4:6)[i], add = TRUE)
}  
legend(x = "topright", legend = c("F(1,30)", "F(3,30)", "F(15,30)"),
       lwd = 2, col = 4:6)


## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
# plot the standard normal density
curve(dnorm(x), 
      xlim = c(-4, 4), 
      xlab = "", 
      lty = 2, 
      ylab = "", 
      main = "")

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
legend("topright", bty="n", 
       c("N(0, 1)", expression(t[2]), expression(t[4]), expression(t[25])), 
       col = 1:4, 
       lty = c(2, 1, 1, 1))


## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
library("scales")
curve(expr = df(x = x, df1 = 9, df2 = 120),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(0, 4),
      ylim = c(0, 1), xaxs="i",yaxs="i")

alpha <- 0.05
q     <- qf(p = 1-alpha, df1 = 9, df2 = 120)
xx    <- seq(0,q,len=25)
yy    <- df(x = xx, df1 = 9, df2 = 120)

polygon(x = c(xx,rev(xx)), y=c(yy, rep(0,length(yy))), border = NA, col = alpha("green", 0.25))
q     <- qf(p = 1-alpha, df1 = 9, df2 = 120)
xx    <- seq(q,4,len=25)
yy    <- df(x = xx, df1 = 9, df2 = 120)

polygon(x = c(xx,rev(xx)), y=c(yy, rep(0,length(yy))), border = NA, col = alpha("red", 0.25))

lines(x=c(0,q-0.02),y=c(0,0), col="darkgreen", lwd=10)
lines(x=c(q+0.02,4),y=c(0,0), col="red", lwd=10)

legend("topright", pch=c(22,NA, 22, NA), lty=c(NA,1,NA,1), lwd=c(NA,4,NA,4), cex=1, 
       col = c(alpha("green", 0.25),"darkgreen",alpha("red", 0.25),"red"), 
       legend=c(expression(1-alpha==~"95% of"~F['9,120']),"Non-Rejection Region",
                expression(alpha==~"5% of"~F['9,120']),"Rejection Region"), 
       bty="n", pt.bg = c(alpha("green", 0.25),alpha("green", 0.25),alpha("red", 0.25),alpha("red", 0.25)))
curve(expr = df(x = x, df1 = 9, df2 = 120), lwd = 2, col = 1, add=TRUE, from = 0, to = 4)
lines(x=c(q,q), y=c(0,.6),lwd=2,lty=2)
text(x = q, y = .65, labels = expression(c[1-alpha]==1.9588))


## ---------------------------
df1   <- 9    # numerator df
df2   <- 120  # denominator df
alpha <- 0.05 # significance level
## Critical value:
crit_value <- qf(p = 1-alpha, df1 = df1, df2 = df2)
crit_value


## ---------------------------
alpha <- 0.01
## Critical value:
crit_value <- qf(p = 1-alpha, df1 = df1, df2 = df2)
crit_value


## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
library("scales")
curve(expr = dt(x = x, df = 12),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(-5, 5),
      ylim = c(0, .6), xaxs="i",yaxs="i")

alpha <- 0.05/2
q      <- qt(p = 1-alpha, df=12)
xx1    <- seq(-5,-q,len=25)
yy1    <- dt(x = xx1, df = 12)
xx2    <- seq(q,5,len=25)
yy2    <- dt(x = xx2, df = 12)
xx3    <- seq(-q,q,len=25)
yy3    <- dt(x = xx3, df = 12)

polygon(x = c(xx1,rev(xx1)), y=c(yy1, rep(0,length(yy1))), border = NA, col = alpha("red", 0.25))

polygon(x = c(xx2,rev(xx2)), y=c(yy2, rep(0,length(yy2))), border = NA, col = alpha("red", 0.25))

polygon(x = c(xx3,rev(xx3)), y=c(yy3, rep(0,length(yy3))), border = NA, col = alpha("green", 0.25))

legend("topright", pch=c(22,22), lty=c(NA,NA), lwd=c(NA,NA), cex=1, 
       col = c(alpha("green", 0.25),alpha("red", 0.25)), 
       legend=c(expression("95% of"~t['12']),
                expression("5% of"~t['12'])), 
       bty="n", pt.bg = c(alpha("green", 0.25),alpha("red", 0.25)))
curve(expr = dt(x = x, df= 12), lwd = 2, col = 1, add=TRUE)
lines(x=c(q,q), y=c(0,.35),lwd=2,lty=2)
lines(x=c(-q,-q), y=c(0,.35),lwd=2,lty=2)
text(x =  q, y = .45, labels = expression(c[1-alpha/2]==2.18))
text(x = -q, y = .45, labels = expression(c[alpha/2]==-2.18))


## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
library("scales")
alpha <- 0.05
q      <- qt(p = 1-alpha, df=12)

xx1    <- seq(-5,-q,len=25)
yy1    <- dt(x = xx1, df = 12)
xx2    <- seq(-q,5,len=25)
yy2    <- dt(x = xx2, df = 12)
##
xx3    <- seq(q,5,len=25)
yy3    <- dt(x = xx3, df = 12)
xx4    <- seq(-5,q,len=25)
yy4    <- dt(x = xx4, df = 12)

par(mfrow=c(1,2))
curve(expr = dt(x = x, df = 12),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(-5, 5),
      ylim = c(0, .6), xaxs="i",yaxs="i")
polygon(x = c(xx1,rev(xx1)), y=c(yy1, rep(0,length(yy1))), border = NA, col = alpha("red", 0.25))
polygon(x = c(xx2,rev(xx2)), y=c(yy2, rep(0,length(yy2))), border = NA, col = alpha("green", 0.25))
lines(x=c(-q,-q), y=c(0,.35),lwd=2,lty=2)
text(x = -q, y = .45, labels = expression(-c[1-alpha]==-1.78))
legend("topright", pch=c(22,22), lty=c(NA,NA), lwd=c(NA,NA), cex=1, 
       col = c(alpha("green", 0.25),alpha("red", 0.25)), 
       legend=c(expression("95% of"~t['12']),
                expression("5% of"~t['12'])), 
       bty="n", pt.bg = c(alpha("green", 0.25),alpha("red", 0.25)))

###########

curve(expr = dt(x = x, df = 12),
      xlab = "", ylab = "", main = "",
      lwd = 2, col = 1, xlim = c(-5, 5),
      ylim = c(0, .6), xaxs="i",yaxs="i")
polygon(x = c(xx3,rev(xx3)), y=c(yy3, rep(0,length(yy3))), border = NA, col = alpha("red", 0.25))
polygon(x = c(xx4,rev(xx4)), y=c(yy4, rep(0,length(yy4))), border = NA, col = alpha("green", 0.25))
lines(x=c(q,q), y=c(0,.35),lwd=2,lty=2)
text(x =  q, y = .45, labels = expression(c[1-alpha]==1.78))
legend("topright", pch=c(22,22), lty=c(NA,NA), lwd=c(NA,NA), cex=1, 
       col = c(alpha("green", 0.25),alpha("red", 0.25)), 
       legend=c(expression("95% of"~t['12']),
                expression("5% of"~t['12'])), 
       bty="n", pt.bg = c(alpha("green", 0.25),alpha("red", 0.25)))
par(mfrow=c(1,1))


## ---------------------------
df    <- 16   # degrees of freedom 
alpha <- 0.05 # significance level
## One-sided critical value (= (1-alpha) quantile):
c_oneSided <- qt(p = 1-alpha, df = df)
c_oneSided
## Two-sided critical value (= (1-alpha/2) quantile):
c_twoSided <- qt(p = 1-alpha/2, df = df)
## lower critical value
-c_twoSided
## upper critical value
c_twoSided


## ---- fig.align="center", echo=FALSE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
library("scales") # transperent color
mean.alt <- 2

x  <- seq(-4, 4, length=1000)
hx <- dnorm(x)
alpha <- 0.05

plot(x, hx, type="n", xlim=c(-4, 7), ylim=c(0, 0.65), ylab = "", xlab = "", axes=T)
#axis(1)

xfit2 <- x + mean.alt
yfit2 <- dnorm(x)

## Print null hypothesis area
polygon(c(min(x), x,  max(x)), 
        c(0,      hx, 0), 
        col   =alpha("grey", 0.5), 
        border=alpha("grey", 0.9))

ub <- max(x)
lb <- round(qnorm(1-alpha),2)

## The green area: Power
i <- xfit2 >= lb
polygon(c(min(xfit2[i]), xfit2[i], max(xfit2[i])), 
        c(0,  yfit2[i], 0), 
        col=alpha("green", 0.25),
        border=alpha("green", 0.25))

## The blue area: P(Type II error)
lb <- min(xfit2)
ub <- round(qnorm(1-alpha),2)

i <- xfit2 >= lb & xfit2 <= ub
polygon(c(lb,xfit2[i],ub), c(0,yfit2[i],0), col=alpha("darkblue", 0.25), border=alpha("darkblue", 0.25))

lines(x=c(ub,ub), y=c(0,.47),lwd=2,lty=2)
text(x = ub, y = .57, labels = expression(c[1-alpha]==1.64))

text(x=0+.25,y=.425, "N(0,1)", pos=2)
text(x=2+.5,y=.425, "N(2,1)", pos=4)
legend(x=-4.5,y=.65, title=NULL, bty="n", 
   c(expression("Null Distribution"~"N(0,1)"),"P(Type II Error)","P(Type I Error)", expression(paste("Power")))[-3], 
    fill=c(alpha("grey", 0.5), alpha("darkblue", 0.25), alpha("red", 0.25), alpha("green", 0.5))[-3], horiz=FALSE)


## ---------------------------
## Function to generate artificial data
## If X=NULL: new X variables are generated
## If the user gives X variables, 
## the sampling of new Y variables is conditionally on 
## the given X variables.
myDataGenerator <- function(n, beta, X=NULL, sigma=3){
  if(is.null(X)){
    X   <- cbind(rep(1, n), 
                 runif(n, 2, 10), 
                 runif(n,12, 22))
  }
  eps  <- rnorm(n, sd=sigma)
  Y    <- X %*% beta + eps
  data <- data.frame("Y"=Y, 
                     "X_1"=X[,1], "X_2"=X[,2], "X_3"=X[,3])
  ##
  return(data)
}

## Define a true beta vector
beta_true <- c(2,3,4)

## Check:
## Generate Y and X data 
test_data     <- myDataGenerator(n = 10, beta=beta_true)
## Generate new Y data conditionally on X
X_cond <- cbind(test_data$X_1,
                test_data$X_2,
                test_data$X_3)
test_data_new <- myDataGenerator(n    = 10, 
                                 beta = beta_true, 
                                 X    = X_cond)
## compare
round(head(test_data,     3), 2) # New Y, new X
round(head(test_data_new, 3), 2) # New Y, conditionally on X


## ---- fig.align="center", echo=TRUE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
set.seed(123)
n           <- 10       # a small sample size
beta_true   <- c(2,3,4) # true data vector
sigma       <- 3        # true var of the error term

## Let's generate a data set from our data generating process
mydata  <- myDataGenerator(n = n, beta=beta_true)
X_cond  <- cbind(mydata$X_1, mydata$X_2, mydata$X_3)

## True mean and variance of the true normal distribution 
## of beta_hat_2|X:
# true mean
beta_true_2     <- beta_true[2] 
# true variance
var_true_beta_2 <- sigma^2 * diag(solve(t(X_cond) %*% X_cond))[2]

## Let's generate 5000 realizations from beta_hat_2 
## conditionally on X and check whether the empirical  
## distribution of these 5000 realizations is close 
## to the true normal distribution of beta_hat_2:
rep        <- 5000 # MC replications
beta_hat_2 <- rep(NA, times=rep)
##
for(r in 1:rep){
    MC_data <- myDataGenerator(n    = n, 
                               beta = beta_true, 
                               X    = X_cond)
    lm_obj        <- lm(Y ~ X_2 + X_3, data = MC_data)
    beta_hat_2[r] <- coef(lm_obj)[2]
}

## Compare
## True beta_2 versus average of beta_hat_2 estimates
beta_true_2
round(mean(beta_hat_2), 4)
## True variance of beta_hat_2 versus 
## empirical variance of beta_hat_2 estimates
round(var_true_beta_2, 4)
round(var(beta_hat_2), 4)

## True normal distribution of beta_hat_2 versus 
## empirical density of beta_hat_2 estimates
library("scales")
curve(expr = dnorm(x, mean = beta_true_2, 
                   sd=sqrt(var_true_beta_2)), 
      xlab="",ylab="", col=gray(.2), lwd=3, lty=1, 
      xlim=range(beta_hat_2), ylim=c(0,1.1))
hist(beta_hat_2, freq=FALSE, col=alpha("blue",.35), add=TRUE)
lines(density(beta_hat_2, bw = bw.SJ(beta_hat_2)), 
      col=alpha("blue",.5), lwd=3)
legend("topleft", lty=c(1,NA,1), lwd=c(3,NA,3), pch=c(NA,15,NA), pt.cex=c(NA,2,NA),
     col=c(gray(.2), alpha("blue",.45), alpha("blue",.5)), bty="n", legend= 
c(expression(
  "Theoretical Gaussian Density of"~hat(beta)[2]~'|'~X), 
  expression(
  "Histogram based on the 5000 MC realizations of"~
  hat(beta)[2]~'|'~X), 
  expression(
  "Nonparametric Density Estimation based on the 5000 MC realizations of"~
  hat(beta)[2]~'|'~X)))


## ---- fig.align="center", echo=TRUE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
set.seed(123)
## Let's generate 5000 realizations from beta_hat_2 
## WITHOUT conditioning on X
beta_hat_2_uncond <- rep(NA, times=rep)
##
for(r in 1:rep){
    MC_data <- myDataGenerator(n    = n, 
                               beta = beta_true)
    lm_obj               <- lm(Y ~ X_2 + X_3, data = MC_data)
    beta_hat_2_uncond[r] <- coef(lm_obj)[2]
}

## Compare
## True beta_2 versus average of beta_hat_2 estimates
beta_true_2
round(mean(beta_hat_2_uncond), 4)
## True variance of beta_hat_2 versus 
## empirical variance of beta_hat_2 estimates
round(var_true_beta_2, 4)
round(var(beta_hat_2_uncond), 4)

## Plot
curve(expr = dnorm(x, mean = beta_true_2, 
                   sd=sqrt(var_true_beta_2)), 
      xlab="",ylab="", col=gray(.2), lwd=3, lty=1, 
      xlim=range(beta_hat_2_uncond), ylim=c(0,1.5))
hist(beta_hat_2_uncond, freq=FALSE, col=alpha("blue",.35), add=TRUE)
lines(density(beta_hat_2_uncond, bw=bw.SJ(beta_hat_2_uncond)), 
      col=alpha("blue",.5), lwd=3)
legend("topleft", lty=c(1,NA,1), lwd=c(3,NA,3), pch=c(NA,15,NA), pt.cex=c(NA,2,NA),
     col=c(gray(.2), alpha("blue",.45), alpha("blue",.5)), bty="n", legend= 
c(expression(
  "Theoretical Gaussian Density of"~hat(beta)[2]~'|'~X), 
expression(
  "Histogram based on the 5000 MC realizations of"~
  hat(beta)[2]), 
expression("Nonparam. Density Estimation based on the 5000 MC realizations of"~
  hat(beta)[2])))


## ---------------------------
suppressMessages(library("car")) # for linearHyothesis()
# ?linearHypothesis

## Estimate the linear regression model parameters
lm_obj <- lm(Y ~ X_2 + X_3, data = mydata)

## Option 1:
car::linearHypothesis(model = lm_obj, 
                      hypothesis.matrix = c("X_2=3", "X_3=4"))

## Option 2:
R <- rbind(c(0,1,0),
           c(0,0,1))
car::linearHypothesis(model = lm_obj, 
                      hypothesis.matrix = R, 
                      rhs = c(3,4))


## ---------------------------
## Let's generate 5000 F-test decisions and check 
## whether the empirical rate of type I errors is 
## close to the theoretical significance level. 
rep             <- 5000 # MC replications
F_test_pvalues  <- rep(NA, times=rep)
##
for(r in 1:rep){
  ## generate new MC_data conditionally on X_cond
    MC_data <- myDataGenerator(n    = n, 
                               beta = beta_true, 
                               X    = NULL)
                               X    = X_cond)
    lm_obj            <- lm(Y ~ X_2 + X_3, data = MC_data)
    ## save the p-value
    p <- linearHypothesis(lm_obj, 
                          c("X_2=3", "X_3=4"))$`Pr(>F)`[2]
    F_test_pvalues[r] <- p
}
##
signif_level <-  0.05
rejections   <- F_test_pvalues[F_test_pvalues < signif_level]
round(length(rejections)/rep, 3)
## 
signif_level <-  0.01
rejections   <- F_test_pvalues[F_test_pvalues < signif_level]
round(length(rejections)/rep, 3)


## ---------------------------
set.seed(321)
rep             <- 5000 # MC replications
F_test_pvalues  <- rep(NA, times=rep)
##
for(r in 1:rep){
  ## generate new MC_data conditionally on X_cond
    MC_data <- myDataGenerator(n    = n, 
                               beta = beta_true, 
                               X    = X_cond)
    lm_obj            <- lm(Y ~ X_2 + X_3, data = MC_data)
    ## save p-values of all rep-many tests
    F_test_pvalues[r] <- linearHypothesis(lm_obj,
                         c("X_2=4","X_3=4"))$`Pr(>F)`[2]
}
##
signif_level <-  0.05
rejections   <- F_test_pvalues[F_test_pvalues < signif_level]
length(rejections)/rep


## ---------------------------
car::linearHypothesis(lm_obj, c("X_2=4", "X_3=4"))


## ---------------------------
signif_level <- 0.05
## 95% CI for beta_2
confint(lm_obj, parm = "X_2", level = 1 - signif_level)
## 95% CI for beta_3 
confint(lm_obj, parm = "X_3", level = 1 - signif_level)


## ---- fig.align="center", echo=TRUE, fig.width = 8, fig.height = 5, out.width = "1\\textwidth"----
## Let's generate 1000 CIs 
set.seed(123)
signif_level <-  0.05
rep          <- 5000 # MC replications
confint_m    <- matrix(NA, nrow=2, ncol=rep)
##
for(r in 1:rep){
  ## generate new MC_data conditionally on X_cond
    MC_data <- myDataGenerator(n    = n, 
                               beta = beta_true, 
                               #X    = NULL)
                               X = X_cond)
    lm_obj            <- lm(Y ~ X_2 + X_3, data = MC_data)
    ## save the p-value
    CI <- confint(lm_obj, parm="X_2", level=1-signif_level)
    confint_m[,r] <- CI
}
##
inside_CI  <- confint_m[1,] <= beta_true_2 & 
                beta_true_2 <= confint_m[2,]

## CI-lower, CI-upper, beta_true_2 inside?
head(cbind(t(confint_m), inside_CI))

round(length(inside_CI[inside_CI == FALSE])/rep, 4)

nCIs <- 100
plot(x=0,y=0,type="n",xlim=c(0,nCIs),ylim=range(confint_m[,1:nCIs]),
     ylab="", xlab="Resamplings", main="Confidence Intervals")
for(r in 1:nCIs){
  if(inside_CI[r]==TRUE){
      lines(x=c(r,r), y=c(confint_m[1,r], confint_m[2,r]), 
            lwd=2, col=gray(.5,.5))
  }else{
      lines(x=c(r,r), y=c(confint_m[1,r], confint_m[2,r]), 
            lwd=2, col="darkred")
    }
}
axis(4, at=beta_true_2, labels = expression(beta[2]))
abline(h=beta_true_2)

