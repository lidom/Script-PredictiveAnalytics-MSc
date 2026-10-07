## ---------------------------------------------------------------------------------------
2+2 # and all the others: *,/,-,^2,^3,... 


## ---------------------------------------------------------------------------------------
x <- 4 
x
4 -> x # possible but unusual
x


## ---------------------------------------------------------------------------------------
y <- c(2,7,4,1)
y


## ---------------------------------------------------------------------------------------
ls()[1:5] # only the first 5 elements
rm(list=ls())
ls()



## ---------------------------------------------------------------------------------------
x <- 4
y <- c(2,7,4,1)

x*y # each element in y (vector) is multiplied by x (scalar).
y*y # this is a term by term product of the elements in y



## ---------------------------------------------------------------------------------------
y %*% t(y)


## ---------------------------------------------------------------------------------------
t(y) %*% y


## ---------------------------------------------------------------------------------------
y^2
log(y)
exp(y)
y-mean(y)
(y-mean(y))/sd(y) # standardization 


## ---- eval=FALSE------------------------------------------------------------------------
## N <- length(y)
## 1:N
## 
## y.sq <- numeric(N)
## y.sq
## 
## for(i in 1:N){
##   y.sq[i] <- y[i]^2
##   if(i == N){
##     print(y.sq)
##   }
## }


## ---- eval=FALSE------------------------------------------------------------------------
## 1:10
## -10:10
## ?seq # Help for the seq()-function
## seq(from=1, to=100, by=7)


## ---------------------------------------------------------------------------------------
?matrix
A <- matrix(data=1:16, nrow=4, ncol=4)
A
A <- matrix(1:16, 4, 4)


## ---------------------------------------------------------------------------------------
dim(A)    # Dimension of matrix A?
dim(y)    # dim() does not operate on vectors.
length(y) # Length of vector y?


## ---- eval=FALSE------------------------------------------------------------------------
## A[,1]
## A[4,4]
## y[c(1,4)]


## ---------------------------------------------------------------------------------------
A[,1][A[,1]>2]

# Note that this give you a boolean vector:
A[,1]>2

# And you can use it in a non-sense relation, too:
y[A[,1]>2]



## ---------------------------------------------------------------------------------------
myFirst.Array <- array(c(1:8), dim=c(2,2,2)) # Take a look at it!


## ---------------------------------------------------------------------------------------
myFirst.List <- list(
  "Some_Numbers" = c(66, 76, 55, 12, 4, 66, 8, 99), 
  "Animals"      = c("Rabbit", "Cat", "Elefant"),
  "My_Series"    = c(30:1)
) 


## ---------------------------------------------------------------------------------------
str(myFirst.List)


## ---------------------------------------------------------------------------------------
myFirst.Dataframe <- data.frame(
  "Credit_Default"   = c( 0, 0, 1, 0, 1, 1), 
  "Age"              = c(35,41,55,36,44,26), 
  "Loan_in_1000_EUR" = c(55,65,23,12,98,76)
) 
# Take a look at it!


## ---- eval=FALSE------------------------------------------------------------------------
## # ATTENTION! YOU HAVE TO CHANGE "\" TO "/":
## auto.data <- read.csv(file   = "C:/your_path/autodata.csv",
##                       header = TRUE)
## head(auto.data)


## ---------------------------------------------------------------------------------------
# install.packages("readr")
library("readr")
auto.data <- suppressMessages(
  read_csv(
  file = "https://cdn.rawgit.com/lidom/Teaching_Repo/bc692b56/autodata.csv",
  col_names = TRUE)
)
# head(auto.data)


## ---------------------------------------------------------------------------------------
gasolin.consumption      <- auto.data$MPG.city
car.weight               <- auto.data$Weight
## Take a look at the first elements of these vectors:
head(cbind(gasolin.consumption,car.weight))


## ----fig-margin, fig.margin = TRUE,fig.width=4.5, fig.height=3.5, cache=TRUE------------
## Plot the data:
plot(y=gasolin.consumption, x=car.weight, 
     xlab="Car-Weight (US-Pounds)", 
     ylab="Consumption (Miles/Gallon)", 
     main="Buy Light-Weight Cars!")


## ---------------------------------------------------------------------------------------
lm.result   <- lm(gasolin.consumption~car.weight)
lm.summary  <- summary(lm.result)
lm.summary


## ----fig.width=10, fig.height=5, out.width='\\textwidth', fig.align='center'------------
## Accessing the computed quantities
names(lm.summary) ## Alternatively: str(lm.summary)

alpha <- lm.summary$coefficients[1]
beta  <- lm.summary$coefficients[2]

## Plot all:
plot(y=gasolin.consumption, x=car.weight, 
     xlab="Car-Weight (US-Pounds)", 
     ylab="Consumption (Miles/Gallon)", 
     main="Buy light-weight Cars!")
abline(a=alpha, 
       b=beta, col="red")


## ---------------------------------------------------------------------------------------
set.seed(109) # Sets the "seed" of the random number generators:
n   <- 50     # Number of observations

## Generate two explanatory variables plus an intercept-variable:
X.1 <- rep(1, n)                 # Intercept
X.2 <- rnorm(n, mean=10, sd=1.5) # Draw realizations form a normal distr.
X.3 <- rt(n, df=5, ncp=2)        # Draw realizations form a t-distr.
X   <- cbind(X.1, X.2, X.3)      # Save as a Nx3-dimensional data matrix.


## ---------------------------------------------------------------------------------------
## Define the slope-coefficients
beta.vec  <- c(1,-5,5)


## ---------------------------------------------------------------------------------------
## Generate realizations from the heteroscadastic error term
eps       <- abs(X.3) * rnorm(n, mean=0, sd=1)


## ---- fig.margin = TRUE, fig.width=4.5, fig.height=3.5, cache=TRUE----------------------
plot(y=eps, x=X.3, 
     main="Realizations of the \nHeteroscedastic Error Term")


## ---------------------------------------------------------------------------------------
## Dependent variable:
y   <- X %*% beta.vec + eps


## ----fig.width=10, fig.height=5, out.width='\\textwidth', fig.align='center'------------
mydata    <- data.frame("Y"=y, "X.1"=X.1, "X.2"=X.2, "X.3"=X.3)
pairs(mydata[,-2]) # The '-2' removes the intercept variable "X.1"


## ---------------------------------------------------------------------------------------
## Computation of the beta-Vector:
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat


## ---------------------------------------------------------------------------------------
myOLSFun <- function(y, x, add.intercept=FALSE){
  
  ## Number of Observations:
  n         <- length(y)
  
  ## Add an intercept to x:
  if(add.intercept){
    Intercept <- rep(1, n)
    x         <- cbind(Intercept, x)
  }
  
  ## Estimation of the slope-parameters:
  beta.hat.vec <- solve(t(x) %*% x) %*% t(x) %*% y
  
  ## Return the result:
  return(beta.hat.vec)
}

## Run the function:
myOLSFun(y=y, x=X)


## ---- fig.width=10, fig.height=5, out.width='\\textwidth', fig.align='center'-----------
# install.packages("ggplot2")
library("ggplot2")

qplot(Sepal.Length, Petal.Length, data = iris, color = Species)


## ----installTidyverse, echo=TRUE, eval=FALSE--------------------------------------------
## install.packages("tidyverse")


## ----usePackage-------------------------------------------------------------------------
library(tidyverse)


## ----helpCWdata, echo=TRUE, eval=FALSE--------------------------------------------------
## help("ChickWeight")


## ----writeCW----------------------------------------------------------------------------
ChickWeight %>%
  select(Chick, Diet, Time, weight) %>% 
  arrange(Chick, Diet, Time) %>% 
  write_csv("ChickWeight.csv")


## ----readCW-----------------------------------------------------------------------------
CW <- read_csv("ChickWeight.csv")


## ----printCW----------------------------------------------------------------------------
CW


## ----glimpseCW--------------------------------------------------------------------------
glimpse(CW)


## ----ViewCW, eval=FALSE-----------------------------------------------------------------
## View(CW)


## ----emptyPlot, fig.width=1.74, fig.height=1.74, fig.show='hold', fig.align='center'----
# An empty plot (the plot on the left)
ggplot(CW, aes(Time, weight))  
# With data (the plot on the right)
ggplot(CW, aes(Time, weight)) + geom_point() 


## ----addColourPlot, fig.height=2.0------------------------------------------------------
# Adding colour for diet
ggplot(CW,aes(Time,weight,colour=factor(Diet))) +
  geom_point() 


## ----makeFactor-------------------------------------------------------------------------
CW <- mutate(CW, Diet = factor(Diet))
CW <- mutate(CW, Time = factor(Time))
glimpse(CW)


## ----ScatterPlot------------------------------------------------------------------------
# Adding jitter to the points
ggplot(CW, aes(Time, weight, colour=Diet)) +
  geom_point() +
  facet_wrap(~Diet) +
  theme(legend.position = "bottom")


## ----meanlinesPlot, fig.height=2.0------------------------------------------------------
ggplot(CW, aes(Time, weight, 
               group=Diet, colour=Diet)) +
  stat_summary(fun="mean", geom="line") 


## ----boxPlot----------------------------------------------------------------------------
ggplot(CW, aes(Time, weight, colour=Diet)) +
  facet_wrap(~Diet) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ggtitle("Chick Weight over Time by Diet")


## ----finalPlot--------------------------------------------------------------------------
ggplot(CW, aes(Time, weight, group=Diet, 
                             colour=Diet)) +
  facet_wrap(~Diet) +
  geom_point() +
  # geom_jitter() +
  stat_summary(fun="mean", geom="line",
               colour="black") +
  theme(legend.position = "none") +
  ggtitle("Chick Weight over Time by Diet") + 
  xlab("Time (days)") +
  ylab("Weight (grams)")


## ----resettblprint, echo = FALSE--------------------------------------------------------
# Temporarily resetting the print limit
options(tibble.print_min=3, tibble.print_max=3)


## ----mutateDemo-------------------------------------------------------------------------
# Added a column
CWm1 <- mutate(CW, weightKg = weight/1000)
CWm1
# Modify an existing column
CWm2 <- mutate(CW, Diet = str_c("Diet ", Diet))
CWm2


## ----selectDemo-------------------------------------------------------------------------
# Drop the weight variable from CWm1 using minus
select(CWm1, -weight)
# Keep variables Time, Diet and weightKg
select(CWm1, Chick, Time, Diet, weightKg)


## ----renameDemo-------------------------------------------------------------------------
rename(CW, Group = Diet, Weight = weight)


## ----filterDemo-------------------------------------------------------------------------
filter(CW, Time==21 & weight>300)


## ----arrangeDemo------------------------------------------------------------------------
arrange(CW, Chick, Time)
arrange(CW, desc(weight))


## ----pipeOpDemo-------------------------------------------------------------------------
CW21 <- CW %>% 
  filter(Time %in% c(0, 21)) %>% 
  rename(Weight = weight) %>% 
  mutate(Group = factor(str_c("Diet ", Diet))) %>% 
  select(Chick, Group, Time, Weight) %>% 
  arrange(Chick, Time) 
CW21


## ----pipeOpExplain, echo=TRUE, eval=FALSE-----------------------------------------------
## CW21 <- CW %>%
##   filter(., Time %in% c(0, 21)) %>%
##   rename(., Weight = weight) %>%
##   mutate(., Group=factor(str_c("Diet ",Diet))) %>%
##   select(., Chick, Group, Time, Weight) %>%
##   arrange(., Chick, Time)


## ----mnsdStatCW-------------------------------------------------------------------------
mnsdCW <- CW %>% 
  group_by(Diet, Time) %>% 
  summarise(N = n(), Mean = mean(weight)) %>% 
  arrange(Diet, Time)
mnsdCW


## ---------------------------------------------------------------------------------------
sumCW <-  CW %>% 
  filter(Time %in% c(0, 21)) %>% 
  group_by(Diet, Time) %>% 
  summarise(N = n(),
            Mean = mean(weight),
            SD = sd(weight),
            Median = median(weight),
            Min = min(weight),
            Max = max(weight)) %>% 
  arrange(Diet, Time)
sumCW


## ----prettySum, echo=TRUE, eval=FALSE---------------------------------------------------
## library("knitr") # to use the kable() function
## prettySumCW <- sumCW %>%
##  mutate(`Mean (SD)` = str_c(format(Mean, digits=1),
##            " (", format(SD, digits=2), ")")) %>%
##  mutate(Range = str_c(Min, " - ", Max)) %>%
##  select(Diet, Time, N, `Mean (SD)`, Median, Range) %>%
##  arrange(Diet, Time) %>%
##  kable(format = "latex")
## prettySumCW


## ----prettySum2, echo=FALSE-------------------------------------------------------------
library("knitr") # to use the kable() function
if( knitr:::is_latex_output() ) {
    prettySumCW <- sumCW %>% 
 mutate(`Mean (SD)` = str_c(format(Mean, digits=1),
           " (", format(SD, digits=2), ")")) %>% 
 mutate(Range = str_c(Min, " - ", Max)) %>% 
 select(Diet, Time, N, `Mean (SD)`, Median, Range) %>%
 arrange(Diet, Time) %>% 
 kable(format = "latex")
prettySumCW
} else {
    prettySumCW <- sumCW %>% 
 mutate(`Mean (SD)` = str_c(format(Mean, digits=1),
           " (", format(SD, digits=2), ")")) %>% 
 mutate(Range = str_c(Min, " - ", Max)) %>% 
 select(Diet, Time, N, `Mean (SD)`, Median, Range) %>%
 arrange(Diet, Time) %>% 
 kable(format = "html")
prettySumCW
}


## ----tblprintdef, echo = FALSE----------------------------------------------------------
# Resetting the print limit back to the defaults
options(tibble.print_min=10, tibble.print_max=20)

