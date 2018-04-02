#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Local Regression NonParametric LOESS
#==============================================================================
# Title          : Local Regression NP LOESS.r
# Description    : Local Regression - Fit a line with LOESS in R
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180304
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : Based on 
# https://stackoverflow.com/questions/15337777/fit-a-line-with-loess-in-r
# http://ggplot2.tidyverse.org/reference/geom_smooth.html
# r-statistics.co by Selva Prabhakaran
# R version      : 3.4
#==============================================================================
# install.packages('ggplot2')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)

# Loess short for Local Regression is a non-parametric approach that fits 
# multiple regressions in local neighborhood. This can be particularly 
# resourceful, if you know that your X variables are bound within a range.

#------------------------------------------------------------------------------
# Regular
#------------------------------------------------------------------------------
load(url('https://www.dropbox.com/s/ud32tbptyvjsnp4/data.R?dl=1'))
lw1 <- loess(y ~ x,data=data)
plot(y ~ x, data=data,pch=19,cex=0.1, main="Loess Smoothing and Prediction")
j <- order(data$x)
lines(data$x[j],lw1$fitted[j],col="red",lwd=3)

#------------------------------------------------------------------------------
# Scatter
#------------------------------------------------------------------------------
scatter.smooth(data$y ~ data$x, span = 2/3, degree = 2, main="Loess Smoothing and Prediction")

#------------------------------------------------------------------------------
# ggplot2
#------------------------------------------------------------------------------
# http://ggplot2.tidyverse.org/reference/geom_smooth.html
load(url("https://www.dropbox.com/s/ud32tbptyvjsnp4/data.R?dl=1"))
ggplot(data, aes(x, y), main="Loess Smoothing and Prediction") + 
geom_point() +
geom_smooth(method = "loess", se = FALSE)

ggplot(data, aes(x, y)) + 
geom_point() +
geom_smooth(method = "loess", se = TRUE)

# Second Example

data(economics, package="ggplot2")  # load data
economics$index <- 1:nrow(economics)  # create index variable
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span
# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

#------------------------------------------------------------------------------
# Finding the optimal smoothing span
#------------------------------------------------------------------------------
# As the smoothing span changes, the accuracy of the fitted curve also changes. 
# If your intent is to minimize the error, the optim() can be used to find that 
# value of span, that minimizes the Sum of Squared Errors (SSE). For this case, 
# it is graphically intuitive that lower SSE will likely be achieved at lower 
# values of span, but for more challenging cases, optimizing span could help.
# To implement optim(), we define the function that computes the SSE. An error 
# handling mechanism is needed to address very low values of span and cases where 
# the non-numerics are produced. The simulated annealing method (SANN) is 
# implemented here to find the span that gives minimal SSE. The par argument 
# specifies the first value of the span at which optim() will begin the search.

# define function that returns the SSE
calcSSE <- function(x){
  loessMod <- try(loess(uempmed ~ index, data=economics, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Run optim to find span that gives min SSE, starting at 0.5
optim(par=c(0.5), calcSSE, method="SANN")

# EOF