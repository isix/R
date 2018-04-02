#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Plotting Survival Curves
#==============================================================================
# Title          : Local Regression MP LOESS.r
# Description    : Local Regression - Fit a line with LOESS in R
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180304
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : Based on 
# https://stackoverflow.com/questions/15337777/fit-a-line-with-loess-in-r
# http://ggplot2.tidyverse.org/reference/geom_smooth.html
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
plot(y ~ x, data=data,pch=19,cex=0.1)
j <- order(data$x)
lines(data$x[j],lw1$fitted[j],col="red",lwd=3)

#------------------------------------------------------------------------------
# ggplot2
#------------------------------------------------------------------------------
# http://ggplot2.tidyverse.org/reference/geom_smooth.html
load(url("https://www.dropbox.com/s/ud32tbptyvjsnp4/data.R?dl=1"))
ggplot(data, aes(x, y)) + 
geom_point() +
geom_smooth(method = "loess", se = FALSE)

ggplot(data, aes(x, y)) + 
geom_point() +
geom_smooth(method = "loess", se = TRUE)


