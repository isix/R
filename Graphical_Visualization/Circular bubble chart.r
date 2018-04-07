#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Circular bubble chart
#==============================================================================
# Title          : Circular bubble chart.r
# Description    : Circular bubble chart plotting.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180407
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
# https://stackoverflow.com/questions/37835023/circular-bubble-chart-with-r
#                  More https://plot.ly/r/bubble-charts/
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
install.packages("packcircles")
install.packages("bubbles")
install.packages("devtools")
devtools::install_github("jcheng5/bubbles")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(packcircles)
library(bubbles)
library(ggplot2)

#------------------------------------------------------------------------------
# Data generation/preparation and pattern
#------------------------------------------------------------------------------
setwd('C:/Users/Isaias Prestes/Documents/Pesquisas/GitHub/R/Graphical_Visualization')
mydata <- read.table(
  "..//Data//Tomatos.dat",
  sep="\t", header=TRUE)

#------------------------------------------------------------------------------
# Visualising circular bubble chart
#------------------------------------------------------------------------------
percfac <- 0.1
xmin <- min(mydata$x) - (min(mydata$x) * percfac)
xmax <- max(mydata$x) + (max(mydata$x) * percfac)
ymin <- min(mydata$y) - (min(mydata$y) * percfac)
ymax <- max(mydata$y) + (max(mydata$y) * percfac)
res <- circleRepelLayout(mydata[,2:4], xlim=c(xmin, xmax), ylim=c(ymin, ymax), maxiter = 1000)
dat <- circlePlotData(res$layout)

doPlot <- function(dat, title) {
	ggplot(dat) + 
	geom_polygon(aes(x, y, group=id), colour="tomato", fill="tomato", alpha=0.3) +
	coord_equal(xlim=c(min(mydata$x),max(mydata$x)), ylim=c(min(mydata$y),max(mydata$y))) +
	theme_bw() +
	theme(axis.text=element_blank(),
		axis.ticks=element_blank(),
		axis.title=element_blank()) +
	labs(title=title)
}

doPlot(mydata, "")

# bubbles ---------------------------------------------------------------------

bubbles(value = runif(26), label = LETTERS,
  color = rainbow(26, alpha=NULL)[sample(26)]
)

text = as.character(seq(from = 1, to = 476))
bubbles(value = count,
        color = rainbow(length(count), alpha=NULL)[sample(length(count))],
        label = text,
        width=1600, height=1600)
		
# EOF