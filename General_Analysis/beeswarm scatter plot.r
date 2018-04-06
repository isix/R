#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# SCATTER PLOT
#==============================================================================
# Title          : beeswarm scatter plot.r
# Description    : beeswarm scatter plotting using {}
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 201800404
# Version        : 0.0.3
# Usage          : Run in R 3.4
# Notes          : Source http://www.cbs.dtu.dk/~eklund/beeswarm
# R version      : 3.4
#==============================================================================

# install.packages('beeswarm')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(beeswarm)

#==============================================================================
# Plots
#==============================================================================
stripchart(decrease ~ treatment, data = OrchardSprays, 
  vertical = TRUE, log = "y", method = 'jitter', jitter = 0.2, cex = 1,
  pch = 16, col = rainbow(8),
  main = 'stripchart')
  
beeswarm(decrease ~ treatment, data = OrchardSprays, 
  log = TRUE, pch = 16, col = rainbow(8),
  main = 'beeswarm')
  
  
# Specify color of individual points
data(breast)
beeswarm(time_survival ~ ER, data = breast,
    pch = 16, pwcol = 1 + as.numeric(event_survival),
    xlab = "", ylab = "Follow-up time (months)",
    labels = c("ER neg", "ER pos"))
legend("topright", legend = c("Yes", "No"),
    title = "Censored", pch = 16, col = 1:2)
	
# methods for arranging points
set.seed(666)
n <- 200
distro <- list(runif = runif(n, min = -3, max = 3), 
               rnorm = rnorm(n))
			   
for (m in c("swarm", "center", "hex", "square")) {
  beeswarm(distro, 
    col = 2:3, pch = 16,
    method = m, 
    main = paste('method = "', m, '"', sep = ''))
}

# methods for arranging points (horizontal axis), "corral" methods
for (ii in c("none", "gutter", "wrap", "random", "omit")) {
  beeswarm(distributions, 
    pch = 21, col = 2:4, bg = "#00000050",
    corral = ii, 
    main = paste('corral = "', ii, '"', sep = ''))
}

# combine beeswarms with "boxplot" or "bxplot"
boxplot(len ~ dose, data = ToothGrowth, 
  outline = FALSE,     ## avoid double-plotting outliers, if any
  main = 'boxplot + beeswarm')
  
beeswarm(len ~ dose, data = ToothGrowth, 
  col = 4, pch = 16, add = TRUE)
  
beeswarm(len ~ dose, data = ToothGrowth, 
  col = 4, pch = 16,
  main = 'beeswarm + bxplot')
  
bxplot(len ~ dose, data = ToothGrowth, add = TRUE)
  
# arguments "side" and "priority"
beeswarm(distributions, col = 2:4, 
  main = 'Default')
  
beeswarm(distributions, col = 2:4, side = -1, 
  main = 'side = -1')
  
beeswarm(distributions, col = 2:4, side = 1, 
  main = 'side = 1')
  
beeswarm(distributions, col = 2:4, priority = "descending", 
  main = 'priority = "descending"')
  
beeswarm(distributions, col = 2:4, priority = "random", 
  main = 'priority = "random"')  
  
beeswarm(distributions, col = 2:4, priority = "density", 
  main = 'priority = "density"')  
  
  