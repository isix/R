#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Plotting Survival Curves
#==============================================================================
# Title          : Plotting Survival Curves.r
# Description    : Survival Curves related plotting using {ggplot2} and 
#                  {ggfortify}.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180304
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : Original http://rpubs.com/sinhrks/plot_surv
# R version      : 3.4
#==============================================================================

# install.packages('ggfortify')
# install.packages('survival')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggfortify)
library(survival)

#==============================================================================
# Cox Models
#==============================================================================
d.coxph <- survfit(coxph(Surv(time, status) ~ sex, data = lung))

windows()
autoplot(d.coxph, surv.linetype = 'dashed', surv.colour = 'blue', 
		conf.int.fill = 'dodgerblue3', conf.int.alpha = 0.5, censor = FALSE)
windows()
autoplot(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung))

#==============================================================================
# Non-Parametric Models
#==============================================================================
fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)

windows()
autoplot(fit, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 2)
		 
windows()
autoplot(survfit(Surv(time, status) ~ 1, data = lung), surv.colour = 'orange', 
		censor.colour = 'red')

autoplot(survfit(Surv(time, status) ~ sex, data = lung), fun = 'event')

# EOF








