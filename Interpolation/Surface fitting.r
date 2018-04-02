#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Surface fitting
#==============================================================================
# Title          : Plotting Survival Curves.r
# Description    : Vector fitting is popular, and it provides a compact way 
#                  of simultaneously displaying a large number of environmental 
#                  variables. However, it implies a linear relationship between
#                  ordination and environment: direction and strength are all 
#                  you need to know. 
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20170324
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : Original on Vegan package documentation.
# R version      : 3.4
#==============================================================================

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(vegan)

#==============================================================================
# Surfacing
#==============================================================================
data(varespec)
data(varechem)
vare.dist <- vegdist(varespec)
vare.mds <- monoMDS(vare.dist)
ordisurf(vare.mds ~ Baresoil, varechem, bubble = 5)

ef <- envfit(vare.mds, varechem, permu = 999)
ef
ef <- envfit(vare.mds ~ Al + Ca, varechem)
plot(vare.mds, display = "sites")
plot(ef)
tmp <- with(varechem, ordisurf(vare.mds, Al, add = TRUE))
with(varechem, ordisurf(vare.mds, Ca, add = TRUE, col = "green4"))

