#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Wrap a 1d ribbon of panels into 2d
#==============================================================================
# Title          : Plots by class ggplot2.r
# Description    : Wrap a 1d ribbon of panels into 2d.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180425
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
#                  http://ggplot2.tidyverse.org/reference/facet_wrap.html
#                  http://ggplot2.tidyverse.org/reference/mpg.html
#                  https://bit.ly/2r4Kuqm
#                  https://bit.ly/2HOMnBF
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
if(!require("ggplot2")) install.packages("ggplot2")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)

#==============================================================================
# Examples
#==============================================================================

# Example 1
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f") & class != "2seater")

plot1 <- ggplot(mpg2, aes(cty, hwy)) + 
  geom_abline() +
  geom_jitter(width = 0.1, height = 0.1) 

plot2 <- ggplot(mpg2, aes(cty, hwy)) + 
  geom_abline() +
  geom_jitter(width = 0.1, height = 0.1) 
plot2 + facet_wrap(~cyl)

plot3 <- ggplot(mpg2, aes(cty, hwy)) + 
  geom_abline() +
  geom_jitter(width = 0.1, height = 0.1) 
plot3 + facet_wrap(~cyl, scales = "free")

mpg2$model <- reorder(mpg2$model, mpg2$cty)
mpg2$manufacturer <- reorder(mpg2$manufacturer, -mpg2$cty)
ggplot(mpg2, aes(cty, model)) + 
  geom_point() + 
  facet_grid(manufacturer ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))

# Example 2
df <- data.frame(
  x = rnorm(120, c(0, 2, 4)),
  y = rnorm(120, c(1, 2, 1)),
  z = letters[1:3]
)

ggplot(df, aes(x, y)) + 
  geom_point(aes(colour = z))
  
ggplot(df, aes(x, y)) + 
  geom_point(aes(colour = z)) + 
  facet_wrap(~z)
  
df2 <- dplyr::select(df, -z)

ggplot(df, aes(x, y)) + 
  geom_point(data = df2, colour = "grey70") +
  geom_point(aes(colour = z)) + 
  facet_wrap(~z)

base <- ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth()

# Full dataset
base
# Scaling to 5--7 throws away data outside that range
base + scale_x_continuous(limits = c(5, 7))
# Zooming to 5--7 keeps all the data but only shows some of it
base + coord_cartesian(xlim = c(5, 7))

#==============================================================================
# In a easier way...
#==============================================================================
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)
  
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl)

# Control the number of rows and columns with nrow and ncol
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, nrow = 4)
  
# facet by multiple variables
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ cyl + drv)  
  
# Or use a character vector:
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(c("cyl", "drv"))

# Use the `labeller` option to control how labels are printed:
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(c("cyl", "drv"), labeller = "label_both")

# To change the order in which the panels appear, change the levels
# of the underlying factor.
mpg$class2 <- reorder(mpg$class, mpg$displ)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class2)

# By default, the same scales are used for all panels. You can allow
# scales to vary across the panels with the `scales` argument.
# Free scales make it easier to see patterns within each panel, but
# harder to compare across panels.
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, scales = "free")
  
# To repeat the same data in every panel, simply construct a data frame
# that does not contain the facetting variable.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(data = transform(mpg, class = NULL), colour = "grey85") +
  geom_point() +
  facet_wrap(~class)
  
# Use `strip.position` to display the facet labels at the side of your
# choice. Setting it to `bottom` makes it act as a subtitle for the axis.
# This is typically used with free scales and a theme without boxes around
# strip labels.
ggplot(economics_long, aes(date, value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(), strip.placement = "outside")
