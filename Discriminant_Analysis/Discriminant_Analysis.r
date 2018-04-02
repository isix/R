###############################################################################
# Discriminant Analysis
#------------------------------------------------------------------------------
# based on http://rpubs.com/sinhrks/plot_pca
###############################################################################
# install.packages("lfda")
# install.packages("ggplot2")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(lfda)
library(ggplot2)

#==============================================================================
# Let's rock'n'roll!
#==============================================================================
# Local Fisher Discriminant Analysis (LFDA)
model <- lfda(iris[-5], iris[, 5], 4, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

# Kernel Local Fisher Discriminant Analysis (KLFDA)
model <- klfda(kmatrixGauss(iris[-5]), iris[, 5], 4, metric="plain")
windows()
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

# Semi-supervised Local Fisher Discriminant Analysis (SELF)
model <- self(iris[-5], iris[, 5], beta = 0.1, r = 3, metric="plain")
windows()
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

