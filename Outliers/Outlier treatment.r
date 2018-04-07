#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Outlier Treatment
#==============================================================================
# Title          : Outlier treatment.r
# Description    : Outlier Treatment
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20170822
# Version        : 0.0.2
# Usage          : Run in R 3.4
# Notes          : Source r-statistics.co by Selva Prabhakaran
# R version      : 3.4
#==============================================================================
# install.packages('car')
# install.packages('outliers')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(car)
library(outliers)
 
#==============================================================================
# Data
#==============================================================================

# Inject outliers into data.
cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(
	speed=c(19,19,20,20,20), 
	dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

# Plot of data with outliers.
par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), 
	main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of 
# best fit line.
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), 
	main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", 
	pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

#==============================================================================
# Detect outliers
#==============================================================================

#------------------------------------------------------------------------------
# Univariate approach ----------------------------------------------------------
#------------------------------------------------------------------------------
url <- "http://rstatistics.net/wp-content/uploads/2015/09/ozone.csv"  
inputData <- read.csv(url)  # import data

outlier_values <- boxplot.stats(inputData$pressure_height)$out  # outlier values.
boxplot(inputData$pressure_height, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#------------------------------------------------------------------------------
# Bivariate approach ----------------------------------------------------------
#------------------------------------------------------------------------------
ozone <- read.csv(url)
# For categorical variable
boxplot(ozone_reading ~ Month, data=ozone, 
	main="Ozone reading across months")  # clear pattern is noticeable.
boxplot(ozone_reading ~ Day_of_week, data=ozone, 
	main="Ozone reading for days of week")  
	# this may not be significant, as day of week variable is a subset of the 
	# month var.

# For continuous variable (convert to categorical if needed.)
boxplot(ozone_reading ~ pressure_height, data=ozone, 
	main="Boxplot for Pressure height (continuos var) vs Ozone")
boxplot(ozone_reading ~ cut(pressure_height, 
	pretty(inputData$pressure_height)), data=ozone, 
	main="Boxplot for Pressure height (categorial) vs Ozone", cex.axis=0.5)

#------------------------------------------------------------------------------
# Multivariate approach -------------------------------------------------------
#------------------------------------------------------------------------------

# Cooks Distance --------------------------------------------------------------
# https://onlinecourses.science.psu.edu/stat501/node/340

mod <- lm(ozone_reading ~ ., data=ozone)
cooksd <- cooks.distance(mod)

# In general use, those observations that have a cook’s distance greater than 
# 4 times the mean may be classified as influential. This is not a hard 
# boundary.

# plot cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, 
	labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
	names(cooksd),""), col="red")  # add labels
# influential row numbers	
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  
head(ozone[influential, ])  # influential observations.

#==============================================================================
# Outliers Test
#==============================================================================
car::outlierTest(mod)

# outliers package ------------------------------------------------------------
set.seed(666)
y=rnorm(100)
outlier(y)
outlier(y,opposite=TRUE)
dim(y) <- c(20,5)  # convert it to a matrix
outlier(y)
outlier(y,opposite=TRUE)

set.seed(666)
x = rnorm(10)
scores(x)  # z-scores => (x-mean)/sd
scores(x, type="chisq")  # chi-sq scores => (x - mean(x))^2/var(x)
scores(x, type="t")  # t scores
scores(x, type="chisq", prob=0.9)  # beyond 90th %ile based on chi-sq
scores(x, type="chisq", prob=0.95)  # beyond 95th %ile
scores(x, type="z", prob=0.95)  # beyond 95th %ile based on z-scores
scores(x, type="t", prob=0.95)  # beyond 95th %ile based on t-scores

#==============================================================================
# Treating the outliers
#==============================================================================
# 1. Imputation
# 2. Capping
#     For missing values that lie outside the 1.5*IQR limits, we could cap it 
#     by replacing those observations outside the lower limit with the value of 
#     5th-ile and those that lie above the upper limit, with the value of 95th 
#     -ile. Below is a sample code that achieves this.
# 3. Prediction

x <- ozone$pressure_height
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
