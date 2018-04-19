#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Discriminant Analysis
#==============================================================================
# Title          : DiscriminantAnalysis.r
# Description    : Discriminant Analysis including linear and quadratic 
#                  functions.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180412
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
# https://www.statmethods.net/advstats/discriminant.html
# https://www.r-bloggers.com/linear-discriminant-analysis-in-r-an-introduction/
# https://ggobi.github.io/ggally
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
install.packages("MASS")
install.packages("car")
install.packages("klaR")
install.packages("ggplot2")
install.packages("mvoutlier")
install.packages("mvnormtest")
install.packages("GGally")
install.packages("RColorBrewer")
install.packages("dplyr")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(MASS)
library(car)
library(klaR)
library(ggplot2)
library(mvoutlier)
library(mvnormtest)
library(GGally)
library(RColorBrewer)
library(dplyr)

#------------------------------------------------------------------------------
# Data generation/preparation and pattern
#------------------------------------------------------------------------------
# mydata <- mtcars
# http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data
mydata <- 
read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
sep=",")

# Variables names 
# (http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names)
# 1) Alcohol
# 2) Malic acid
# 3) Ash
# 4) Alcalinity of ash  
# 5) Magnesium
# 6) Total phenols
# 7) Flavanoids
# 8) Nonflavanoid phenols
# 9) Proanthocyanins
# 10)Color intensity
# 11)Hue
# 12)OD280/OD315 of diluted wines
# 13)Proline            
names(mydata) <- c("Class", "Alcohol", "MalicAcid", "Ash", "AlcalinityAsh", 
"Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", 
"Proanthocyanins", "Color", "Hue", "OD280_OD315", "Proline")
# Visualizing data
scatterplotMatrix(mydata)

ggpairs(mydata)

plot(mydata$Flavanoids, mydata$NonflavanoidPhenols)
text(mydata$Flavanoids, mydata$NonflavanoidPhenols, mydata$Alcohol, cex=0.7, pos=4, col="red")

# Profile Plot - how is the variation in each of the variables
makeProfilePlot <- function(mylist,names)
  {
     require(RColorBrewer)
     # find out how many variables we want to include
     numvariables <- length(mylist)
     # choose 'numvariables' random colours
     colours <- brewer.pal(numvariables,"Set1")
     # find out the minimum and maximum values of the variables:
     mymin <- 1e+20
     mymax <- 1e-20
     for (i in 1:numvariables)
     {
        vectori <- mylist[[i]]
        mini <- min(vectori)
        maxi <- max(vectori)
        if (mini < mymin) { mymin <- mini }
        if (maxi > mymax) { mymax <- maxi }
     }
     # plot the variables
     for (i in 1:numvariables)
     {
        vectori <- mylist[[i]]
        namei <- names[i]
        colouri <- colours[i]
        if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
        else         { points(vectori, col=colouri,type="l")                                     }
        lastxval <- length(vectori)
        lastyval <- vectori[length(vectori)]
        text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
     }
  }

lnames <- names(mydata)[c(6,2,3)]
mylist <- list(mydata$Magnesium, mydata$Alcohol, mydata$MalicAcid)
makeProfilePlot(mylist,lnames)  

# Descriptive statistics
summary(mydata , digits = 2)
tapply(mydata$Alcohol, mydata$Class, summary)
mydata %>% group_by(mydata$Class) %>% summarize(mean=mean(mydata$Alcohol), sum=sum(mydata$Magnesium))


#------------------------------------------------------------------------------
# Assumptions 
#------------------------------------------------------------------------------
#
# 1) Multivariate normality and homogeneity of covariance matrices.
# 2) Assumes that the independent variables are normally distributed.
# 3) The analysis is quite sensitive to outliers and the size of the smallest group must be larger than the number of predictor variables.[7]
# 4) LDA assumes linearly separable latent representations.
# 
# Multivariate normality: Independent variables are normal for each level of the grouping variable.
# Homogeneity of variance/covariance (homoscedasticity): Variances among group variables are the same across levels of predictors. Can be tested with Box's M statistic.It has been suggested, however, that linear discriminant analysis be used when covariances are equal, and that quadratic discriminant analysis may be used when covariances are not equal.
# Multicollinearity: Predictive power can decrease with an increased correlation between predictor variables.
# Independence: Participants are assumed to be randomly sampled, and a participant’s score on one variable is assumed to be independent of scores on that variable for all other participants.
# It has been suggested that discriminant analysis is relatively robust to slight violations of these assumptions, and it has also been shown that discriminant analysis may still be reliable when using dichotomous variables (where multivariate normality is often violated).
#==============================================================================
# Testing assumptions
#==============================================================================

# Outliers --------------------------------------------------------------------
# Detect Outliers in the MTCARS Data
outliers <- aq.plot(mydata[c("Alcohol", "MalicAcid", "Ash", "AlcalinityAsh", 
"Color")])
outliers

# Multivariate Normality ------------------------------------------------------
Gm <- as.matrix(mydata[,c("Alcohol", "MalicAcid", "Ash", "AlcalinityAsh", 
"Color")])
mshapiro.test(Gm)

# Graphical Assessment of Multivariate Normality
x <- as.matrix(Gm) # n x p numeric matrix
center <- colMeans(x) # centroid
n <- nrow(x); p <- ncol(x); cov <- cov(x); 
d <- mahalanobis(x,center,cov) # distances 
qqplot(qchisq(ppoints(n),df=p),d,
  main="QQ Plot Assessing Multivariate Normality",
  ylab="Mahalanobis D2")
abline(a=0,b=1)

# In case no multinormality, preceed an inspection for problems univariately 
# Univariate Normality

# Q-Q Plot for variable 
qqnorm(mydata$Alcohol) ; qqline(mydata$Alcohol)

# Homogeneity of Variances ----------------------------------------------------

# Bartlett Test of Homogeneity of Variances
mylist4 <- list(mydata$Alcohol, mydata$MalicAcid, mydata$Ash, 
				mydata$AlcalinityAsh, mydata$Color)

bartlett.test(mylist, data=mydata)

# Figner-Killeen Test of Homogeneity of Variances
fligner.test(mylist, data=mydata)

# HH package provides a graphic test of homogeneity of variances based on 
# Brown-Forsyth. In the following example, y is numeric and G is a grouping 
# factor. Note that G must be of type factor.


#==============================================================================
# Linear Discriminant Analysis with Jacknifed Prediction (LDA)
#==============================================================================

# LDA, using listwise deletion of missing data. CV=TRUE generates jacknifed 
# (i.e., leave one out) predictions. 
G <- mydata[,c("Class", "Alcohol", "MalicAcid", "Ash", "AlcalinityAsh", "Color")]

model.lda <- lda(Class ~ ., data = G, na.action="na.omit", CV=TRUE)
model.lda

model.lda <- lda(Class ~ ., data = G, na.action="na.omit") # no validation
model.lda

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(mydata$G, model.lda$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# lda() prints discriminant functions based on centered (not standardized) variables. The "proportion of trace" that is printed is the proportion of between-class variance that is explained by successive discriminant functions.

#==============================================================================
# Plots
#==============================================================================

# Scatter plot using the 1st two discriminant dimensions 
plot(fit) # fit from lda

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(fit, dimen=1, type="both") # fit from lda

# The partimat( ) function in the klaR package can display the results of a linear or quadratic classifications 2 variables at a time.

# Exploratory Graph for LDA or QDA
partimat(G~x1+x2+x3,data=mydata,method="lda")

# Scatterplot for 3 Group Problem 
pairs(mydata[c("x1","x2","x3")], main="My Title ", pch=22, 
   bg=c("red", "yellow", "blue")[unclass(mydata$G)])
   
   
#==============================================================================
# Quadratic Discriminant Analysis (QDA)
#==============================================================================

# Quadratic Discriminant Analysis with resubstitution prediction and equal prior probabilities. 
library(MASS)
fit <- qda(G ~ x1 + x2 + x3 + x4, data=na.omit(mydata),
  prior=c(1,1,1)/3))
  
# EOF