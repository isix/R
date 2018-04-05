#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Plotting Survival Curves
#==============================================================================
# Title          : Missing Value Treatment.r
# Description    : Code to perform missing data analysis and imputation using
#                  a few packages.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180401
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on r-statistics.co by Selva Prabhakaran
#                  ggplot2 documentation
#                  https://www.r-bloggers.com/ggplot-your-missing-data-2/
#                  ggplot your missing data, njtierney - rbloggers
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
# install.packages("VIM")
# install.packages("ggplot2")
# install.packages('mice')
# install.packages("DMwR")
# install.packages('Hmisc')
# install.packages('Amelia')
# install.packages('reshape2')
# install.packages('dplyr')
# install.packages('wakefield')
# install.packages('rpart')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(VIM) # VIM package for Visualization and Imputation of Missing Values
library(mice)
library(ggplot2)
library(DMwR)
library(Hmisc)
library(Amelia)
library(Zelig) # Amelia's LM
library(reshape2)
library(dplyr)
library(wakefield)
library(rpart)

#------------------------------------------------------------------------------
# Data generation/preparation and pattern
#------------------------------------------------------------------------------
n <- 254
mydata <- r_data_frame( 
    n = n, id, race, age, sex, hour, iq, height, died, 
	Scoring = rpois(50), Smoker = valid) %>% r_na(prob=0.0)
mydata <- data.frame(mydata)

# backup original data
original <- mydata  
# Introduce missing values - test purposes.
set.seed(666)
mydata[sample(1:nrow(mydata), 40), "Age"] <- NA
mydata[sample(1:nrow(mydata), 40), "Height"] <- NA
mydata[sample(1:nrow(mydata), 40), "Race"] <- NA

head(mydata)

# list missing values in data.
md.pattern(mydata)  

# Show complete cases
mydata[complete.cases(mydata),]
  
#------------------------------------------------------------------------------
# Correlations to explore missing values  
#------------------------------------------------------------------------------
# Create data frame indicating missingness by 1
x <- as.data.frame(abs(is.na(mydata)))
# Select columns with some (but not all) missing values
y <- x[,sapply(x, sd) > 0]
# Create a correlation matrix: Variables missing together have high correlation
cor(y)
# Rows are observed variables, columns are indicator variables for missingness
# high correlation means the row variables is strongly correlated with 
# missingness of the column variable
cor(mydata[,c(3,6,7,9)], y, use = "pairwise.complete.obs")
  
#------------------------------------------------------------------------------
# Visualising missing data
#------------------------------------------------------------------------------
missmap(mydata) # Amelia package

#..............................................................................
# ggplot_missing(x)
# This function plots missing data of a dataset.
# inputs: a is a data.frame.
# notes: ggplot2 solution, based on njtierney - rbloggers.
ggplot_missing <- function(x, title = "Visualising missing data"){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations") +
	ggtitle(title)
}
#..............................................................................
ggplot_missing(mydata)

# mice ------------------------------------------------------------------------
aggr(mydata, prop = F, numbers = T) # in number
aggr(mydata, prop = T, numbers = T) # in proportions
# red for missing values, darker values are high values.
matrixplot(mydata, interactive = F, sortby = "Age")
# Margin plot. 
# Red dots have at least one missing. 
# No observation with two missing values here.
marginplot(mydata[,c("Age","Height")])

# VIM -------------------------------------------------------------------------
scattmatrixMiss(mydata, interactive = F, highlight = c("Age"))

#------------------------------------------------------------------------------
# Imputation mean / median / mode
#------------------------------------------------------------------------------

# Hmisc -----------------------------------------------------------------------
library(Hmisc)
impute(mydata$Age, 20)  # replace specific number
impute(mydata$Age, mean)  # replace with mean
impute(mydata$Age, median)  # median

# DMwR ------------------------------------------------------------------------
library(DMwR)
actuals <- original$Age[is.na(mydata$Age)]
predicteds <- rep(mean(mydata$Age, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)

#------------------------------------------------------------------------------
# k-Nearest Neighbours (kNN) Imputation
#------------------------------------------------------------------------------

# DMwR ------------------------------------------------------------------------
knnOutput <- knnImputation(mydata[, c("Age","IQ","Height","Scoring")])  
anyNA(knnOutput)

# compute accuracy
actuals <- original$Age[is.na(mydata$Age)]
predicteds <- knnOutput[is.na(mydata$Age), "Age"]
regr.eval(actuals, predicteds)

# rpart -----------------------------------------------------------------------

# categorical
class_mod <- rpart(Race ~ . - IQ, 
	data=mydata[!is.na(mydata$Race), ], method="class", 
	na.action=na.omit)

# numeric
anova_mod <- rpart(Age ~ . - IQ, data=mydata[!is.na(mydata$Age), ], 
	method="anova", na.action=na.omit)
Race_pred <- predict(class_mod, mydata[is.na(mydata$Age), ])
Age_pred <- predict(anova_mod, mydata[is.na(mydata$Age), ])

# compute accuracy
actuals <- original$Age[is.na(mydata$Age)]
predicteds <- Age_pred
regr.eval(actuals, predicteds)

actuals <- original$Race[is.na(BostonHousing$Race)]
predicteds <- as.numeric(colnames(Race_pred)[apply(Race_pred, 1, which.max)])
mean(actuals != predicteds)  # compute misclass error.

#------------------------------------------------------------------------------
# Multiple imputation
#------------------------------------------------------------------------------

# mice ------------------------------------------------------------------------

# Multivariate Imputation by Chained Equations (MICE)
system.time(mi.mydata <- mice(mydata, seed = 666, printFlag = FALSE))
# Check mi object
mice:::print.mids(mi.mydata)
# Plot mi object
plot(mi.mydata)
# Check imputed values for Dream variable (5 columns for 5 datasets)
mi.mydata$imp$Age
# Show 3rd imputed dataset
complete(mi.mydata, action = 3)
# Fit a model for each imputed dataset
mi.fit.Scoring <- with(mi.mydata, lm(Scoring ~ Height + Age))
# Pool results from imputed datasets
mi.pool.res <- pool(mi.fit.Scoring)
# Show summary: nmis: number missing; fmi: fraction of missing information
summary(mi.pool.res)
## Complete case analysis
cc.res <- with(mydata, lm(Scoring ~ Height + Age))
summary(cc.res)

# Amelia ----------------------------------------------------------------------
mydatasub <- mydata[,c(3,6,7,9)]
## Perform imputation
system.time(am.mydata <- amelia(x = mydatasub, 
			parallel = "multicore", p2s = 0))
## Summary of imputed dataset 
summary(am.mydata)
## Plot for imformation
plot(am.mydata)

# Fit linear model using zelig
res.zelig <- zelig(Scoring ~ Height + Age, data = am.mydata, model = "normal")
# results
summary(res.zelig)

# EOF