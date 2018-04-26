#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Word Cloud plot of Bible
#==============================================================================
# Title          : Word_Cloud_plot.r
# Description    : Wordclouds in R.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180422
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
#                  https://www.r-bloggers.com/building-wordclouds-in-r/
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
if(!require("tm")) install.packages("tm")
if(!require("SnowballC")) install.packages("SnowballC")
if(!require("wordcloud")) install.packages("wordcloud")


#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(tm)
library(SnowballC)
library(wordcloud)

#==============================================================================
# Fire in the hole!
#==============================================================================

# Bible text data
# mycsvfile <- "http://www.gutenberg.org/cache/epub/10/pg10.txt"

# The World English Bible (WEB)
mycsvfile <- "http://www.gutenberg.org/cache/epub/8294/pg8294.txt"

# Remove useless and empty lines
dfdata <- readLines(mycsvfile)
dfdata <- dfdata[1:99849,] # JK
dfdata <- dfdata[43:79122,] # WEB


nonempty <- sapply(dfdata,nchar)
names(nonempty) <- NULL

pos_sel <- nonempty != 0

length(dfdata)
length(pos_sel)

dfdata <- dfdata[(nonempty != 0)]

length(dfdata)
length(pos_sel)

# Create a corpus
jobTitles <- Corpus(VectorSource(dfdata))

# Convert to plain text document
jobTitles <- tm_map(jobTitles, PlainTextDocument)

# Remove numbers and punctuation, just in case
jobTitles <- tm_map(jobTitles, removeNumbers)
jobTitles <- tm_map(jobTitles, removePunctuation)

# Make all jobTitles lowercase
jobTitles <- tm_map(jobTitles, content_transformer(tolower))

# Remove non job title words
jobTitles <- tm_map(jobTitles, removeWords, stopwords('english'))
jobTitles <- tm_map(jobTitles, removeWords, c("and", "the", "unto", "thi"))

# Generate the wordcloud
wordcloud(jobTitles, 
          scale = c(5,0.2), 
          max.words = 150, 
          random.order = FALSE, 
          rot.per = 0.35, 
          use.r.layout = TRUE, 
          colors = brewer.pal(6, "Blues")[c(4,5,6,7,8,9)])
		  
windows()
# Generate the wordcloud
wordcloud(jobTitles, 
          scale = c(5,0.2), 
          max.words = 150, 
          random.order = TRUE, 
          rot.per = 0.35, 
          use.r.layout = TRUE, 
          colors = brewer.pal(6, "Blues")[c(4,5,6,7,8,9)])
		  
# "Je n'ai fait celle-ci plus longue que parce que je n'ai pas eu le loisir de la faire plus courte."
# - Provincial Letters: Letter XVI (4 December 1656). 
# (I would have written a shorter letter, but I did not have the time.

