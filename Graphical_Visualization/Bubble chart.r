#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Bubble chart
#==============================================================================
# Title          : Bubble chart.r
# Description    : Bubble chart plotting.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180407
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
# http://flowingdata.com/2010/11/23/how-to-make-bubble-charts/
# 
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
install.packages("ggplot2")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)

#------------------------------------------------------------------------------
# Data generation/preparation and pattern
#------------------------------------------------------------------------------
mydata2 <- read.table("..//Data//Tomatos.dat",  sep="\t", header=TRUE)
mydata3 <- data.frame(mydata2, Type = 
c('OntarioRed Salsa','Ohio Red Breakfast Sandwich',
'Chicken & Pepper Cobb Salad','Tomato Jelly',
'Green Tomato Pie','Green Tomato Blueberry Jam'))

dbconn01 <- textConnection(
"Row PowerSource ProductSegment Price Model ManufacturingLocation Quantity
1 High SegmentA Low ModA LocationA 5000
2 Low SegmentB Low ModB LocationB 25000
3 High SegmentC Low ModC LocationC 15000
4 Low SegmentD High ModD LocationD 30000
5 High SegmentE High ModE LocationA 2500
6 Low SegmentA Low ModF LocationB 110000
7 High SegmentB Low ModG LocationC 20000
8 Low SegmentC Low ModH LocationD 3500
9 High SegmentD Low ModI LocationA 65500
10 Low SegmentE Low ModJ LocationB 145000
11 High SegmentA Low ModK LocationC 15000
12 Low SegmentB Low ModL LocationD 5000
13 High SegmentC Low ModM LocationA 26000
14 Low SegmentD Low ModN LocationB 14000
15 High SegmentE Mid ModO LocationC 75000
16 Low SegmentA High ModP LocationD 33000
17 High SegmentB Low ModQ LocationA 14000
18 Low SegmentC Mid ModR LocationB 33000
19 High SegmentD High ModS LocationC 95000
20 Low SegmentE Low ModT LocationD 4000
 ")
mydata <- read.table(dbconn01, header= TRUE)
close(zz) ; rm(mydata2) ; mydata

#------------------------------------------------------------------------------
# ggplot - bubble chart
#------------------------------------------------------------------------------
ggplot(mydata, aes(x = ManufacturingLocation, y = PowerSource, label = Model)) +
    geom_point(aes(size = Quantity, colour = Price)) + 
    geom_text(hjust = 1, size = 2) +
    scale_size(range = c(1,15)) +
    theme_bw()

	
set.seed(666666)
x=sample(seq(1,50),50,T)
y=sample(seq(1,50),50,T)
value = rnorm(50,10,20)
plot_dat=data.frame(x=x,y=y,value=value)
ggplot(data=plot_dat, aes(x=x, y=y,colour=factor(sign(value)), size=abs(value))) +
      geom_point() +
      scale_color_manual(values=c("orange","blue"),guide=FALSE)+
      scale_size_continuous(breaks=c(10,10,20,20,30,30,40,40,50,50),
	  labels=c(-50,-40,-30,-20,-10,10,20,30,40,50),range = c(1,5))+
      guides(size = guide_legend(override.aes = list(colour = 
	  list("orange","orange","orange","orange","orange","blue","blue","blue","blue","blue"),
           size=c(4.92,4.14,3.50,2.56,1.78,1.78,2.56,3.50,4.14,4.92))))
# bubbles ---------------------------------------------------------------------
data <- read.csv(
"https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

p <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', 
	mode = 'markers', marker = list(size = ~gap, opacity = 0.5)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="bubble-simple")
chart_link

# EOF