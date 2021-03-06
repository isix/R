###############################################################################
# Exponential Regression
#------------------------------------------------------------------------------
# based on Lionel Hertzog code.
###############################################################################

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library("easynls")


#------------------------------------------------------------------------------
# Non-linear least squares approach (function nls in R) 
#------------------------------------------------------------------------------

# simulate some data
set.seed(20160227)
x <- seq(0,50,1)
y <- ((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)

# Model -----------------------------------------------------------------------
# for simple models nls find good starting values for the parameters even if it 
# throw a warning
m <- nls(y~a*x/(b+x))

# get some estimation of goodness of fit
cor(y,predict(m))

# Plot ------------------------------------------------------------------------
plot(x,y)
lines(x,predict(m),lty=2,col="red",lwd=3)

#------------------------------------------------------------------------------
# Find correct starting value
#------------------------------------------------------------------------------

# The best way to find correct starting value is to “eyeball” the data, 
# plotting them and based on the understanding that you have from the equation 
# find approximate starting values for the parameters.

# simulate some data, this without a priori knowledge of the parameter value
y <- runif(1,5,15)*exp(-runif(1,0.01,0.05)*x)+rnorm(51,0,0.5)

# visually estimate some starting parameter values
plot(x,y)

# from this graph set approximate starting values
a_start <- 8 #param a is the y value when x=0
b_start <- 2*log(2)/a_start #b is the decay rate

# Model -----------------------------------------------------------------------
m <- nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))

# get some estimation of goodness of fit
cor(y,predict(m))

# Plot ------------------------------------------------------------------------
lines(x,predict(m),col="red",lty=2,lwd=3)



#------------------------------------------------------------------------------
# Non-linear least squares approach (package easynls) 
#------------------------------------------------------------------------------

# data represent weights of an Angus cow at ages from 8 to 108 months (Kaps and Lamberson, 2009)
weight=c(280,340,430,480,550,580,590,600,590,600)
age=c(8,12,24,36,48,60,72,84,96,108)
data1=data.frame(age, weight)
# linear
nlsfit(data1, model=1)
# quadratic
nlsfit(data1, model=2)
# linear plateau
nlsfit(data1, model=3)
# quadratic plateau
nlsfit(data1, model=4)
# two linear
nlsfit(data1, model=5, start=c(250,6,2,50))
# exponential
nlsfit(data1, model=6, start=c(250,0.05))
# logistic
nlsfit(data1, model=7, start=c(600,4,0.05))
# van bertalanffy
nlsfit(data1, model=8, start=c(600,2,0.05))
# brody
nlsfit(data1, model=9, start=c(600,4,0.05))
# gompertz
nlsfit(data1, model=10, start=c(600,4,0.05))
# describe the growth of Zagorje turkeys (Kaps and Lamberson, 2009)
weight=c(44,66,100,150,265,370,455,605,770)
age=c(1,7,14,21,28,35,42,49,56)
data2=data.frame(age,weight)
# two linear
nlsfit(data2, model=5, start=c(25,6,10,20))
# using segmented regression to estimate a plateau
# the requirement for the methionine will be estimated measurements of gain of turkey poults
#(Kaps and Lamberson, 2009)
methionine=c(80,85,90,95,100,105,110,115,120)
gain=c(102,115,125,133,140,141,142,140,142)
data3=data.frame(methionine, gain)
# linear
nlsfit(data3, model=1)
# quadratic
nlsfit(data3, model=2)
# linear plateau
nlsfit(data3, model=3)
# quadratic plateau
nlsfit(data3, model=4)
# lactation curve
milk=c(25,24,26,28,30,31,27,26,25,24,23,24,22,21,22,20,21,19,
18,17,18,18,16,17,15,16,14)
days=c(15,15,15,75,75,75,135,135,135,195,195,195,255,255,255,
315,315,315,375,375,375,435,435,435,495,495,495)
data4=data.frame(days,milk)
nlsfit(data4, model=11, start=c(16,0.25,0.004))
# ruminal degradation
time=c(2,6,9,24,48,72,96)
deg=c(20,33,46,55,66,72,76)
data5=data.frame(time,deg)
nlsfit(data5, model=12)
# logistic bi-compartmental (gas production)
time=c(0,12,24,36,48,60,72,84,96,108,120,144,168,192)
gas=c(0.002,3.8,8,14.5,16,16.5,17,17.4,17.9,18.1,18.8,
19,19.2,19.3)
data6=data.frame(time,gas)
nlsfit(data6, model=13, start=c(19,4,0.025,0.004,5))

# weights of an Angus cow at ages from 8 to 108 months (Kaps and Lamberson, 2009)
weight=c(280,340,430,480,550,580,590,600,590,600)
age=c(8,12,24,36,48,60,72,84,96,108)
data1=data.frame(age, weight)
# linear
nlsplot(data1, model=1)
# quadratic
nlsplot(data1, model=2)
# linear plateau
nlsplot(data1, model=3)
# quadratic plateau
nlsplot(data1, model=4)
# two linear
nlsplot(data1, model=5, start=c(250,6,2,50))
# exponential
nlsplot(data1, model=6, start=c(250,0.05))
# logistic
nlsplot(data1, model=7, start=c(600,4,0.05))
# van bertalanffy
nlsplot(data1, model=8, start=c(600,2,0.05))
# brody
nlsplot(data1, model=9, start=c(600,4,0.05))
# gompertz
nlsplot(data1, model=10, start=c(600,4,0.05))
# growth of Zagorje turkeys (Kaps and Lamberson, 2009)
weight=c(44,66,100,150,265,370,455,605,770)
age=c(1,7,14,21,28,35,42,49,56)
data2=data.frame(age,weight)
# two linear
nlsplot(data2, model=5, start=c(25,6,10,20))
# using segmented regression to estimate a plateau
# requirement for the methionine will be estimated measurements gain of turkey poults
#(Kaps and Lamberson, 2009)
methionine=c(80,85,90,95,100,105,110,115,120)
gain=c(102,115,125,133,140,141,142,140,142)
data3=data.frame(methionine, gain)
# linear
nlsplot(data3, model=1)
# quadratic
nlsplot(data3, model=2)
# linear plateau
nlsplot(data3, model=3)
# quadratic plateau
nlsplot(data3, model=4)
# lactation curve
milk=c(25,24,26,28,30,31,27,26,25,24,23,24,22,21,22,20,21,19,18,17,18,
18,16,17,15,16,14)
days=c(15,15,15,75,75,75,135,135,135,195,195,195,255,255,255,315,315,
315,375,375,375,435,435,435,495,495,495)
data4=data.frame(days,milk)
nlsplot(data4, model=11, start=c(16,0.25,0.004))
# ruminal degradation
time=c(2,6,9,24,48,72,96)
deg=c(20,33,46,55,66,72,76)
data5=data.frame(time,deg)
nlsplot(data5, model=12)
# logistic bi-compartmental (gas production)
time=c(0,12,24,36,48,60,72,84,96,108,120,144,168,192)
gas=c(0.002,3.8,8,14.5,16,16.5,17,17.4,17.9,18.1,18.8,19,19.2,19.3)
data6=data.frame(time,gas)
nlsplot(data6, model=13, start=c(19,4,0.025,0.004,5))


# I’ll take my NLS with weights, please…
# https://www.r-bloggers.com/ill-take-my-nls-with-weights-please/