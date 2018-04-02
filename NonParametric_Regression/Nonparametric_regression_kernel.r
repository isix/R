#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__, |___|___|
#==============================================================================
# Nonparametric Smoothing in Regression
#==============================================================================
# Title          : Nonparametric_regression_kernel.r
# Description    : Nonparametric regression using kernel weights.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20170604
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : Reference 
#                  http://www.stat.cmu.edu/~cshalizi/uADA/12/lectures/ch04.pdf
# R version      : 3.4
#==============================================================================

# Using Nonparametric Smoothing in Regression
x = runif(300, 0, 3)
yf = sin(x)*cos(20*x) + rnorm(length(x), 0, 0.15)
yg = log(x + 1) + rnorm(length(x), 0, 0.15)
par(mfcol=c(2, 1))
plot(x, yf, xlab="x", ylab=expression(f(x) + epsilon))
curve(sin(x)*cos(20*x), col="grey", add=TRUE)
plot(x, yg, xlab="x", ylab=expression(g(x) + eta))
curve(log(x + 1), col="grey", add=TRUE)

# Selecting a region 
windows()
par(mfcol=c(2,1))
colors=ifelse((x<1.7)&(x>1.5),"black","grey")
plot(x,yf,xlab="x",ylab=expression(f(x)+epsilon),col=colors)
curve(sin(x)*cos(20*x),col="grey",add=TRUE)
points(1.6,mean(yf[(x<1.7)&(x>1.5)]),pch="*",cex=2)
plot(x,yg,xlab="x",ylab=expression(g(x)+eta),col=colors)
curve(log(x+1),col="grey",add=TRUE)
points(1.6,mean(yg[(x<1.7)&(x>1.5)]),pch="*",cex=2)

# 
loc_ave_err <- function(h,y,y0) {abs(y0-mean(y[(1.6-h < x) & (1.6+h>x)]))}
yf0=sin(1.6)*cos(20*1.6)
yg0=log(1+1.6)
f.LAE = sapply(0:100/100,loc_ave_err,y=yf,y0=yf0)
g.LAE = sapply(0:100/100,loc_ave_err,y=yg,y0=yg0)
plot(0:100/100,f.LAE,xlab="Radius of averaging window",
ylab="Absolute value of error",type="l")
lines(0:100/100,g.LAE,lty=2)
abline(h=0.15,col="grey")

# Cross-validation for univariate kernel regression
cv_bws_npreg <- function(x,y,bandwidths=(1:50)/50, num.folds=10) {
	require(np)
	n <- length(x)
	stopifnot(n> 1, length(y) == n)
	stopifnot(length(bandwidths) > 1)
	stopifnot(num.folds > 0, num.folds==trunc(num.folds))
	fold_MSEs <- matrix(0,nrow=num.folds,
	ncol=length(bandwidths))
	colnames(fold_MSEs) = bandwidths
	case.folds <- rep(1:num.folds,length.out=n)
	case.folds <- sample(case.folds)
	for (fold in 1:num.folds) {
		train.rows = which(case.folds==fold)
		x.train = x[train.rows]
		y.train = y[train.rows]
		x.test = x[-train.rows]
		y.test = y[-train.rows]
		for (bw in bandwidths) {
			fit <- npreg(txdat=x.train,tydat=y.train,
			exdat=x.test,eydat=y.test,bws=bw)
			fold_MSEs[fold,paste(bw)] <- fit$MSE
		}
	}
	CV_MSEs = colMeans(fold_MSEs)
	best.bw = bandwidths[which.min(CV_MSEs)]
	return(list(best.bw=best.bw, CV_MSEs=CV_MSEs, fold_MSEs=fold_MSEs))
}

fbws <- cv_bws_npreg(x,yf,bandwidths=(1:100)/200)
gbws <- cv_bws_npreg(x,yg,bandwidths=(1:100)/200)
plot(1:100/200,sqrt(fbws$CV_MSEs),xlab="Bandwidth", ylab="Root CV MSE",type="l",ylim=c(0,0.6))
lines(1:100/200,sqrt(gbws$CV_MSEs),lty=2)
abline(h=0.15,col="grey")

x.ord = order(x)
par(mfcol = c(2, 1))
plot(x, yf, xlab = "x", ylab = expression(f(x)+epsilon))
fhat <- npreg(bws = fbws$best.bw, txdat = x, tydat = yf)
lines(x[x.ord], fitted(fhat)[x.ord], lwd = 4)
curve(sin(x)*cos(20*x), col = "grey", add = TRUE, lwd = 2)
plot(x, yg, xlab = "x", ylab = expression(g(x)+eta))
ghat <- npreg(bws = fbws$best.bw, txdat = x, tydat = yg)
lines(x[x.ord], fitted(ghat)[x.ord], lwd = 4)
curve(log(x+1), col = "grey", add = TRUE, lwd = 2)

# Gaussian kernel regression of the points 
noise.np <- npreg(y~x1+x2,data=noise)
y.out <- matrix(0,100,100)
y.out <- predict(noise.np,newdata=x12grid)
wireframe(y.out~x12grid$x1*x12grid$x2,scales=list(arrows=FALSE),xlab=expression(x^1),ylab=expression(x^2),zlab="y")

# Average Predictive Comparisons
new.frame <- data.frame(x=seq(-3,3,length.out=300),y=median(y.noise))
plot(new.frame$x,predict(noise.np,newdata=new.frame),
type="l",xlab=expression(x^1),ylab="y",ylim=c(0,1.0))
new.frame$y <- quantile(y.noise,0.25)
lines(new.frame$x,predict(noise.np,newdata=new.frame),lty=2)
new.frame$y <- quantile(y.noise,0.75)
lines(new.frame$x,predict(noise.np,newdata=new.frame),lty=3)

curve(exp(7*x)/(1+exp(7*x)),from=-5,to=5,ylab="y")
