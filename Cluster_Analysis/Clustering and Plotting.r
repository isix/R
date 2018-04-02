###############################################################################
# Clustering and Plotting
#------------------------------------------------------------------------------
# R documentation
###############################################################################

#------------------------------------------------------------------------------
# ggplot K-Means Cluster Centers and Clusters
#------------------------------------------------------------------------------

data(mtcars)
library(ggplot2)
c1 <- kmeans(mtcars,9)
x <- tapply(mtcars$mpg,c1$cluster,mean)
y <- tapply(mtcars$hp,c1$cluster,mean)
kcenters <- data.frame(x,y)
ggplot(mtcars,aes(mpg,hp))+geom_point(col=c1$cluster,size=4) +  
	geom_point(data=kcenters,aes(x,y),pch=8,size=10)

ggplot(mtcars,aes(mpg,hp))+geom_point(col=c1$cluster,size=4) +  
    geom_point(data=kcenters,aes(x,y),pch=8,size=10,colour=1:9)
	 
ggplot(cbind(mtcars, cluster=factor(c1$cluster)))+
    geom_point(aes(mpg,hp, col=cluster),size=4) +
    geom_point(data=cbind(kcenters, cluster=factor(1:nrow(kcenters))),aes(x,y, col=cluster),pch=8,size=10)

#------------------------------------------------------------------------------
# Plot Clusters and Centroids - by https://gist.github.com/thoolihan
#------------------------------------------------------------------------------

library(ggplot2)

cars <- mtcars
cars$cyl <- factor(cars$cyl, labels = 
                     c('Four cylinder', 'Six cylinder', 'Eight cylinder'))

features <- c('wt', 'qsec')
n_clusters <- 3
car_clusters <- kmeans(cars[, features], n_clusters, nstart = 30)

cars$cluster <- factor(car_clusters$cluster)

centroids <- data.frame(cluster = factor(seq(1:n_clusters)),
                        wt = car_clusters$centers[,'wt'],
                        qsec = car_clusters$centers[,'qsec'])

# cross tab of cylinder by cluster
print(table(cars$cluster, cars$cyl))

g <- ggplot() + 
  geom_point(data = cars, 
             aes(x = wt, 
                 y = qsec,
                 color = cluster),
            size = 3) +
  geom_text(data = cars,
            aes(x = wt,
                y = qsec,
                label = row.names(cars),
                color = cluster),
            nudge_y = .2,
            check_overlap = TRUE) +
  geom_point(data = centroids,
             mapping = aes(x = wt,
                           y = qsec,
                           color = cluster),
             size = 20,
             pch = 13) 

print(g)
	
#------------------------------------------------------------------------------
# Cluster analysis in R: determine the optimal number of clusters
#------------------------------------------------------------------------------
n = 1000
kk = 10    
x1 = runif(kk)
y1 = runif(kk)
z1 = runif(kk)    
x4 = sample(x1,length(x1))
y4 = sample(y1,length(y1)) 
randObs <- function()
{
  ix = sample( 1:length(x4), 1 )
  iy = sample( 1:length(y4), 1 )
  rx = rnorm( 1, x4[ix], runif(1)/8 )
  ry = rnorm( 1, y4[ix], runif(1)/8 )
  return( c(rx,ry) )
}  
x = c()
y = c()
for ( k in 1:n )
{
  rPair  =  randObs()
  x  =  c( x, rPair[1] )
  y  =  c( y, rPair[2] )
}
z <- rnorm(n)
d <- data.frame( x, y, z )

#------------------------------------------------------------------------------
# Data generation
#------------------------------------------------------------------------------

g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)

#  Look for a bend or elbow in the sum of squared error (SSE) scree plot. See http://www.statmethods.net/advstats/cluster.html & http://www.mattpeeples.net/kmeans.html for more. The location of the elbow in the resulting plot suggests a suitable number of clusters for the kmeans:

mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
windows()
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
	 
# You can do partitioning around medoids to estimate the number of clusters using the pamk function in the fpc package.

library(fpc)
pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(d, pamk.best$nc))

# we could also do:
library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(d, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
# still 4

require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!

#------------------------------------------------------------------------------
# optimal model and number of clusters according to the BIC
#------------------------------------------------------------------------------
# Determine the optimal model and number of clusters according to the Bayesian 
# Information Criterion for expectation-maximization, initialized by 
# hierarchical clustering for parameterized Gaussian mixture models
# 
# See http://www.jstatsoft.org/v18/i06/paper
# http://www.stat.washington.edu/research/reports/2006/tr504.pdf

library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(d), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)

#------------------------------------------------------------------------------
# Affinity propagation (AP) clustering, see http://dx.doi.org/10.1126/science.1136800
#------------------------------------------------------------------------------

library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), d)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
# 4
heatmap(d.apclus)
plot(d.apclus, d)

#------------------------------------------------------------------------------
# Gap Statistic for Estimating the Number of Clusters
#------------------------------------------------------------------------------
# Gap Statistic for Estimating the Number of Clusters. See also some code for 
# a nice graphical output. Trying 2-10 clusters here:
# You may also find it useful to explore your data with clustergrams to visualize cluster assignment, see 
# http://www.r-statistics.com/2010/06/clustergram-visualization-and-diagnostics-for-cluster-analysis-r-code/ 
# for more details.

library(cluster)
clusGap(d, kmeans, 10, B = 100, verbose = interactive())

#------------------------------------------------------------------------------
# Indices to determine the number of clusters in a dataset
#------------------------------------------------------------------------------
# The NbClust package provides 30 indices to determine the number of clusters in a dataset.

library(NbClust)
nb <- NbClust(d, diss="NULL", distance = "euclidean", 
        min.nc=2, max.nc=15, method = "kmeans", 
        index = "alllong", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
# Looks like 3 is the most frequently determined number of clusters
# and curiously, four clusters is not in the output at all!

d_dist <- dist(as.matrix(d))   # find distance matrix 
plot(hclust(d_dist))           # apply hirarchical clustering and plot

# a Bayesian clustering method, good for high-dimension data, more details:
# http://vahid.probstat.ca/paper/2012-bclust.pdf
install.packages("bclust")
library(bclust)
x <- as.matrix(d)
d.bclus <- bclust(x, transformed.par = c(0, -50, log(16), 0, 0, 0))
viplot(imp(d.bclus)$var); plot(d.bclus); ditplot(d.bclus)
dptplot(d.bclus, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus)$var, horizbar.distance = 0, dendrogram.lwd = 2)
# I just include the dendrogram here

# Other
library(pvclust)
library(MASS)
data(Boston)
boston.pv <- pvclust(Boston)
plot(boston.pv)

#------------------------------------------------------------------------------
# Plots and Centroids on plot
#------------------------------------------------------------------------------
# http://www.mattpeeples.net/kmeans.html
df=iris
m=as.matrix(cbind(df$Petal.Length, df$Petal.Width),ncol=2)
cl=(kmeans(m,3))
cl$size
cl$withinss
df$cluster=factor(cl$cluster)
centers=as.data.frame(cl$centers)
library(ggplot2)

ggplot(data=df, aes(x=Petal.Length, y=Petal.Width, color=cluster )) + 
 geom_point() + 
 geom_point(data=centers, aes(x=V1,y=V2, color=’Center’)) +
 geom_point(data=centers, aes(x=V1,y=V2, color=’Center’), size=52, alpha=.3, legend=FALSE)
 
# Misclassified Observations

sqldf('select Species, cluster, count(*) from df group by Species, Cluster')
df2 = sqldf('select * from df where (Species || cluster) in 
             (select Species || cluster from df group by Species, Cluster having count(*) < 10)')
last_plot() +  geom_point(data=df2, aes(x=Petal_Length, y=Petal_Width, shape=5, alpha=.7, size=4.5), legend=FALSE) 
 