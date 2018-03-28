###################################################################
# MDS and PCoA
#
# Reference: https://goo.gl/wUXSae
# https://goo.gl/ckSGFW
###################################################################

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)

# Load the data and add some whitenoise as control
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))
rownames(data.matrix) <- paste("gene", 1:100, sep="")
for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
 
  data.matrix[i,] <- c(wt.values, ko.values)
}
head(data.matrix)
dim(data.matrix)
 
#==============================================================================
# Computing PCA as reference
#==============================================================================
pca <- prcomp(t(data.matrix), scale=TRUE, center=TRUE) 
 
## calculate the percentage of variation that each PC accounts for...
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
 
## now make a fancy looking plot that shows the PCs and the variation:
pca.data <- data.frame(Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2])
pca.data
 
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph")
 
#==============================================================================
# PCoA
# 	PCA = PCoA if distance metric is Euclidean.
# 	Different distance metrics produces diferent results
#==============================================================================
distance.matrix <- dist(scale(t(data.matrix), center=TRUE, scale=TRUE), 
					method="euclidean")
 
## do the MDS math (this is basically eigen value decomposition)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
 
## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per
 
## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
						X=mds.values[,1],
						Y=mds.values[,2])
mds.data
 
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
	geom_text() +
	theme_bw() +
	xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
	ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
	ggtitle("MDS plot using Euclidean distance")
 
#==============================================================================
# Log scale
#==============================================================================
log2.data.matrix <- log2(data.matrix)
 
# empty distance matrix
log2.distance.matrix <- matrix(0,
  nrow=ncol(log2.data.matrix),
  ncol=ncol(log2.data.matrix),
  dimnames=list(colnames(log2.data.matrix),
    colnames(log2.data.matrix)))
 
log2.distance.matrix
 
# now compute the distance matrix using avg(absolute value(log2(FC)))
for(i in 1:ncol(log2.distance.matrix)) {
  for(j in 1:i) {
    log2.distance.matrix[i, j] <-
      mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]))
  }
}
log2.distance.matrix
 
## do the MDS math (this is basically eigen value decomposition)
## cmdscale() is the function for "Classical Multi-Dimensional Scalign"
mds.stuff <- cmdscale(as.dist(log2.distance.matrix),
  eig=TRUE,
  x.ret=TRUE)
 
## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per
 
## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
  X=mds.values[,1],
  Y=mds.values[,2])
mds.data
 
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using avg(logFC) as the distance")
