# 't-distributed stochastic neighbor embedding' (t-SNE) is a machine learning 
# algorithm for dimensionality reduction developed by Geoffrey Hinton and 
# Laurens van der Maaten. t-SNE is a popular method for making an easy to read 
# Graph from a complex dataset.
#
# SOURCE for this code and PPT
# help file for Rtsne, StatQuest
#
# REFERENCE: https://lvdmaaten.github.io/tsne/

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library("Rtsne")

#==============================================================================
 
iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])

set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(iris_matrix) # Run TSNE
 
# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=iris_unique$Species)



# EOF