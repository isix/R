###############################################################################
# Hierarchical Clustering and Plotting
#------------------------------------------------------------------------------
# based on Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)
###############################################################################

# install.packages('ggplot2')
# install.packages('ggdendro')
# install.packages('vegan')

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)
library(ggdendro)
library(vegan)

#==============================================================================
# Let's rock'n'roll!
#==============================================================================
# Load the abundance table 
abund_table <- read.csv("..//Data//SPE_pitlatrine.csv", 
					row.names = 1, check.names = FALSE)
 
# Transpose the data to have sample names on rows
abund_table <- t(abund_table)
 
# Get grouping information
grouping_info <- data.frame(row.names = rownames(abund_table), 
				t(as.data.frame(strsplit(rownames(abund_table), "_"))))
head(grouping_info)
 
betad <- vegdist(abund_table, method = "bray")
 
# Use Adonis to test for overall differences
res_adonis <- adonis(betad ~ X1, grouping_info) 
 
# Cluster the samples
hc <- hclust(betad)

#==============================================================================
# Plots
#==============================================================================
 
# We will color the labels according to countries(group_info[, 1])
hc_d <- dendro_data(as.dendrogram(hc))
hc_d$labels$Type <- grouping_info[as.character(hc_d$labels$label), 1]
 
# Coloring function
gg_color_hue <- function(n){
 hues = seq(15, 375, length = n+1)
 hcl(h = hues, l = 65, c = 100)[1:n]
}
 
cols = gg_color_hue(length(unique(hc_d$labels$Type)))
hc_d$labels$color = cols[hc_d$labels$Type]
 
p1 <- ggplot(data = segment(hc_d)) +
 geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
 coord_flip() +
 scale_x_discrete(labels = label(hc_d)$label) +
 ylab("Distance (beta diversity = bray)") + theme_bw()+
 theme(axis.text.y = element_text(color = hc_d$labels$color), 
 axis.title.y = element_blank())
p1 <- p1 + geom_point(data = hc_d$label, aes(x = x, y = y, color = Type), 
		inherit.aes = F, alpha = 0)
p1 <- p1 + guides(colour = guide_legend(override.aes = 
		list(size = 3, alpha = 1)))+
		scale_color_manual(values = cols)
pdf("Cluster.pdf", height = 10)
print(p1)
dev.off()