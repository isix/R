###############################################################################
# Negative Binomial GLM fitting 
#------------------------------------------------------------------------------
# The DESeq {DESeq2} package allows negative binomial GLM fitting and Wald 
# statistics for abundance data. You can use this script as an alternative to 
# KW.R to find taxa that are significantly different between different 
# conditions.
#------------------------------------------------------------------------------
# based on Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)
# and https://github.com/MadsAlbertsen/ampvis/blob/master/R/amp_test_species.R
###############################################################################

# install.packages('DESeq2') # Install package.

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(ggplot2)
library(DESeq2)

#==============================================================================
# Let's rock'n'roll!
#==============================================================================

#Load the abundance table 
abund_table <- read.csv("SPE_pitlatrine.csv", row.names=1, check.names=FALSE)
 
#Transpose the data to have sample names on rows
abund_table <- t(abund_table)
 
#Get grouping information
grouping_info <- data.frame(row.names=rownames(abund_table), t(as.data.frame(strsplit(rownames(abund_table), "_"))))
# > head(grouping_info)
 
#We will convert our table to DESeqDataSet object
countData = round(as(abund_table,  "matrix"),  digits = 0)
# We will add 1 to the countData otherwise DESeq will fail with the error:
# estimating size factors
# Error in estimateSizeFactorsForMatrix(counts(object),  locfunc = locfunc,   : 
# every gene contains at least one zero,  cannot compute log geometric means
countData <- (t(countData+1)) 
 
dds  <-  DESeqDataSetFromMatrix(countData,  grouping_info,  as.formula(~ X1))
 
#Reference:https://github.com/MadsAlbertsen/ampvis/blob/master/R/amp_test_species.R
 
#Differential expression analysis based on the Negative Binomial (a.k.a. Gamma-Poisson) distribution
#Some reason this doesn't work: data_deseq_test = DESeq(dds,  test="wald",  fitType="parametric")
data_deseq_test = DESeq(dds)
 
## Extract the results
res = results(data_deseq_test,  cooksCutoff = FALSE)
res_tax = cbind(as.data.frame(res),  as.matrix(countData[rownames(res),  ]),  OTU = rownames(res))
 
sig = 0.001
fold = 0
plot.point.size = 2
label=T
tax.display = NULL
tax.aggregate = "OTU"
 
res_tax_sig = subset(res_tax,  padj < sig & fold < abs(log2FoldChange))
 
res_tax_sig  <-  res_tax_sig[order(res_tax_sig$padj), ]
 
## Plot the data
### MA plot
res_tax$Significant  <-  ifelse(rownames(res_tax) %in% rownames(res_tax_sig) ,  "Yes",  "No")
res_tax$Significant[is.na(res_tax$Significant)]  <-  "No"
p1  <-  ggplot(data = res_tax,  aes(x = baseMean,  y = log2FoldChange,  color = Significant)) +
  geom_point(size = plot.point.size) +
  scale_x_log10() +
  scale_color_manual(values=c("black",  "red")) +
  labs(x = "Mean abundance",  y = "Log2 fold change")+theme_bw()
if(label == T){
  if (!is.null(tax.display)){
    rlab  <-  data.frame(res_tax,  Display = apply(res_tax[, c(tax.display,  tax.aggregate)],  1,  paste,  collapse="; "))
  } else {
    rlab  <-  data.frame(res_tax,  Display = res_tax[, tax.aggregate])
  }
  p1  <-  p1 + geom_text(data = subset(rlab,  Significant == "Yes"),  aes(label = Display),  size = 4,  vjust = 1)
}
pdf("NB_MA.pdf")
print(p1)
dev.off()
 
res_tax_sig_abund = cbind(as.data.frame(countData[rownames(res_tax_sig),  ]),  OTU = rownames(res_tax_sig),  padj = res_tax[rownames(res_tax_sig), "padj"]) 
 
#Apply normalisation (either use relative or log-relative transformation)
#data <- abund_table/rowSums(abund_table)
data <- log((abund_table+1)/(rowSums(abund_table)+dim(abund_table)[2]))
data <- as.data.frame(data)
 
#Now we plot taxa significantly different between the categories
df <- NULL
for(i in res_tax[rownames(res_tax_sig), "OTU"]){
  tmp <- data.frame(data[, i], groups, rep(paste(i, " padj = ", round(res_tax[i, "padj"], 5), sep=""), dim(data)[1]))
  if(is.null(df)){df <- tmp} else { df <- rbind(df, tmp)} 
}
colnames(df) <- c("Value", "Type", "Taxa")
 
#==============================================================================
# Plots
#==============================================================================
p <- ggplot(df, aes(Type, Value, colour=Type))+ylab("Log-relative normalised")
p <- p+geom_boxplot()+geom_jitter()+theme_bw()+
  facet_wrap( ~ Taxa ,  scales="free",  ncol=3)
p <- p+theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
pdf("NB_significant.pdf", width=10, height=14)
print(p)
dev.off()