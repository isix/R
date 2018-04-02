###############################################################################
# Kruskal-Wallis
#------------------------------------------------------------------------------
# by Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)
# 
###############################################################################

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(reshape)
library(ggplot2)
 
#==============================================================================
# Let's rock'n'roll!
#==============================================================================
# Load the abundance table 
abund_table <- read.csv("SPE_pitlatrine.csv",row.names=1,check.names=FALSE)
 
# Transpose the data to have sample names on rows
abund_table <- t(abund_table)
 
# Get grouping information
grouping_info <- data.frame(row.names=rownames(abund_table),t(as.data.frame(strsplit(rownames(abund_table),"_"))))
 
# Use countries as grouping information
groups <- as.factor(grouping_info[,1])
 
# Apply normalisation (either use relative or log-relative transformation)
# data <- abund_table/rowSums(abund_table)
data <- log((abund_table+1)/(rowSums(abund_table)+dim(abund_table)[2]))
data <- as.data.frame(data)
 
# Reference: http://www.bigre.ulb.ac.be/courses/statistics_bioinformatics/practicals/microarrays_berry_2010/berry_feature_selection.html
kruskal.wallis.alpha=0.01
kruskal.wallis.table  <-  data.frame()
for (i in 1:dim(data)[2]) {
  ks.test  <-  kruskal.test(data[,i], g=groups)
  #  Store the result in the data frame
  kruskal.wallis.table  <-  rbind(kruskal.wallis.table,
                                data.frame(id=names(data)[i],
                                           p.value=ks.test$p.value
                                ))
  #  Report number of values tested
  cat(paste("Kruskal-Wallis test for ",names(data)[i]," ", i, "/", 
            dim(data)[2], "; p-value=", ks.test$p.value,"\n", sep=""))
}
 
 
kruskal.wallis.table$E.value  <-  kruskal.wallis.table$p.value * dim(kruskal.wallis.table)[1]
 
kruskal.wallis.table$FWER  <-  pbinom(q=0, p=kruskal.wallis.table$p.value, 
                                    size=dim(kruskal.wallis.table)[1], lower.tail=FALSE)
 
kruskal.wallis.table  <-  kruskal.wallis.table[order(kruskal.wallis.table$p.value,
                                                   decreasing=FALSE), ]
kruskal.wallis.table$q.value.factor  <-  dim(kruskal.wallis.table)[1] / 1:dim(kruskal.wallis.table)[1]
kruskal.wallis.table$q.value  <-  kruskal.wallis.table$p.value * kruskal.wallis.table$q.value.factor
pdf("KW_correction.pdf")
plot(kruskal.wallis.table$p.value,
     kruskal.wallis.table$E.value,
     main='Multitesting corrections',
     xlab='Nominal p-value',
     ylab='Multitesting-corrected statistics',
     log='xy',
     col='blue',
     panel.first=grid(col='# BBBBBB',lty='solid'))
lines(kruskal.wallis.table$p.value,
      kruskal.wallis.table$FWER,
      pch=20,col='darkgreen', type='p'
)
lines(kruskal.wallis.table$p.value,
      kruskal.wallis.table$q.value,
      pch='+',col='darkred', type='p'
)
abline(h=kruskal.wallis.alpha, col='red', lwd=2)
legend('topleft', legend=c('E-value', 'p-value', 'q-value'), col=c('blue', 'darkgreen','darkred'), lwd=2,bg='white',bty='o')
dev.off()
 
last.significant.element  <-  max(which(kruskal.wallis.table$q.value <= kruskal.wallis.alpha))
selected  <-  1:last.significant.element
diff.cat.factor  <-  kruskal.wallis.table$id[selected]
diff.cat  <-  as.vector(diff.cat.factor)
 
print(kruskal.wallis.table[selected,])
 
#==============================================================================
# Plots
#==============================================================================

# Now we plot taxa significantly different between the categories
df <- NULL
for(i in diff.cat){
  tmp <- data.frame(data[,i],groups,rep(paste(i," q = ",round(kruskal.wallis.table[kruskal.wallis.table$id==i,"q.value"],5),sep=""),dim(data)[1]))
  if(is.null(df)){df <- tmp} else { df <- rbind(df,tmp)} 
}
colnames(df) <- c("Value","Type","Taxa")
 
p <- ggplot(df,aes(Type,Value,colour=Type))+ylab("Log-relative normalised")
p <- p+geom_boxplot()+geom_jitter()+theme_bw()+
  facet_wrap( ~ Taxa , scales="free", ncol=3)
p <- p+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
pdf("KW_significant.pdf",width=10,height=14)
print(p)
dev.off()



