n <- 2000
dataset <- data.frame(category = rep(LETTERS[1:5], n),
    x = rnorm(500, mean = rep(1:5, n)),
    y = rnorm(500, mean = rep(1:5, n)))
dataset$fCategory <- factor(dataset$category)
subdata <- subset(dataset, category %in% c("A", "D", "E"))
setwd('C:/Users/Isaias Prestes/Documents/Pesquisas/GitHub/R/Data')
write.table(subdata, file = "Data_IRF_20100312.dat", sep = "\t", row.names = 0)


n <- 2000
dataset <- data.frame(z = rnorm(500, mean = rep(1:5, n)),
    x = rnorm(500, mean = rep(1:5, n)),
    y = rnorm(500, mean = rep(1:5, n)))
subdata <- dataset
setwd('C:/Users/Isaias Prestes/Documents/Pesquisas/GitHub/R/Data')
write.table(subdata, file = "Data_IRF_20100312.dat", sep = "\t", row.names = FALSE)


## License

This project is licensed under the terms of the MIT license, see LICENSE.