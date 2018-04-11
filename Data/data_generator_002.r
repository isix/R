dataset <- data.frame(category = rep(LETTERS[1:5], 100),
    x = rnorm(500, mean = rep(1:5, 100)),
    y = rnorm(500, mean = rep(1:5, 100)))
dataset$fCategory <- factor(dataset$category)
subdata <- subset(dataset, category %in% c("A", "D", "E"))
