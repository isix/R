# Exploring ggplot2
library(ggplot2)

# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# color
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# size
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

  
# geom_point ------------------------------------------------------------------
	p <- ggplot(mtcars, aes(wt, mpg))
	p + geom_point()

	# Add aesthetic mappings
	p + geom_point(aes(colour = factor(cyl)))
	p + geom_point(aes(shape = factor(cyl)))
	p + geom_point(aes(size = qsec))

	# Change scales
	p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
	p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)

	# Set aesthetics to fixed value
	ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red", size = 3)


	# Varying alpha is useful for large datasets
	d <- ggplot(diamonds, aes(carat, price))
	d + geom_point(alpha = 1/10)
	d + geom_point(alpha = 1/20)
	d + geom_point(alpha = 1/100)


	# For shapes that have a border (like 21), you can colour the inside and
	# outside separately. Use the stroke aesthetic to modify the width of the
	# border
	ggplot(mtcars, aes(wt, mpg)) +
	  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)


	# You can create interesting shapes by layering multiple points of
	# different sizes
	p <- ggplot(mtcars, aes(mpg, wt, shape = factor(cyl)))
	p + geom_point(aes(colour = factor(cyl)), size = 4) +
	  geom_point(colour = "grey90", size = 1.5)
	p + geom_point(colour = "black", size = 4.5) +
	  geom_point(colour = "pink", size = 4) +
	  geom_point(aes(shape = factor(cyl)))

	# These extra layers don't usually appear in the legend, but we can
	# force their inclusion
	p + geom_point(colour = "black", size = 4.5, show.legend = TRUE) +
	  geom_point(colour = "pink", size = 4, show.legend = TRUE) +
	  geom_point(aes(shape = factor(cyl)))

	# geom_point warns when missing values have been dropped from the data set
	# and not plotted, you can turn this off by setting na.rm = TRUE
	mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
	ggplot(mtcars2, aes(wt, mpg)) + geom_point()
	ggplot(mtcars2, aes(wt, mpg)) + geom_point(na.rm = TRUE)
  
# geom_point and geom_smooth --------------------------------------------------
# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

# Add geom_point() and geom_smooth() with +
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + 
geom_smooth(method = "gam", span = 0.3)

# geom_point(alpha = 0.05) --------------------------------------------------

# 1 - The plot you created in the previous exercise
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

# 2 - Copy the above command but show only the smooth line
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth()


# 3 - Copy the above command and assign the correct value to col in aes()
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_smooth()


# 4 - Keep the color settings from previous command. Plot only the points with argument alpha.
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) + geom_point(alpha = 0.4)

# Object manipulation --------------------------------------------------
# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot <- dia_plot + geom_point()

# Add the same geom layer, but with aes() inside
dia_plot <- dia_plot + geom_point(aes(color = clarity))

dia_plot

# Understanding the grammar ------------------------------------------------
# 1 - The dia_plot object has been created for you
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# 2 - Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(alpha = 0.2)

# 3 - Plot dia_plot with additional geom_smooth() with se set to FALSE
dia_plot + geom_smooth(se = FALSE)

# 4 - Copy the command from above and add aes() with the correct mapping to geom_smooth()
dia_plot + geom_smooth(aes(col = clarity), se = FALSE)

# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------

