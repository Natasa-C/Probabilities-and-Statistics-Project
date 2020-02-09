# Import Data
data("trees")
head(trees)
str(trees)

# Find Mean for trees
result.mean.girth <- mean(trees$Girth)
result.mean.height <- mean(trees$Height)
result.mean.volume <- mean(trees$Volume)

# Prints the results of means for trees
cat("Mean(girth) = ", result.mean.girth, "\n")
cat("Mean(height) = ", result.mean.height, "\n")
cat("Mean(volume) = ", result.mean.volume, "\n")

# Find Median for trees
result.median.girth <- median(trees$Girth)
result.median.height <- median(trees$Height)
result.median.volume <- median(trees$Volume)

# Prints the results of medians for trees
cat("Median(girth) = ", result.median.girth, "\n")
cat("Median(height) = ", result.median.height, "\n")
cat("Median(volume) = ", result.median.volume, "\n")

# Find Variance for trees
result.variance.girth <- var(trees$Girth)
result.variance.height <- var(trees$Height)
result.variance.volume <- var(trees$Volume)

# Prints the results of variances for trees
cat("Variance(girth) = ", result.variance.girth, "\n")
cat("Variance(height) = ", result.variance.height, "\n")
cat("Variance(volume) = ", result.variance.volume, "\n")

# Find Quantile for trees
result.quantile.girth <- quantile(trees$Girth)
result.quantile.height <- quantile(trees$Height)
result.quantile.volume <- quantile(trees$Volume)

# Prints the results of quantile for trees
cat("Quantile(girth) = ", result.quantile.girth, "\n")
cat("Quantile(height) = ", result.quantile.height, "\n")
cat("Quantile(volume) = ", result.quantile.volume, "\n")

# Plot the chart
par(mfrow = c(2, 2))

#boxplot trees girth: no outliers
boxplot(trees$Girth, main="Girth of trees: no outliers", col = "green")

#boxplot trees height: no outliers
boxplot(trees$Height, main="Height of trees: no outliers", col = "green")

#boxplot volume height: few outliers
boxplot(trees$Volume, main="Volume of trees: few outliers", col = "green")

