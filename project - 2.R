library(ggplot2)
library(GGally)
library(scatterplot3d)

# Import Data Frame
data("trees")
attach(trees)

# ********************************** Simple Linear Regession ************************************
# Calculate correlation between girth and volume
cor(trees$Girth, Volume)
cat("Correlation between girth and volume = ", cor(trees$Girth, Volume), "\n")

# Calculate correlation between height and volume
cor(trees$Height, Volume)
cat("Correlation between height and volume = ", cor(trees$Height, Volume), "\n")

ggpairs(data=trees, columns=1:3, title="Trees data")
# From looking at the ggpairs() output, girth definitely seems to be related to volume:
# the correlation coefficient is close to 1, and the points seem to have a linear pattern.

# Create Training and Test data
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(trees), 0.8*nrow(trees))  # row indices for training data
trainingData <- trees[trainingRowIndex, ]  # model training data
testData  <- trees[-trainingRowIndex, ]    # test data

# Individual plot for girth-volume corelation
par(mfrow = c(2, 2))
# y = volume of trees (response variable) and x = girth of trees (predictor variable)
plot(jitter(trainingData$Girth), trainingData$Volume,
     pch = 20,
     xlab = "Girth (in)",
     ylab = "Volume (ft3)",
     main = "Simple linear regression Volume vs. Girth of Trees",
     col = "#66CC00")

# For linear regression, choose the biggest correlation: girth-volume correlation
# Build linear model, relating tree volume to girth (LRVG) on training data
LRVG <- lm(Volume ~ Girth, data = trainingData)
modelSummary <- summary(LRVG)
modelSummary
modelCoeffs <- modelSummary$coefficients
intercept <- modelCoeffs["(Intercept)", "Estimate"]
slope <- modelCoeffs["Girth", "Estimate"]
r2 <- modelSummary$adj.r.squared
# r2 closer to 1 represents a well-fitting model

abline(intercept, slope, col="black", lwd = 2)

RSquarelabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 15, y = 52, labels = RSquarelabel)


# ggplot(data = trainingData, aes(x = Girth, y = Volume)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "#66CC00") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Linear Model Fitted to Data")

# The gray shading around the line represents a confidence interval of 0.95.
# This 0.95 confidence interval is the probability that the true linear model for the girth and volume of all
# black cherry trees will lie within the confidence interval of the regression model fitted to our data.

# predict volume for test data
volumePred <- predict(LRVG, testData)  # predict volume
actuals_preds <- data.frame(cbind(actuals=testData$Volume, predicteds=volumePred))  # make actuals_predicteds dataframe
correlation_accuracy <- cor(actuals_preds) # how to interpret this?
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy
mape


# ********************************** Multiple linear regression with Girth and Hight ************************************
# We suspect that tree hight and girth are also correlated
# Calculate correlation between girth and hight
cor(trees$Girth, trees$Height)
cat("Corelation between girth and hight = ", cor(trees$Girth, trees$Height), "\n")

# Build linear model that includes multiple predictor variables (LRVGH) on training data
LRVGH <- lm(Volume ~ Girth + Height, data = trainingData)
modelSummary <- summary(LRVGH)
modelSummary
modelCoeffs <- modelSummary$coefficients
intercept <- modelCoeffs["(Intercept)", "Estimate"]
slopeGirth <- modelCoeffs["Girth", "Estimate"]
slopeHeight <- modelCoeffs["Height", "Estimate"]
r2 <- modelSummary$adj.r.squared
RSquarelabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
# we notice an increase in r2 value => new model fits data better

# to visualize the regression line in 2D run the following command (Height being represented by the intensity of the color)
# ggplot(trainingData, aes(y = Volume, x = Girth, color = Height)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

GirthList <- seq(min(trainingData$Girth), max(trainingData$Girth), by=0.5) ## make a girth vector
HeightList <- seq(min(trainingData$Height), max(trainingData$Height), by=0.5) ## make a height vector
trainingGrid <- expand.grid(Girth = GirthList, Height = HeightList)
## make a grid using the vectors

trainingGrid$Volume <- predict(LRVGH, new = trainingGrid)
LRVGHscatterplot <- scatterplot3d(trainingGrid$Girth, trainingGrid$Height, trainingGrid$Volume, angle = 60, color = "#66CC00", pch = 1, main = "Multiple linear regression with Girth and Hight", sub = RSquarelabel, ylab = "Hight (ft)", xlab = "Girth (in)", zlab = "Volume (ft3)" )
LRVGHscatterplot$points3d(trainingData$Girth, trainingData$Height, trainingData$Volume, pch=16)

# predict volume for test data
volumePred <- predict(LRVGH, testData)  # predict volume
actuals_preds <- data.frame(cbind(actuals=testData$Volume, predicteds=volumePred))  # make actuals_predicteds dataframe
correlation_accuracy <- cor(actuals_preds) # how to interpret this?
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy
mape


# ********************** Multiple linear regression with Girth and Hight, accounting for interactions ********************
# Build linear model that includes multiple predictor variables (LRVGHI) on training data
LRVGHI <- lm(Volume ~ Girth * Height, data = trainingData)
modelSummary <- summary(LRVGHI)
modelSummary
modelCoeffs <- modelSummary$coefficients
intercept <- modelCoeffs["(Intercept)", "Estimate"]
slopeGirth <- modelCoeffs["Girth", "Estimate"]
slopeHeight <- modelCoeffs["Height", "Estimate"]
slopeGH<- modelCoeffs["Girth:Height", "Estimate"]
r2 <- modelSummary$adj.r.squared
RSquarelabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
# we notice an increase in r2 value => new model fits data better

GirthList <- seq(min(trainingData$Girth), max(trainingData$Girth), by=0.5) ## make a girth vector
HeightList <- seq(min(trainingData$Height), max(trainingData$Height), by=0.5) ## make a height vector
trainingGrid <- expand.grid(Girth = GirthList, Height = HeightList)
## make a grid using the vectors

trainingGrid$Volume <- predict(LRVGHI, new = trainingGrid)
LRVGHIscatterplot <- scatterplot3d(trainingGrid$Girth, trainingGrid$Height, trainingGrid$Volume, angle = 60, color = "#66CC00", pch = 1, main = "Multiple linear regression with Girth and Hight, accounting for interactions", sub = RSquarelabel, ylab = "Hight (ft)", xlab = "Girth (in)", zlab = "Volume (ft3)" )
LRVGHIscatterplot$points3d(trainingData$Girth, trainingData$Height, trainingData$Volume, pch=16)

# predict volume
volumePred <- predict(LRVGHI, testData)  # predict volume
actuals_preds <- data.frame(cbind(actuals=testData$Volume, predicteds=volumePred))  # make actuals_predicteds dataframe
correlation_accuracy <- cor(actuals_preds) # how to interpret this?
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy
mape


# ********************** Multiple linear regression with Girth and Defects, accounting for interactions ********************
# auxiliar function to produce a vector having any desired correlation rho with a given vector Y
# https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables
complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

# The added variable tells the wood percentage that cannot be used for timber,
# the percentage of defects the tree has, generated with a normal distribution
randomDefects <- rnorm(31, mean = 2.5 , sd = 2)

girthDefectsCor <- 0.3 # choose desired corelation for Girth - Defects variables

# add new column to trees data frame
# Defects column is generated using randomDefects with a corelation to Girth equal with girthDefectsCorelation
trees <- cbind(trees, data.frame(Defects = as.vector(sapply(girthDefectsCor, function(girthDefectsCor) complement(trees$Girth, girthDefectsCor, randomDefects)))))

# Create new Training and Test data that includes Defects
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(trees), 0.8*nrow(trees))  # row indices for training data
trainingData <- trees[trainingRowIndex, ]  # model training data
testData  <- trees[-trainingRowIndex, ]    # test data   

# regression line for girth defects with a corelation eqaul to girthDefectsCor 
# see how the regression line modifies when changing corelation from 0.3 to 0.9
# ggplot(data = trainingData, aes(x = Girth, y = Defects)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "#66CC00") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Linear Model Fitted for Girth - Defects")


# build multiple linear model for girth defects with interactions
LRVGDI <- lm(Volume ~ Girth * Defects, data = trainingData)
modelSummary <- summary(LRVGDI)
modelSummary
modelCoeffs <- modelSummary$coefficients
intercept <- modelCoeffs["(Intercept)", "Estimate"]
slopeGirth <- modelCoeffs["Girth", "Estimate"]
slopeDefects <- modelCoeffs["Defects", "Estimate"]
slopeGD<- modelCoeffs["Girth:Defects", "Estimate"]
r2 <- modelSummary$adj.r.squared
RSquarelabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

GirthList <- seq(min(trainingData$Girth), max(trainingData$Girth), by=0.5) ## make a girth vector
DefectsList <- seq(min(trainingData$Defects), max(trainingData$Defects), by=0.5) ## make a defects vector
trainingGrid <- expand.grid(Girth = GirthList, Defects = DefectsList)
## make a grid using the vectors

trainingGrid$Volume <- predict(LRVGDI, new = trainingGrid)
LRVGDIscatterplot <- scatterplot3d(trainingGrid$Girth, trainingGrid$Defects, trainingGrid$Volume, angle = 60, color = "#66CC00", pch = 1, main = "Multiple linear regression with Girth and Defects, accounting for interactions", sub = RSquarelabel, ylab = "Defects", xlab = "Girth (in)", zlab = "Volume (ft3)" )
LRVGDIscatterplot$points3d(trainingData$Girth, trainingData$Defects, trainingData$Volume, pch=16)

# Calculate correlation between defects and volume
cor(trees$Defects, Volume)
cat("Correlation between defects and volume = ", cor(trees$Defects, Volume), "\n")

# Calculate correlation between height and volume
cor(trees$Height, Volume)
cat("Correlation between height and volume = ", cor(trees$Height, Volume), "\n")

# ultimul model liniar are un R2 mai mic decat cel obtinut in regresia multipla girth - height cu interactiuni, ceea ce
# este de asteptat avand in vedere ca am ales corelatia dintre defects - girth (= 0.3) mai mica decat corelatia dintre height si girth (= 0.519)
