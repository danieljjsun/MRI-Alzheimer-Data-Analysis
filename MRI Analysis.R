setwd("C:/Data Analytics/MRI Alzheimer's/")
library(rpart)
library(tidyverse)
library(caret)
library(nnet)
library(coefplot)
install.packages("coefplot")
data <- read.csv("oasis_longitudinal.csv")

# 373 observations
glimpse(data)

new_data <- data %>%
  filter(!is.na(`Subject.ID`) & !is.na(`MRI.ID`) & !is.na(`Group`) & 
           !is.na(`Visit`) & !is.na(`MR.Delay`) & !is.na(`M.F`) & 
           !is.na(`Hand`) & !is.na(`Age`) & !is.na(`EDUC`) & 
           !is.na(`SES`) & !is.na(`MMSE`) & !is.na(`CDR`) & 
           !is.na(`eTIV`) & !is.na(`nWBV`) & !is.na(`ASF`)) %>%
  group_by(Subject.ID)

# 354 observations
nrow(new_data)
# We lost 19 observations not that many

#column name m.f isn't as visually aesthetic
colnames(new_data)[6] <-"Sex"

#check for outliers in data (regression assumption of homoscedasticity)
#z_scores_x1 <- abs(scale(new_data$Subject.ID))
#outliers_x1 <- which(z_scores_x1 > 3)

#z_scores_x2 <- abs(scale(new_data$MRI.ID))
#outliers_x2 <- which(z_scores_x2 > 3)

#z_scores_x3 <- abs(scale(new_data$Group))
#outliers_x3 <- which(z_scores_x3 > 3)

#z_scores_x4 <- abs(scale(new_data$Visit))
#outliers_x4 <- which(z_scores_x4 > 3)

#z_scores_x5 <- abs(scale(new_data$MR.Delay))
#outliers_x5 <- which(z_scores_x5 > 3)

#z_scores_x6 <- abs(scale(new_data$Sex))
#outliers_x6 <- which(z_scores_x6 > 3)

#z_scores_x7 <- abs(scale(new_data$Hand))
#outliers_x7 <- which(z_scores_x7 > 3)

#z_scores_x8 <- abs(scale(new_data$Age))
#outliers_x8 <- which(z_scores_x8 > 3)

#z_scores_x9 <- abs(scale(new_data$EDUC))
#outliers_x9 <- which(z_scores_x9 > 3)

#z_scores_x10 <- abs(scale(new_data$SES))
#outliers_x10 <- which(z_scores_x10 > 3)

#z_scores_x11 <- abs(scale(new_data$MMSE))
#outliers_x11 <- which(z_scores_x11 > 3)

#z_scores_x12 <- abs(scale(new_data$CDR))
#outliers_x12 <- which(z_scores_x12 > 3)

#z_scores_x13 <- abs(scale(new_data$eTIV))
#outliers_x13 <- which(z_scores_x13 > 3)

#z_scores_x14 <- abs(scale(new_data$nWBV))
#outliers_x14 <- which(z_scores_x14 > 3)

#z_scores_x15 <- abs(scale(new_data$ASF))
#outliers_x15 <- which(z_scores_x15 > 3)

#outliers <- c(outliers_x1, outliers_x2, outliers_x3, outliers_x4, outliers_x5, outliers_x6,
#              outliers_x7, outliers_x8, outliers_x9, outliers_x10, outliers_x11, outliers_x12,
#              outliers_x13, outliers_x14, outliers_x15)
#data_clean <- new_data[-outliers, ]

#most of this data is bounded by some scientific constraints so there aren't truly outliers as they're within the range

#create training, validation and testing data

set.seed(1)
trainIndex <- sample(1:nrow(new_data), 0.8 * nrow(new_data))
trainData <- new_data[trainIndex, ]
testData <- new_data[-trainIndex, ]


#response variable needs to be a factor for clm() to work
#################################
model_one <- rpart(Group ~  Sex + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF, data = trainData)

predicted_response <- predict(model_one, newdata = testData, type = "class")

# Create a confusion matrix
confusion <- table(predicted_response, testData$Group)

# Calculate accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy
par(cex = 0.5)
plot(model_one)
# Retrieve the Gini impurity values for each split
gini_impurity <- model_one$cptable[, "xerror"]

text(model_one, use.n = TRUE, all = TRUE, cex = 0.8, split = TRUE, xpd = NA,
     pretty = 0, digits = 2)
var_importance <- varImp(model_one)
print(var_importance)
##########################################################
model_two <- rpart(Group ~  Sex + Age + EDUC + SES + MMSE + eTIV + nWBV + ASF, data = trainData)

predicted_response <- predict(model_two, newdata = testData, type = "class")

# Create a confusion matrix
confusion <- table(predicted_response, testData$Group)

# Calculate accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy
par(cex = 0.5)
plot(model_two)
text(model_two, use.n = TRUE, all = TRUE, cex = 0.8, split = TRUE, xpd = NA, 
     pretty = 0, varlen = 0, prettyline = FALSE, digits = 2)
text(model_two)
###############################################
model_three <- rpart(Group ~  Sex + Age + EDUC + SES + eTIV + nWBV + ASF, data = trainData)

predicted_response <- predict(model_three, newdata = testData, type = "class")

confusion <- table(predicted_response, testData$Group)

# Calculate accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy
par(cex = 0.5)
plot(model_three)
text(model_three, use.n = TRUE, all = TRUE, cex = 0.8, split = TRUE, xpd = NA, 
     pretty = 0, varlen = 0, prettyline = FALSE, digits = 2)
text(model_three)
##########################################
model_four <- rpart(Group ~ Age , data = trainData)

predicted_response <- predict(model_four, newdata = testData, type = "class")

confusion <- table(predicted_response, testData$Group)

# Calculate accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy
par(cex = 0.5)
plot(model_four)
text(model_four, use.n = TRUE, all = TRUE, cex = 0.8, split = TRUE, xpd = NA, 
     pretty = 0, varlen = 0, prettyline = FALSE, digits = 2)
text(model_four)
######################################################
#heatmap
data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
heatmap(data, 
        col = colorRampPalette(c("blue", "white", "red")),  # Set the color scheme
        main = "Heat Map",                                  # Set the title
        xlab = "Columns",                                   # Set the x-axis label
        ylab = "Rows",                                      # Set the y-axis label
        cex.main = 1.5,                                     # Set the title font size
        cex.axis = 1.2,                                     # Set the axis label font size
        cex.lab = 1.2,                                      # Set the axis tick label font size
        key = TRUE)                                         # Show the color key
##################################################
#multinomial logistic regression
log_model <- multinom(Group ~ Age, data = new_data)
summary(log_model)
coefplot(log_model, plot = TRUE)
#this implies that age and dementia are negatively correlated which is not what research says

#we will attempt to check for Simpson's Paradox by splitting the data into groups of 10, then 5

##############################################
#groups of 5

