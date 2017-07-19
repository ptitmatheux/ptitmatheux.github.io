
# ---------------------------------------------------------------------------
# loading and cleaning data:

modeling <- read.csv("data/pml-training.csv")
predicting <- read.csv("data/pml-testing.csv")

# checking for NA and empty values:
check.NA.model <- apply(modeling, MARGIN=2, FUN=function(COL) { sum(is.na(COL)) })
print(table(check.NA.model))
check.empty.model <- apply(modeling, MARGIN=2, FUN=function(COL) { sum(COL == "") })
print(table(check.empty.model))
# removing those columns from modeling set:
modeling <- subset(modeling, select=names(which(check.empty.model == 0))) # this removes also the NA values

#--------------------------------------------------------------------------
# exploratory analysis:
library(ggplot2)
CHECK <- modeling[modeling$user_name == "carlitos",]
qq <- qplot(raw_timestamp_part_1, pitch_belt, colour=classe, data=CHECK)
qq
#--------------------------------------------------------------------------
check.NA.pred <- apply(predicting, MARGIN=2, FUN=function(COL) { sum(is.na(COL)) })
print(table(check.NA.pred))
check.empty.pred <- apply(predicting, MARGIN=2, FUN=function(COL) { sum(COL == "") })
print(table(check.empty.pred))
# removing those columns from testing set:
predicting <- subset(predicting, select=names(which(check.empty.pred == 0))) # this removes also the NA values

# one checks if the remaining variables are all the same; only the last one differ
# this is normal as the testing set does not contain the output variable "classe"
which(names(modeling) != names(predicting))

submodeling <- subset(modeling, select=-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))
subpredicting <- subset(predicting, select=-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))

# ---------------------------------------------------------------------------
# preparing data:
library(caret)

set.seed(1234)
#trainIndex = createDataPartition(modeling$classe, p = 0.6, list=FALSE)
trainIndex = createDataPartition(submodeling$classe, p = 0.6)[[1]]
training = submodeling[trainIndex,]
testing = submodeling[-trainIndex,]

# subtraining <- subset(training, select=-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))
# subtesting <- subset(testing, select=-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))

# ---------------------------------------------------------------------------
# preprocessing:
# preproc <- preProcess(training, method="pca", thresh=0.9) # thresh: setting amount of explained variance
# preproc$rotation

# ---------------------------------------------------------------------------
# training:
#library(mlr)
library(doMC)
registerDoMC(cores=4)

fit.rf <- train(classe ~ ., method="rf",
                preProcess="pca",
                trControl=trainControl(preProcOptions = list(thresh=0.9)),
                data=training)

#fit.rf$preProcess$rotation

test.rf <- predict(fit.rf, testing)
confusionMatrix(test.rf, testing$classe)
#confMat <- confusionMatrix(test.rf, testing$classe)$table
#confMatNorm <- t(t(confMat)/as.vector(table(testing$classe)))
#apply(confMatNorm, MARGIN=2, sum)

pred.rf <- predict(fit.rf, subpredicting)

# fit.tree <- train(classe ~ . - X - user_name - raw_timestamp_part_1 - raw_timestamp_part_2 - cvtd_timestamp - new_window - num_window,
#                   method="rpart",
#                   data=training)
# pred.tree <- predict(fit.tree, testing)
# confusionMatrix(pred.tree, testing$classe)


#pred.rf = B A B A A E D B A A B C B A E E A B B B # this is the correct one!










