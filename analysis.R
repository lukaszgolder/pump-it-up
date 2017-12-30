library(randomForest)
library(caret)

source("R/functions.R")
source("R/utilities.R")

train_values <- read.csv(train_values_url)
train_labels <- read.csv(train_labels_url)
test_values  <- read.csv(test_values_url)

train <- merge(train_labels, train_values)
train <- featureEngineering(train)

set.seed(42)

model <- randomForest(
  as.factor(status_group) ~ longitude + latitude + extraction_type_group + quality_group + quantity + waterpoint_type + construction_year + install_3 + population,
  data = train,
  importance = TRUE,
  ntree = 50,
  nodesize = 1
)

prediction_train <- predict(model, train)

importance(model)
confusionMatrix(prediction_train, train$status_group)

test <- featureEngineering(test_values)

prediction_test <- predict(model, test)

submission <- data.frame(test$id)
submission$status_group <- prediction_test
names(submission)[1] <- "id"

write.csv(submission, output_url, row.names=FALSE)

# Accuracy : 0.8354
# Accuracy : 0.8411 - Added install_3
# Accuracy : 0.8557 - Increased ntree to 50
# Accuracy : 0.8612 - Decreased nodesize to 1
# Accuracy : 0.9351 - Added population
