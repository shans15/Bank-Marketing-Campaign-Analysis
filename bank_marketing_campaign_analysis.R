library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)

# 1(a)
data.raw = read.csv("C:/Users/sarth/Downloads/Bank_Marketing_Response_Updates.csv")
str(data.raw)

#1(b)
colSums(is.na(data.raw))

#1(c)
# Convert categorical variables to factors
data.raw$job <- as.factor(data.raw$job)
data.raw$marital <- as.factor(data.raw$marital)
data.raw$education <- as.factor(data.raw$education)
data.raw$default <- as.factor(data.raw$default)
data.raw$housing <- as.factor(data.raw$housing)
data.raw$loan <- as.factor(data.raw$loan)
data.raw$contact <- as.factor(data.raw$contact)
data.raw$month <- as.factor(data.raw$month)
data.raw$poutcome <- as.factor(data.raw$poutcome)
data.raw$outcome <- as.factor(data.raw$outcome)

# Convert numerical variables to numeric
data.raw$ID <- as.numeric(data.raw$ID)
data.raw$age <- as.numeric(data.raw$age)
data.raw$balance <- as.numeric(data.raw$balance)
data.raw$day <- as.numeric(data.raw$day)
data.raw$campaign <- as.numeric(data.raw$campaign)
data.raw$pdays <- as.numeric(data.raw$pdays)
data.raw$previous <- as.numeric(data.raw$previous)
# Print the summary of the dataset 
summary(data.raw)

#1(d)
#1.Campaign success rate
campaign_success <- data.raw %>%
  group_by(day) %>%
  summarize(success_rate = sum(outcome == "yes") / n())
ggplot(campaign_success, aes(x = day, y = success_rate)) +
  geom_line() +
  labs(title = "Campaign Success Rate Over Time", x = "Day of Month", y = "Success Rate")

#2.Contact method success rate
contact_success <- data.raw %>%
  group_by(contact) %>%
  summarize(success_rate = sum(outcome == "yes") / n())
ggplot(contact_success, aes(x = contact, y = success_rate, fill = contact)) +
  geom_bar(stat = "identity") +
  labs(title = "Campaign Success Rate by Contact Method", x = "Contact Method", y = "Success Rate")

#3.Job type and success rate
job_success <- data.raw %>%
  group_by(job) %>%
  summarize(success_rate = sum(outcome == "yes") / n())
ggplot(job_success, aes(x = success_rate, y = job)) +
  geom_col() +
  labs(title = "Campaign Success Rate by Job Type", x = "Success Rate", y = "Job Type")

# 1(e)
set.seed(2023)
spl = sample.split(data.raw$outcome, SplitRatio = 0.7)
train = data.raw %>% filter(spl == TRUE)
test = data.raw %>% filter(spl == FALSE)

# Print the frequency distribution of the outcome variable in both train and test data
table(train$outcome)
table(test$outcome)

#1(f)
# Train a logistic regression model
model <- glm(outcome ~ ., data = train, family = "binomial")
# View the summary of the model
summary(model)

#1(g)
# Confusion matrix for Train data
probability <- predict(model, newdata = train, type = "response")
predictions <- ifelse(probability >= 0.20, 1, 0)

table.output = table(train$outcome, predictions)

tn = table.output[1,1]
fp = table.output[1,2]
fn = table.output[2,1]
tp = table.output[2,2]

#Accuracy:
(tp+tn)/(tp+tn+fp+fn)
#True Postive Rate
tp/(tp+fn)
#False Postive Rate
fp/(fp+tn)

# Confusion matrix for Test data
probability <- predict(model, newdata = test, type = "response")
predictions <- ifelse(probability >= 0.20, 1, 0)

table.output = table(test$outcome, predictions)

tn = table.output[1,1]
fp = table.output[1,2]
fn = table.output[2,1]
tp = table.output[2,2]

# Accuracy:
(tp+tn)/(tp+tn+fp+fn)
#True Postive Rate
tp/(tp+fn)
#False Postive Rate
fp/(fp+tn)

#1(h)
#ROCR Plot Train
probability <- predict(model, newdata = train, type = "response")
pred <- prediction(probability, train$outcome)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc_train <- performance(pred, measure = "auc")@y.values[[1]]
cat("Train AUC:", auc_train, "\n")


#ROCR Plot Test
probability <- predict(model, newdata = test, type = "response")
pred <- prediction(probability, test$outcome)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc_test <- performance(pred, measure = "auc")@y.values[[1]]
cat("Test AUC:", auc_test, "\n")


#2(a)
#Decision tree model
tree = rpart(outcome ~ ., data = train, method = "class")
rpart.plot(tree)

#2(b)
# Create rpart control object
control <- rpart.control(minbucket = 250)

# Fit decision tree model with specified control object
myTree <- rpart(outcome ~ ., data = train, method = "class", control = control)

# Plot decision tree
rpart.plot(myTree)

#2(c)
#Plotted the graph

#2(d)
# Predict probability of campaign offer acceptance for training data
train_prob <- predict(myTree, train, type = "prob")[,2]

# Predict probability of campaign offer acceptance for test data
test_prob <- predict(myTree, test, type = "prob")[,2]

# Create ROC curve for training data
train.pred <- prediction(train_prob, train$outcome)
train.perf <- performance(train.pred, "tpr", "fpr")
plot(train.perf)
train.auc <- performance(train.pred, "auc")@y.values[[1]]
cat("Training AUC:", train.auc, "\n")

# Create ROC curve for test data
test.pred <- prediction(test_prob, test$outcome)
test.perf <- performance(test.pred, "tpr", "fpr")
plot(test.perf)
test.auc <- performance(test.pred, "auc")@y.values[[1]]
cat("Test AUC:", test.auc, "\n")

