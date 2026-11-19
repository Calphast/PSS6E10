library(tidyverse)
library(randomForest)
library(caret)

# data intake
airline.test <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/test.csv")
airline.train <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/train.csv")
airline.submit <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/sample_submission.csv")

# logistic model
airline.col.names <- colnames(airline.train)
print(airline.col.names) 

model.logisitc.1 <- glm(satisfaction ~ Age + `Inflight wifi service`
                      + `Departure/Arrival time convenient` + `Ease of Online booking` 
                      + `Gate location` + `Food and drink` + `Online boarding`
                      + `Seat comfort` + `Inflight entertainment` + `On-board service`
                      + `Leg room service` + `Baggage handling` + `Checkin service`
                      + Cleanliness + `Departure Delay in Minutes` + `Arrival Delay in Minutes`
                      + Gender + `Customer Type` + `Type of Travel` + Class,
                      data = airline.train, family = binomial)
summary(model.logisitc.1)
exp(coef(model.logisitc.1))

car::vif(model.logisitc.1)

model.logisitc.2 <- glm(satisfaction ~ Age + `Inflight wifi service`
                        + `Departure/Arrival time convenient` + `Ease of Online booking` 
                        + `Gate location` + `Food and drink` + `Online boarding`
                        + `Seat comfort` + `On-board service`
                        + `Leg room service` + `Baggage handling` + `Checkin service`
                        + Cleanliness + `Departure Delay in Minutes` + `Arrival Delay in Minutes`
                        + Gender + `Customer Type` + `Type of Travel` + Class,
                        data = airline.train, family = binomial)
summary(model.logisitc.2)
exp(coef(model.logisitc.2))

car::vif(model.logisitc.2)

model.logisitc.3 <- glm(satisfaction ~ Age + `Inflight wifi service`
                        + `Departure/Arrival time convenient` 
                        + `Gate location` + `Food and drink` + `Online boarding`
                        + `Seat comfort` + `On-board service`
                        + `Leg room service` + `Baggage handling` + `Checkin service`
                        + Cleanliness + `Departure Delay in Minutes` + `Arrival Delay in Minutes`
                        + Gender + `Customer Type` + `Type of Travel` + Class,
                        data = airline.train, family = binomial)
summary(model.logisitc.3)
exp(coef(model.logisitc.3))

car::vif(model.logisitc.3)

# random forest model
names(airline.train) <- make.names(names(airline.train))
names(airline.test)  <- make.names(names(airline.test))
names(airline.train)
airline.train$Arrival.Delay.in.Minutes <- NULL
airline.train$satisfaction <- factor(airline.train$satisfaction)
randommodel <- randomForest(satisfaction ~ Age + `Inflight.wifi.service`
                            + `Departure.Arrival.time.convenient` 
                            + `Gate.location` + `Food.and.drink` + `Online.boarding`
                            + `Seat.comfort` + `On.board.service`
                            + `Leg.room.service` + `Baggage.handling` + `Checkin.service`
                            + Cleanliness + `Departure.Delay.in.Minutes`
                            + Gender + `Customer.Type` + `Type.of.Travel` + Class, data = airline.train)
print(randommodel)

names(airline.test)

# predicting from model
prob.randomforest <- predict(randommodel, newdata =airline.test, type = "prob")
prob.logistic <- predict(model.logisitc.3, newdata = airline.test, type = "response")
prob.logistic <- as.numeric(prob.logistic)

print(prob.randomforest)
print(prob.logistic)

# putting together sample submission for randomforest
head(airline.submit)

prob_sub.randomforest <- prob.randomforest[, "TRUE"]
submission1 <- data.frame(id = airline.test$id, satisfaction = prob_sub.randomforest)
write.csv(submission1, "submission1.csv", row.names = FALSE)

# Inspect submission
submission.check <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/submission.csv")

# putting together sample submission for logistic regression
submission2 <- data.frame(id = airline.test$id, satisfaction = prob.logistic)
write.csv(submission2, "submission2.csv", row.names = FALSE)

# Inspect submission
submission2.check <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/submission2.csv")
# move forward with randomforest