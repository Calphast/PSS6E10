library(tidyverse)
library(data.table)
library(mlr)
library(h2o)

# data intake
airline.test <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/test.csv")
airline.train <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/train.csv")
airline.submit <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/sample_submission.csv")

## Data optimization
# data inspecting
setDT(airline.test)
setDT(airline.train)

dim(airline.test)
dim(airline.train)
str(airline.test)
str(airline.train)

# fixing colnames and factoring data
setnames(airline.train, make.names(names(airline.train)))
setnames(airline.test,  make.names(names(airline.test)))

cols <- c("Gender", "Customer.Type", "Type.of.Travel", "Class")

airline.train[, (cols) := lapply(.SD, factor), .SDcols = cols]
airline.test[,  (cols) := lapply(.SD, factor), .SDcols = cols]

# checking for missingness
table(is.na(airline.train))
sapply(airline.train, function(x) (sum(is.na(x))/length(x)) *100)

table(is.na(airline.test))
sapply(airline.test, function(x) (sum(is.na(x))/length(x)) *100)

airline.train <- as.data.frame(airline.train)
airline.test  <- as.data.frame(airline.test)

# imputing null
imp1 <- impute(airline.train, target = "satisfaction",
               cols = list(Arrival.Delay.in.Minutes = imputeMedian()))
airline.train <- imp1$data
airline.test <- reimpute(airline.test, imp1$desc)

## modeling
# create task
# removing ids so that it doesn't treat it as a predictor
airline.train$satisfaction <- factor(airline.train$satisfaction)

test_ids <- airline.test$id
airline.train$id <- NULL
airline.test$id  <- NULL

task <- makeClassifTask(data = airline.train, target = "satisfaction")

traintask <- makeClassifTask(data=airline.train, target = "satisfaction")

# creating learned
bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)

# tuning parameters
params <- makeParamSet(
  makeIntegerParam("mtry",     lower = 2,  upper = 10),
  makeIntegerParam("nodesize", lower = 1,  upper = 50),
  makeIntegerParam("ntree",    lower = 100, upper = 500)
)
ctrl <- makeTuneControlRandom(maxit = 20)   


#set 5 fold cross validation
rdesc <- makeResampleDesc("CV",iters=5L)

lrn <- makeLearner("classif.randomForest", predict.type = "prob")

set.seed(123)
library(parallelMap)
parallelStartSocket(cpus = parallel::detectCores())
tuned <- tuneParams(learner = lrn, task = task, resampling = rdesc,
                    measures = auc, par.set = params, control = ctrl,
                    show.info = TRUE)
parallelStop()
tuned$x   
tuned$y   

# create the randomtree learner
lrn_tuned <- setHyperPars(lrn, par.vals = tuned$x)
model <- train(lrn_tuned, task)

pred <- predict(model, newdata = airline.test)
prob_sub <- getPredictionProbabilities(pred)
