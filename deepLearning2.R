install.packages("h2o")
install.packages("zoo")
install.packages("dplyr")

library(h2o)
library(zoo)
library(dplyr)

rm(list = ls())
localH2O <- h2o.init()

df <- read.csv("big_data_Matrix.csv")
colnames(df)
df <- subset(df, select = -c(X,loanNum,start,stop,ApprovalDate))

# Creation of Train, Validation and Test set for Time Series Sampling
df.train.ts <- filter(df,ApprovalFiscalYear <= 2002)
df.valid.ts <- filter(df,ApprovalFiscalYear > 2002 & ApprovalFiscalYear <= 2006)
df.test.ts <- filter(df,ApprovalFiscalYear>2006)

train_h2o <- as.h2o(df.train.ts)
valid_h2o <- as.h2o(df.valid.ts)
test_h2o <- as.h2o(df.test.ts)

h2o.describe(train_h2o)
h2o.describe(valid_h2o)
h2o.describe(test_h2o)

# getting the column names
y <- "isDefault"
x <- setdiff(names(train_h2o), y)

train_h2o[,y] <- as.factor(train_h2o[,y])
valid_h2o[,y] <- as.factor(valid_h2o[,y])
test_h2o[,y] <- as.factor(test_h2o[,y])

set.seed(1)
model <- h2o.deeplearning(x = x,  # column names for predictors
                          y = y,   # column name for label
                          training_frame = train_h2o, # train data in H2O format
                          validation_frame = valid_h2o, # test data in H2O format
                          distribution = "multinomial", # used for multi-classification 
                          activation = "RectifierWithDropout", # activation function 
                          hidden = c(200), # number of nodes in hidden layers (3 layers with 200 nodes each)
                          input_dropout_ratio = 0.2, # A fraction of the features for each training row to omit from training in order to improve generalization (dimension sampling).
                          l1 = 1e-5, # l1 regularization
                          epochs = 10) # number of iterations

# View the specified parameters of your deep learning model
model@parameters
model

# training set metrics
h2o.performance(model, train = TRUE) 

# validation set metrics
# randomly sampled training points to be used for scoring (the default uses 10,000 points)
h2o.performance(model, valid = TRUE) 

# Get MSE only
h2o.mse(model, valid = TRUE)

# making predictions (in terms of probability)
pred <- h2o.predict(model, newdata = test_h2o)
head(pred)
# converting the prediction object from h2o formato to a data frame
y_hat = as.data.frame(pred)[,1]
head(y_hat)

# calculating classification error
err = 1 - mean(y_hat == df.test.ts$isDefault)
err
xtab <- table(y_hat,df.test.ts$isDefault)
xtab

save.image("~/Dropbox/MS&E 246/model200x1.RData")

h2o.shutdown(prompt = FALSE)
