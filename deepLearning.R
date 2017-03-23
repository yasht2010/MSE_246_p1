install.packages("h2o")
install.packages("zoo")
install.packages("dplyr")
install.packages("ROCR")

library(h2o)
library(zoo)
library(dplyr)
library(ROCR)

rm(list = ls())
localH2O <- h2o.init(nthreads = -1)

df.train.ts <- read.csv("training_set.csv")
df.valid.ts <- read.csv("validation_set.csv")
df.test.ts <- read.csv("test500_set.csv")

colnames(df.test.ts)

# Creation of Train, Validation and Test set for Time Series Sampling
#df.train.ts <- filter(df,ApprovalFiscalYear <= 2002)
#df.valid.ts <- filter(df,ApprovalFiscalYear > 2002 & ApprovalFiscalYear <= 2006)
#df.test.ts <- filter(df,ApprovalFiscalYear>2006)

set.seed(1)

df.train.ts <- subset(df.train.ts, 
                      select = -c(X,loanNum,start,stop,ApprovalDate,
                                  Defaultyn,endIndicator,interestRatebucket))
df.valid.ts <- subset(df.valid.ts, 
                      select = -c(X,loanNum,start,stop,ApprovalDate,
                                  Defaultyn,endIndicator))
df.test.ts <- subset(df.test.ts, 
                     select = -c(X,loanNum,start,stop,ApprovalDate,
                                 Defaultyn,endIndicator))

df.train.ts$GrossApproval <- log(df.train.ts$GrossApproval)
df.train.ts$ThirdPartyDollars <- log(df.train.ts$ThirdPartyDollars)
df.train.ts$hpiState <- log(df.train.ts$hpiState)
df.train.ts$SBLR <- log(df.train.ts$SBLR)

df.valid.ts$GrossApproval <- log(df.valid.ts$GrossApproval)
df.valid.ts$ThirdPartyDollars <- log(df.valid.ts$ThirdPartyDollars)
df.valid.ts$hpiState <- log(df.valid.ts$hpiState)
df.valid.ts$SBLR <- log(df.valid.ts$SBLR)

df.test.ts$GrossApproval <- log(df.test.ts$GrossApproval)
df.test.ts$ThirdPartyDollars <- log(df.test.ts$ThirdPartyDollars)
df.test.ts$hpiState <- log(df.test.ts$hpiState)
df.test.ts$SBLR <- log(df.test.ts$SBLR)


train_h2o <- as.h2o(df.train.ts)
valid_h2o <- as.h2o(df.valid.ts)
test_h2o <- as.h2o(df.test.ts)

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
                          hidden = c(250,250,250), # number of nodes in hidden layers (3 layers with 200 nodes each)
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
pred <- h2o.predict(model, newdata = valid_h2o)
head(pred)
# converting the prediction object from h2o formato to a data frame
y_hat = as.data.frame(pred)[,1]
head(y_hat)

# calculating classification error
err = 1 - mean(y_hat == df.valid.ts$isDefault)
err
xtab <- table(predict = y_hat,truth = df.valid.ts$isDefault)
xtab

pred_log_reg <- as.data.frame(pred)[,3]
df.valid.ts <- read.csv("validation_set.csv")
df.valid.ts$GrossApproval <- log(df.valid.ts$GrossApproval)
df.valid.ts$ThirdPartyDollars <- log(df.valid.ts$ThirdPartyDollars)
df.valid.ts$hpiState <- log(df.valid.ts$hpiState)
df.valid.ts$SBLR <- log(df.valid.ts$SBLR)

loan_indices <- as.data.frame(unique(df.valid.ts$loanNum))
loan_indices$start <- 1
loan_indices$stop <- 1
colnames(loan_indices) <- c("loan_no","start","stop")
l = 1
counter = 1

# find start and stop for each loan
for(i in 1:nrow(df.valid.ts)){
  if(df.valid.ts$endIndicator[i]==1){
    loan_indices$stop[l]=i
    l=l+1
    if(l<=nrow(loan_indices)){
      loan_indices$start[l]=i+1
    }
  }
}

pred_test <- rep(0,length(pred_log_reg))

for(i in 1:nrow(loan_indices))
{
  a = loan_indices$start[i]
  b = loan_indices$stop[i]
  for(j in a:b){
    if(j==a){
      pred_test[a]=pred_log_reg[a]
    }
    else{
      for(k in a:j){
        counter = counter*(1-pred_log_reg[k])
      }
      pred_test[j]=1-counter
    }
    counter = 1
  }
}

pred_test_val <- rep(0,length(pred_log_reg))
pred_test_val[pred_test>0.08] <-1 
table(pred_test_val,df.valid.ts$isDefault)

# for loop over start and stop for each loan

## if i = 1 => Prob of default is just the cond. prob
## if i>1
### for loop from 1 to i-1
#### multiply with survival
### multiply with default in i


#ROC Curve
library(ROCR)
#does not work if no loans defaulted. Only works when there are two classes.
pred1 = prediction(pred_test, df.valid.ts$isDefault)
perf1 = performance(pred1, "tpr", "fpr")
plot(perf1)
auc1 <- performance(pred1,measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1


save.image("~/Dropbox/MS&E 246/mode50x2_scrambled.RData")

h2o.shutdown(prompt = FALSE)

sum(df.test.ts$isDefault)/length(unique(df.test.ts$loanNum))
