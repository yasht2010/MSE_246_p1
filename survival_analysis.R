# This file will do a hazard model-survival analysis on the SBA loan dataset 

# Authors: Daniel, Brent, Peng and Yash

# File Description: 
# Inputs: 
# Outputs: 

library(dplyr)
library(GGally)
library(survival)
library(ROCR)
set.seed(1)

df <- read.csv("SBA_cleaned_data.csv")

# Creation of Train, Validation and Test set for Time Series Sampling
df.train.ts <- filter(df,ApprovalFiscalYear <= 2002)
df.valid.ts <- filter(df,ApprovalFiscalYear > 2002 & ApprovalFiscalYear <= 2006)
df.test.ts <- filter(df,ApprovalFiscalYear>2006)

# Creation of Training Set for Random Sampling

train.random.rows <- sample(nrow(df),floor(0.7*nrow(df)))
df.train.random <- df[train.random.rows,]

df.valid.test.random <- df[-train.random.rows,]
valid.random.rows <- sample(nrow(df.valid.test.random),floor(0.2*nrow(df)))

# Creation of Valid and Test set for Random Sampling

df.valid.random <- df.valid.test.random[valid.random.rows,]
df.test.random <- df.valid.test.random[-valid.random.rows,]

#### 

glm.fit <- glm((isDefault-1) ~ GrossApproval*TermInMonths+
                 GrossApproval*DeliveryMethod+Naics2digits,data = df.train.ts,family="binomial")
summary(glm.fit)


glm.prob.val <- predict(glm.fit,newdata = df.valid.ts,type = "response")
glm.pred.val <- rep(0,nrow(df.valid.ts))
glm.pred.val[glm.prob.val>0.2] <- 1
glm.pred.val <- as.factor(glm.pred.val)
table(glm.pred.val,(df.valid.ts$isDefault-1))
mean(glm.pred.val==(df.valid.ts$isDefault-1))

# ROC Curve 
roc <- performance(prediction(glm.prob.val,df.valid.ts$isDefault-1),measure = "tpr",x.measure = "fpr")
plot(roc)

auc <- performance(prediction(glm.prob.val,df.valid.ts$isDefault-1),measure = "auc")
auc <- auc@y.values[[1]]
auc

# Survival Analysis

mod.coxph <- coxph(Surv(dayselapsed,isDefault)~GrossApproval+Naics2digits+DeliveryMethod
                   ,data=df.train.ts)
mod.coxph
mod.coxph.prob = predict(mod.coxph, type = "risk")
mod.coxph.pred = rep(1, nrow(df.train.ts))
mod.coxph.pred[mod.coxph.prob>=2.4] = 2
table(mod.coxph.pred, df.train.ts$isDefault)
mean(mod.coxph.pred==df.train.ts$isDefault)

plot(survfit(mod.coxph),ylim = c(0.9,1))
hist(mod.coxph.prob)

plot(survfit(mod.coxph,newdata = df.test.ts[1:10,]),ylim=c(0.6,1))


## Value at Risk Computation

# Choose portfolio of loans
# Find the predicted default for each of those loans at the end of 1 and 5 years

set.seed(1)

# Get Loss given default distribution
df.train.default <- df.train.ts[df.train.ts$isDefault==2,]
df.valid.default <- df.valid.ts[df.valid.ts$isDefault==2,]
# Inserting the probability of default
df.train.default$probDef <- predict(glm.fit,newdata = df.train.default,type = "response")
df.valid.default$probDef <- predict(glm.fit,newdata = df.valid.default,type = "response")

# Removing isDefault from the default data set
df.train.default <- subset(df.train.default,select = -c(isDefault))
df.valid.default <- subset(df.valid.default,select = -c(isDefault))
# Removing LoanStatus column
df.train.default <- subset(df.train.default,select = -c(LoanStatus))
df.valid.default <- subset(df.valid.default,select = -c(LoanStatus))

model.loss.default <- lm(data = df.train.default,GrossChargeOffAmount~ GrossApproval*TermInMonths+
                           GrossApproval*DeliveryMethod+Naics2digits)

summary(model.loss.default)

df.train.default$predLoss <- predict(model.loss.default,newdata = df.train.default)
df.valid.default$predLoss <- predict(model.loss.default,newdata = df.valid.default)

# Model 1
# Probability of default
# Model 2
# LGD Model
# F(X,y) = P(LGD = y|X)
# Fit model only defaulted loans 

# sampling algorithm (non-parametric approach) - 
# F is a cumulative distribution
# generate a std. uniform variable u
# set L = F-1(u)
# L is a sample from LGD

## For multiple iterations (Monte Carlo sim)
loss <- rep(0,1000)
for(i in 1:1000){
  # Using the distribution for the Loss Given Default, find the expected loss for each loan
  VaRTable$lgd <- sample(lossRatio,nrow(VaRTable),replace = TRUE)
  VaRTable$loss <- VaRTable$lgd*VaRTable$grossApprovedAmount
  # sum the expected loss and save it
  loss[i] <- sum(VaRTable$loss)
}
# Plot the sum of expected loss for each. Find the VaR corresponding to 99%, 99.9% levels
hist(loss)
VaR_99 <- quantile(loss,c(.99))
loanBookSize <- sum(as.numeric(df.valid.ts$GrossApproval))
VaR_99/loanBookSize
