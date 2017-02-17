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

mod.coxph <- coxph(Surv(dayselapsed,isDefault)~GrossApproval,data=df.train.ts)
mod.coxph
mod.coxph.prob = predict(mod.coxph, type = "risk")
mod.coxph.pred = rep(0, nrow(df.train.ts))
mod.coxph.pred[mod.coxph.prob>0.5] = 1
mod.coxph.prob = as.factor(mod.coxph.pred)
table(mod.coxph.pred, df.train.ts$isDefault)
mean(mod.coxph.pred==df.train.ts$isDefault)
