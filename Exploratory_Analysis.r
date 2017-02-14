library(dplyr)
library(GGally)
library(survival)
library(ROCR)
df <- read.csv("SBA_loan_data_new.csv")
set.seed(1)

# Creating a copy of the original dataframe for our operations
df_new <- df

# Macroeconomic data sets 

unemployment_data <- read.csv("emp-unemployment.csv")

# FED Interest Rate data per year (Source: https://fred.stlouisfed.org/series/DFF/downloaddata)
interest_rates_data <- read.csv("FED_IR.csv")

# Total Economic Cost due to Tornados per year per state  http://www.spc.noaa.gov/wcm/test.html (manipulated using R and excel)

Tornado_damage_economic <- read.csv("Tornado FL.csv")

# Small Business Lending data stats per state for 2011 (https://www.sba.gov/advocacy/firm-size-data) and https://www.sba.gov/content/small-business-lending-united-states-2010-2011

Small_business_lending_data <- read.csv("Small_Business_lending_stats.csv")
SBLR <- c(rep(0,length(df_new$BorrState)))

for (i in 1:length(df_new$BorrState)) {
  SBLR[i]=Small_business_lending_data[match(df_new$BorrState[i],Small_business_lending_data$State),5]
}

#Add small business loan ratio (total small business loans issued/total small businesses per state )
df_new=cbind(df_new,SBLR)

# Average HPI Index by State for 1990-2016
hpi_state <- read.csv("hpi_state.csv", header=TRUE, stringsAsFactors=F)
hpi_state$ProjectState = as.factor(hpi_state$ProjectState)
df_new <- left_join(df_new, hpi_state, by=c("ProjectState", "ApprovalFiscalYear"))
df_new$mn_hpi = as.numeric(df_new$mn_hpi)
# Filtering out Loans which are canceled or exempt

df_new <- filter(df_new, LoanStatus != "CANCLD")
df_new <- filter(df_new, LoanStatus != "EXEMPT")
df_new <- filter(df_new, DeliveryMethod != "504REFI")

unique(df_new$LoanStatus)

# Removing the first 3 columns of the data set  as they contain the common program info and a 
# unique identifying name for every business

df_new <- df_new[,-c(1,2,3)]

head(df_new)

# Removing the column BorrZip Code

df_new <- df_new[,-c(3)]

head(df_new)

# Removing CDC street, city, zip

df_new <- df_new[,-c(4,5,6,7)]

# Adding 4 columns - isDefault, NotSameState, ThirdPartyApproved and dayselapsed

df_new$isDefault <- ifelse(df_new$LoanStatus=="CHGOFF",1,0)
df_new$isDefault <- factor(df_new$isDefault)
df_new$isDefault <- as.numeric(df_new$isDefault)
df_new$NotSameState <- factor(df_new$NotSameState)
df_new$ThirdPartyApproved <- factor(df_new$ThirdPartyApproved)

df_new <- mutate(df_new,dayselapsed = as.numeric(difftime(strptime(ChargeOffDate, format = "%m/%d/%Y"),
strptime(ApprovalDate, format = "%m/%d/%Y"))))
df_new$dayselapsed[is.na(df_new$dayselapsed)] <- 7300

head(df_new$dayselapsed)

# Removing the Third Party Lender Name, City, State

df_new <- df_new[,-c(4,5,6)]

# Removing subpgmdesc

df_new <- df_new[,-c(9)]

# Removing NAICS description

df_new <- df_new[,-c(12)]

# Removing Project County and State

df_new <- df_new[,-c(12,13)]


# Creation of Train, Validation and Test set for Time Series Sampling
df.train.ts <- filter(df_new,ApprovalFiscalYear <= 2002)
df.valid.ts <- filter(df_new,ApprovalFiscalYear > 2002 & ApprovalFiscalYear <= 2006)
df.test.ts <- filter(df_new,ApprovalFiscalYear>2006)

nrow(df.train.ts)
nrow(df.valid.ts)
nrow(df.test.ts)

# Creation of Training Set for Random Sampling

train.random.rows <- sample(nrow(df_new),floor(0.7*nrow(df_new)))
df.train.random <- df_new[train.random.rows,]

df.valid.test.random <- df_new[-train.random.rows,]
valid.random.rows <- sample(nrow(df.valid.test.random),floor(0.2*nrow(df_new)))

# Creation of Valid and Test set for Random Sampling

df.valid.random <- df.valid.test.random[valid.random.rows,]

df.test.random <- df.valid.test.random[-valid.random.rows,]

nrow(df.train.random)
nrow(df.valid.random)
nrow(df.test.random)

# Logistic Regression

glm.fit <- glm(isDefault ~ GrossApproval*TermInMonths+
                GrossApproval*DeliveryMethod,data = df.train.ts,family="binomial")
summary(glm.fit)

glm.prob.train <- predict(glm.fit,type="response")
glm.pred.train <- rep(0,nrow(df.train.random))
glm.pred.train[glm.prob.train>0.2] <- 1
glm.pred.train <- as.factor(glm.pred.train)
table(glm.pred.train,df.train.random$isDefault)
mean(glm.pred.train==df.train.random$isDefault)

glm.prob.val <- predict(glm.fit,newdata = df.valid.random,type = "response")
glm.pred.val <- rep(0,nrow(df.valid.random))
glm.pred.val[glm.prob.val>0.2] <- 1
glm.pred.val <- as.factor(glm.pred.val)
table(glm.pred.val,df.valid.random$isDefault)
mean(glm.pred.val==df.valid.random$isDefault)

# ROC Curve 
roc <- performance(prediction(glm.prob.val,df.valid.random$isDefault),measure = "tpr",x.measure = "fpr")
plot(roc)

auc <- performance(prediction(glm.prob.val,df.valid.random$isDefault),measure = "auc")
auc <- auc@y.values[[1]]
auc

# Survival Analysis

mod.coxph <- coxph(Surv(dayselapsed,isDefault)~GrossApproval,data=df.train.ts)
mod.coxph
mod.coxph.prob = predict(mod.coxph, type = "risk")
mod.coxph.pred = rep(0, nrow(df.train.ts))
mod.coxph.pred[mod.coxph.prob>0.2] = 1
mod.coxph.prob = as.factor(mod.coxph.pred)
table(mod.coxph.pred, df.train.ts$isDefault)
mean(mod.coxph.pred==df.train.ts$isDefault)

class(df.train.ts$ThirdPartyApproved)
unique(df.train.ts$DeliveryMethod)


