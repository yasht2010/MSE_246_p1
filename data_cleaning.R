# This file will do an initial data cleaning/addition of the SBA loan dataset 

# Authors: Daniel, Brent, Peng and Yash

# File Description: 
# Inputs: 
# Outputs: 

library(dplyr)
library(GGally)
library(survival)
library(ROCR)
set.seed(1)

df <- read.csv("SBA_loan_data_new.csv")

# Macroeconomic data sets 

unemployment_data <- read.csv("emp-unemployment.csv")

# FED Interest Rate data per year 
# Source: https://fred.stlouisfed.org/series/DFF/downloaddata

interest_rates_data <- read.csv("FED_IR.csv")

# Total Economic Cost due to Tornados per year per state 
# Source: http://www.spc.noaa.gov/wcm/test.html (manipulated using R and excel)

Tornado_damage_economic <- read.csv("Tornado FL.csv")

# Small Business Lending data stats per state for 2011 
# Source:
# 1. https://www.sba.gov/advocacy/firm-size-data 
# 2. https://www.sba.gov/content/small-business-lending-united-states-2010-2011

Small_business_lending_data <- read.csv("Small_Business_lending_stats.csv")

SBLR <- c(rep(0,length(df$BorrState)))

for (i in 1:length(df$BorrState)) {
  SBLR[i]=Small_business_lending_data[match(df$BorrState[i],Small_business_lending_data$State),5]
}

#Add small business loan ratio (total small business loans issued/total small businesses per state )
df=cbind(df,SBLR)

# Average HPI Index by State for 1990-2016
hpi_state <- read.csv("hpi_state.csv", header=TRUE, stringsAsFactors=F)
hpi_state$ProjectState = as.factor(hpi_state$ProjectState)
df <- left_join(df, hpi_state, by=c("ProjectState", "ApprovalFiscalYear"))
df$mn_hpi = as.numeric(df$mn_hpi)

# Filtering out Loans which are canceled or exempt

df <- filter(df, LoanStatus != "CANCLD")
df <- filter(df, LoanStatus != "EXEMPT")
df <- filter(df, DeliveryMethod != "504REFI")

# Removing the first 3 columns of the data set  as they contain the common program info and a 
# unique identifying name for every business

df <- df[,-c(1,2,3)]

# Removing the column BorrZip Code

df <- df[,-c(3)]

# Removing CDC street, city, zip

df <- df[,-c(4,5,6,7)]

# Adding 4 columns - isDefault, NotSameState, ThirdPartyApproved and dayselapsed

df$isDefault <- ifelse(df$LoanStatus=="CHGOFF",1,0)
df$isDefault <- factor(df$isDefault)
df$isDefault <- as.numeric(df$isDefault)
df$NotSameState <- factor(df$NotSameState)
df$ThirdPartyApproved <- factor(df$ThirdPartyApproved)

df <- mutate(df,dayselapsed = as.numeric(difftime(strptime(ChargeOffDate, format = "%m/%d/%Y"),
                                                          strptime(ApprovalDate, format = "%m/%d/%Y"))))

df$dayselapsed[is.na(df$dayselapsed)] <- 7300

# Removing the Third Party Lender Name, City, State

df <- df[,-c(4,5,6)]

# Removing subpgmdesc

df <- df[,-c(9)]

# Removing NAICS description

df <- df[,-c(12)]

# Removing Project County and State

df <- df[,-c(12,13)]

df <- mutate(df,Naics2digits = substr(NaicsCode,1,2))


interest_rates=matrix(0,nrow=length(df$ApprovalFiscalYear),ncol=length(1990:2014))

for (i in 1:length(1990:2014)) {
  interest_rates[,i]=interest_rates_data[match(1989+i,interest_rates_data$Year),2]
}

for (i in 1:length(df$ApprovalFiscalYear)){

  if ((round(df$dayselapsed[i]/365)+1990) < 2014) {
    j = 2014-(round(df$dayselapsed[i]/365)+1990)
    column_final=length(1990:(round(df$dayselapsed[i]/365)+1990))
    
    for (n in  column_final:25){
      interest_rates[i,n]==0}
  }
}



# Writing the cleaned dataframe to a csv file

write.csv(df,file = "SBA_cleaned_data.csv")
