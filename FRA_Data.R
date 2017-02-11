library(dplyr)
library(GGally)

df <- read.csv('SBA_Loan_data_.csv', header=TRUE, sep=",")

df <- filter(df, LoanStatus != "CANCLD" & LoanStatus != "EXEMPT")

# Creating default column
df$DidDefault <- ifelse(df$LoanStatus=="CHGOFF", 1, 0)

## Include Macro-Economic Data 
year_approval=df$ApprovalFiscalYear
borrower_state=as.vector(df$BorrState)
term_in_months=df$TermInMonths
n=length(year_approval)

unemployment_data=read.csv("emp-unemployment.csv")
unemployment_rates=c(rep(0,n))

for (i in 1:n) {
  unemployment_rates[i]=unemployment_data[match(borrower_state[i],unemployment_data[,3]),match(year_approval[i],unemployment_data[1,])]
}

#FED Interest Rate data per year (Source: https://fred.stlouisfed.org/series/DFF/downloaddata)

interest_rates_data=read.csv("FED_IR.csv")
FED_interest_rates=c(rep(0,n))

for (i in 1:n) {
  FED_interest_rates[i]=interest_rates_data[match(year_approval[i],interest_rates_data$Year),2]
}

# Consumer Price Index data per year (Source: http://www.seattle.gov/financedepartment/cpi/historical.htm)
CPI_data= read.csv("US_CPI.csv")
CPI=c(rep(0,n))

for (i in 1:n) {
  CPI[i]=CPI_data[match(year_approval[i],CPI_data$Yeatar),2]
}

#Total Economic Cost due to Tornados per year per state  http://www.spc.noaa.gov/wcm/test.html (manipulated using R and excel)

Tornado_damage_economic=read.csv("Tornado FL.csv")

#Small Business Lending data stats per state for 2011 (https://www.sba.gov/advocacy/firm-size-data) and https://www.sba.gov/content/small-business-lending-united-states-2010-2011

Small_business_lending_data=read.csv("Small_Business_lending_stats.csv")
SBLR=c(rep(0,n))

for (i in 1:n) {
  SBLR[i]=Small_business_lending_data[match(df$BorrState[i],Small_business_lending_data$State),5]
}
