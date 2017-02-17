
# This file will do an initial exploratory analysis of the dataset to uncover basic insights 
# into the SBA loan datasets. 

# Authors: Daniel, Brent, Peng and Yash

# File Description: 
# Inputs: 

library(dplyr)
library(GGally)
library(survival)
library(ROCR)
library(ggplot2)

set.seed(1)

df <- read.csv("SBA_cleaned_data.csv")

ggplot(data = df)+stat_summary(mapping = aes(x = ApprovalFiscalYear, y = isDefault))

ggplot(data = df)+stat_summary(mapping = aes(x = BusinessType, y = isDefault))

ggplot(data = df)+stat_summary(mapping = aes(x = ThirdPartyApproved, y = isDefault))

ggplot(data = df)+stat_summary(mapping = aes(x = NotSameState, y = isDefault))

ggplot(data = df)+geom_point(mapping = aes(x = ApprovalFiscalYear, y = GrossApproval))

ggplot(data = df)+stat_summary(mapping = aes(x = ApprovalFiscalYear, y = mn_hpi))

ggplot(data = df)+stat_summary(mapping = aes(x = ApprovalFiscalYear, y = SBLR))

ggplot(data = df)+stat_summary(mapping = aes(x = Naics2digits, y = isDefault))

