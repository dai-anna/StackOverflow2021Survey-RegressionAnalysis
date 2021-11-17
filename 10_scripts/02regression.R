rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(magrittr)
library(kableExtra)

################################################################################
############################### 0. Loading the data ############################
################################################################################

df = readRDS("../20_intermediate_files/clean_data_set.rds")

head(df)
str(df)
dim(df)
summary(df)
colnames(df)

factor_cols = c("US_State", "EdLevel", "OrgSize", "Age", "Gender", "Trans","Sexuality", "Ethnicity", "Accessibility", "MentalHealth") 
df[factor_cols] <- lapply(df[factor_cols], factor)

################################################################################
################################## 1. EDA ######################################
################################################################################


################### Explore Logistic Regression on "Leader" ####################

cond_prob <- function (df, col1, col2) {
  round(apply(table(df[, c(col1, col2)])/sum(table(df[, c(col1, col2)])),
              2,function(x) x/sum(x)), 2)
}

cond_prob(df, "EdLevel",  "Senior Executive (C-Suite, VP, etc.)")
chisq.test(table(df[,c("EdLevel", "Senior Executive (C-Suite, VP, etc.)")]))


sum(df$"Senior Executive (C-Suite, VP, etc.)")/length(df)
# 3.21 % of total observations is classified as a "leader"
# it would require extensive rebalancing to gain meaning results from the dataset
# also, seeing that we do not have data on individual's progressions, we will choose to not model "leader" but total comp

################## Focus on Linear Regression on Compensation ##################

# confirm my response variable looks clean and log transform looks good
hist(df$ConvertedCompYearly)
hist(df$logConvertedCompYearly)



# Look at numeric predictors
hist(df$YearsCodeProNum)
hist(log(df$YearsCodeProNum)) # wow looks great but predictors don't need to be normally distributed

ggplot(data=df, aes(x=YearsCodeProNum, y=logConvertedCompYearly)) + geom_point() #curved trend
       
hist(df$YearsCodeNum) 
hist(log(df$YearsCodeNum)) # not bad
ggplot(data=df, aes(x=YearsCodeNum, y=logConvertedCompYearly)) + geom_point() #curved trend

# Look at factor predictors
ggplot(data=df, aes(x=Age1stCode, y=logConvertedCompYearly)) + geom_boxplot() #*
ggplot(data=df, aes(x=OrgSize, y=logConvertedCompYearly)) + geom_boxplot() #.
ggplot(data=df, aes(x=Age, y=logConvertedCompYearly)) + geom_boxplot() #***
ggplot(data=df, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #?
ggplot(data=df, aes(x=Trans, y=logConvertedCompYearly)) + geom_boxplot() #.
ggplot(data=df, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #?
ggplot(data=df, aes(x=Accessibility, y=logConvertedCompYearly)) + geom_boxplot() #?
ggplot(data=df, aes(x=MentalHealth, y=logConvertedCompYearly)) + geom_boxplot() #?

# Look at groups of binary predictors


# Look at hierarchical predictor
ggplot(data=df, aes(x=US_State, y=logConvertedCompYearly)) + geom_boxplot() # different -> random intercepts




