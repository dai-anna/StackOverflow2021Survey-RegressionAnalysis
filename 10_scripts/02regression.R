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

df$US_State

factor_cols = c("US_State", "EdLevel", "OrgSize", "Age", "Gender", "Trans","Sexuality", "Ethnicity", "Accessibility", "MentalHealth") 
df[factor_cols] <- lapply(df[factor_cols], factor)

################################################################################
################################## 1. EDA ######################################
################################################################################


################ Explore Logistic Regression on "Leader" #################


cond_prob <- function (df, col1, col2) {
  round(apply(table(df[, c(col1, col2)])/sum(table(df[, c(col1, col2)])),
              2,function(x) x/sum(x)), 2)
}


df$DevType


cond_prob(df, "EdLevel",  "Leader")
chisq.test(table(df[,c("EdLevel", "Leader")]))


ggplot(data, aes(x=re78)) + geom_histogram() # skewed to the right

