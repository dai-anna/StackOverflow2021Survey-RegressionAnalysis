rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(magrittr)
library(kableExtra)
library(mice)
library(VIM)
library(lattice)
library(dplyr)
library(data.table)



################################################################################
############################### 0. Loading the data ############################
################################################################################

df = readRDS("../20_intermediate_files/clean_USdata.rds")

head(df)
str(df)
dim(df)
summary(df)
colnames(df)

sapply(df, class)

# confirm my response variable looks clean and log transform looks good
hist(df$ConvertedCompYearly)
hist(df$logConvertedCompYearly)

# drop unused columns
dropcols = c("LearnCode","YearsCode","YearsCodePro","DevType", "logConvertedCompYearly")
df[dropcols] = NULL

################################################################################
######################## O. Evaluating the missing data ########################
################################################################################

# rename columns to remove spaces for mice... cries
df <- df %>%
  rename(OnlineResources = `Online Resources`,
         Bootcamp = `Coding Bootcamp`,
         SoftwareDevelopment = `Software Development`,
         DataScience = `Data Science`,
         DevOps_Admin = `DevOps and Admin`,
         ProductManager = `Product manager`,
         SeniorExecutive = `Senior Executive (C-Suite, VP, etc.)`,
         NonTechnicalRole = `Other/Non-Technical`,
         OtherMethods = Other
  )


############################# Examine N/As #####################################
df_eval = df
all_predictors = df_eval %>% colnames
all_predictors = all_predictors[(all_predictors != "ConvertedCompYearly") & (all_predictors != "US_State")]; all_predictors

sapply(df_eval, function(z)
  (sum(is.na(z)) / length(z)))*100
sapply(df_eval, function(z)
  sum(is.na(z)))

####### Logistic regression on each variable as logistic missing or not ########

df_eval = df #reset
all_predictors = df_eval %>% colnames
all_predictors = all_predictors[(all_predictors != "ConvertedCompYearly") & (all_predictors != "US_State")]; all_predictors

predictor =  "Gender" # <<<<<<<<<<<< CHANGE VARIABLES HERE TO EVALUATE
names(df_eval)[names(df_eval) == predictor] = "predictor"
predictors = all_predictors[all_predictors != predictor]; predictors; length(predictors)
df$response = is.na(df_eval$predictor)
eval_formula = as.formula(paste("response ~",
                 paste(unlist(predictors),collapse=" + "))); eval_formula
eval = glm(eval_formula, data = df_eval, family = binomial())
summary(eval)

df_eval$response = is.na(df_eval$Age1stCode)

# NOTE: checked all model but most models above did not converge or had mostly close to 1 p-scores
# there were a few that other predictors were able to predict, suggesting missing at random

# these are evaluated manually for patterns as there may be too few missing data to predict
df[is.na(df_eval$EdLevel),]
df[is.na(df_eval$Ethnicity),]
df[is.na(df_eval$Gender),]
df[is.na(df_eval$YearsCodeProNum),]

# the below are 100% correlated in terms of missingness because they came from the same survey question
df[is.na(df_eval$SeniorExecutive),]
df[is.na(df_eval$DataScience),]

# will be able to proceed with imputations

################################################################################
############################## 1. Imputing the data ############################
################################################################################

# limit df to the 50 states + DC and change N/A to "Not Provided" - cannot impute
df$US_State = as.character(df$US_State)
df = df[!(df$US_State == "I do not reside in the United States" |
            df$US_State == "Guam" |
            df$US_State == "Puerto Rico"), ]
table(df$US_State)

df[is.na(df$US_State),"US_State"] = "Not Provided"
df[is.na(df$US_State),"US_State"] 
df$US_State = as.factor(df$US_State)

# set to not impute US_State
m0 <- mice(df, maxit=0)
meth <- m0$method
meth[names(meth) %in% c("US_State")] <- ""
pred <- m0$predictorMatrix
pred[, colnames(pred) %in% c("US_State")] <- 0   

# norm (bayesian) and ppm imputations & save to disk
IWANTTOWAIT30MIN = FALSE
if(IWANTTOWAIT30MIN == TRUE){
  imputed_norm = mice(df,
                      m=5,
                      defaultMethod=c("norm","logreg","polyreg","polr"),
                      print=F, 
                      predictorMatrix=pred, 
                      method=meth)
  saveRDS(imputed_norm, "../20_intermediate_files/imputed_norm.rds")
}

IWANTTOWAITANOTHER30MIN = FALSE
if(IWANTTOWAITANOTHER30MIN == TRUE){
  imputed_pmm = mice(df,
                     m=5,
                     defaultMethod=c("pmm","logreg","polyreg","polr"),
                     print=F, 
                     predictorMatrix=pred, 
                     method=meth)
  saveRDS(imputed_pmm, "../20_intermediate_files/imputed_ppm.rds")
}


################################################################################
########################## 2. Assessing the imputations ########################
################################################################################


# Assess norm imputations
imputed_norm = readRDS("../20_intermediate_files/imputed_norm.rds")
stripplot(imputed_norm, col=c("grey","#061953"),pch=c(1,20)) # this is fine, too slow to rerun
densityplot(imputed_norm) # this is fine, too slow to rerun

xyplot(
  imputed_norm,
  ConvertedCompYearly ~ YearsCodeProNum |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "#061953")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ Gender |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "#061953")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ Ethnicity |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "#061953")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ OrgSize |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "#061953")
)


# Assess pmm imputations
imputed_pmm = readRDS("../20_intermediate_files/imputed_ppm.rds")
# stripplot(imputed_pmm, col=c("grey","darkred"),pch=c(1,20)) # this is fine, too slow to rerun
# densityplot(imputed_pmm) # this is fine, too slow to rerun

xyplot(
  imputed_pmm,
  ConvertedCompYearly ~ YearsCodeProNum |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_pmm,
  ConvertedCompYearly ~ Gender |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_pmm,
  ConvertedCompYearly ~ Ethnicity |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_pmm,
  ConvertedCompYearly ~ OrgSize |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)


# Overall I find my imputations look good and representative of my data
