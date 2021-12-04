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

sapply(df, function(z)
  (sum(is.na(z)) / length(z)))*100
sapply(df, function(z)
  sum(is.na(z)))

sapply(df, class)

# confirm my response variable looks clean and log transform looks good
hist(df$ConvertedCompYearly)
hist(df$logConvertedCompYearly)

# drop unused columns
dropcols = c("LearnCode","YearsCode","YearsCodePro","DevType", "logConvertedCompYearly")
df[dropcols] = NULL

################################################################################
############################## 1. Imputing the data ############################
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

# limit df to the 50 states + DC
df = df[!(df$US_State == "I do not reside in the United States" |
            df$US_State == "Guam" |
            df$US_State == "Puerto Rico"), ]
df$US_State = droplevels(df$US_State)
table(df$US_State)

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
# stripplot(imputed_norm, col=c("grey","darkred"),pch=c(1,20)) # this is fine, too slow to rerun
# densityplot(imputed_norm) # this is fine, too slow to rerun

xyplot(
  imputed_norm,
  ConvertedCompYearly ~ YearsCodeProNum |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ Gender |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ Ethnicity |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
)
xyplot(
  imputed_norm,
  ConvertedCompYearly ~ OrgSize |
    .imp,
  pch = c(1, 20),
  cex = 1.4,
  col = c("grey", "darkred")
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
