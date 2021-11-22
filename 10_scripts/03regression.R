rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(magrittr)
library(kableExtra)
library(car)
library(lme4)
library(arrow)


################################################################################
############################### 0. LOADING THE DATA ############################
################################################################################

df = readRDS("../20_intermediate_files/clean_USdata.rds")

head(df)
str(df)
dim(df)
summary(df)
colnames(df)

# factor_cols = c("US_State", "EdLevel", "OrgSize", "Age", "Gender", "Trans","Sexuality", "Ethnicity", "Accessibility", "MentalHealth") 
# df[factor_cols] <- lapply(df[factor_cols], factor)

################################################################################
################################## 1. EDA ######################################
################################################################################

################### EXPLORE LOGISTIC REGRESSION ON "LEADER" ####################
# 
# cond_prob <- function (df, col1, col2) {
#   round(apply(table(df[, c(col1, col2)])/sum(table(df[, c(col1, col2)])),
#               2,function(x) x/sum(x)), 2)
# }
# 
# cond_prob(df, "EdLevel",  "Senior Executive (C-Suite, VP, etc.)")
# chisq.test(table(df[,c("EdLevel", "Senior Executive (C-Suite, VP, etc.)")]))
# 
# 
# sum(df$"Senior Executive (C-Suite, VP, etc.)")/length(df)
# # 3.21 % of total observations is classified as a "leader"
# # it would require extensive rebalancing to gain meaning results from the dataset
# # also, seeing that we do not have data on individual's progressions, we will choose to not model "leader" but total comp

################## FOCUS ON LINEAR REGRESSION ON COMPENSATION ##################

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
table(df$Age1stCode)

ggplot(data=df, aes(x=EdLevel, y=logConvertedCompYearly)) + geom_boxplot()
table(df$EdLevel) # maybe collapse into less levels

ggplot(data=df, aes(x=OrgSize, y=logConvertedCompYearly)) + geom_boxplot() #.
table(df$OrgSize) # maybe collapse into less levels

ggplot(data=df, aes(x=Currency, y=logConvertedCompYearly)) + geom_boxplot() #?
table(df$Currency) 
#***************^^^^ LOOK INTO THIS ^^^^*********************#

ggplot(data=df, aes(x=Age, y=logConvertedCompYearly)) + geom_boxplot() #***
table(df$Age) # maybe collapse into less levels

ggplot(data=df, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$Gender) 

ggplot(data=df, aes(x=Trans, y=logConvertedCompYearly)) + geom_boxplot() #.
table(df$Trans) 

ggplot(data=df, aes(x=Sexuality, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Sexuality) 

ggplot(data=df, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #**
table(df$Ethnicity) # maybe collapse into less levels

ggplot(data=df, aes(x=Accessibility, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Accessibility)

ggplot(data=df, aes(x=MentalHealth, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$MentalHealth)

# Look at groups of binary predictors


# Look at hierarchical predictor
ggplot(data=df, aes(x=US_State, y=logConvertedCompYearly)) + geom_boxplot() # different -> random intercepts


################################################################################
############################### 2. MODEL SELECTION #############################
################################################################################


################################################################################
#*********************** dropping n/as here & variables ***********************#
dropcols = c("LearnCode","YearsCode","YearsCodePro","DevType")
df[dropcols] = NULL
df = df %>% drop_na()

############################## LINEAR MODEL FITTING ############################

# Model selection
null_model = lm(logConvertedCompYearly ~ Gender + Ethnicity , data=df)
full_model = lm(
  logConvertedCompYearly ~ `EdLevel` +
    `Age1stCode` +
    `OrgSize` +
    `Currency` +
    `Age` +
    `Gender` +
    `Trans` +
    `Sexuality` +
    `Ethnicity` +
    `Accessibility` +
    `MentalHealth` +
    # `YearsCodeNum` + highly correlated with YearsCodeProNum
    `YearsCodeProNum` +
    `Online Resources` +
    `School` +
    `Peers` +
    `Other` +
    `Coding Bootcamp` +
    `Software Development` +
    `Data Science` +
    `Research` +
    `DevOps and Admin` +
    `Product manager` +
    `Senior Executive (C-Suite, VP, etc.)` +
    `Other/Non-Technical`,
  data = df
)
summary(full_model)

#AIC stepwise selection
# step_model = step(null_model, scope = formula(full_model), direction = 'both', trace=0)
# summary(step_model)

#AIC forward selection 
# step_model = step(null_model, scope = formula(full_model), direction = 'forward', trace=0)
# summary(step_model) # similar to stepwise

#AIC backward selection
# step_model = step(null_model, scope = formula(full_model), direction = 'backward', trace=0)
# summary(step_model) # kicks out everything


n=nrow(df)
#BIC stepwise selection
step_model = step(null_model, scope = list(upper=full_model, lower=null_model), direction = 'both', trace=0, k=log(n))
summary(step_model) # more parsimonious than AIC, slightly worse R^2 (0.4) -> CHOSEN ONE
AIC(step_model)

# Transform to original scale
coef = data.frame(exp(step_model$coefficients))
cf = data.frame(exp(confint(step_model)))
summaryprint = round(cbind(coef, cf),4)
summaryprint

# Significant Variables:
# Gender + Ethnicity + YearsCodeProNum + Age + OrgSize + EdLevel + `Senior Executive (C-Suite, VP, etc.)` + `DevOps and Admin` + School + Research + Currency


# #BIC stepwise selection
# step_model = step(null_model, scope = formula(full_model), direction = 'forward', trace=0, k=log(n))
# summary(step_model) # similar to stepwise


############################# LINEAR MODEL ASSESSMENT ##########################

par(bty='n')
plot(step_model, which=1)                       # equal variance and independence assum problem
plot(step_model, which=2)                       # OOOOOUUFFF
plot(step_model, which=3)
plot(step_model, which=4)                       # no influential points?
plot(step_model, which=5)                       # several outliers but not influential
plot(df$YearsCodeNum, step_model$residuals)     # linearity assumption holds
plot(df$YearsCodeProNum, step_model$residuals)
vif(step_model)           

########################### HIERARCHICAL MODEL FITTING #########################

# Level = State (random intercept)
step_model = lm(null_model, scope = list(upper=full_model, lower=null_model), direction = 'both', trace=0, k=log(n))
randint_model <- lmer(logConvertedCompYearly ~ Gender + Ethnicity + YearsCodeProNum + Age + OrgSize + EdLevel + `Senior Executive (C-Suite, VP, etc.)` + `DevOps and Admin` + School + Research + Currency + (1 | US_State), data = df)
summary(randint_model) 
AIC(randint_model)

anova(step_model,randint_model) 
