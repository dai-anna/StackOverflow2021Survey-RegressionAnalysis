rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(magrittr)
library(kableExtra)
library(car)
library(broom.mixed)
library(mice)
library(lme4)
library(arrow)


################################################################################
############################### 0. LOADING THE DATA ############################
################################################################################

imputed_norm = readRDS("../20_intermediate_files/imputed_norm.rds")


# factor_cols = c("US_State", "EdLevel", "OrgSize", "Age", "Gender", "Trans","Sexuality", "Ethnicity", "Accessibility", "MentalHealth") 
# df[factor_cols] = lapply(df[factor_cols], factor)


################################################################################
################################## 1. EDA ######################################
################################################################################

######################## EVALUATE  ONE IMPUTED DATA SET ########################
df = complete(imputed_norm, 1); df # just going to look at the first one
df$logConvertedCompYearly = log(df$ConvertedCompYearly)

head(df)
str(df)
dim(df)
summary(df)
colnames(df)

################### EXPLORE LOGISTIC REGRESSION ON "LEADER" ####################
# 
# cond_prob = function (df, col1, col2) {
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
table(df$EdLevel) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=OrgSize, y=logConvertedCompYearly)) + geom_boxplot() #.
table(df$OrgSize) # maybe collapse into less levels #DONE

ggplot(data=df, aes(x=Currency, y=logConvertedCompYearly)) + geom_boxplot() #?
table(df$Currency) 

ggplot(data=df, aes(x=Age, y=logConvertedCompYearly)) + geom_boxplot() #***
table(df$Age) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$Gender) 

ggplot(data=df, aes(x=Trans, y=logConvertedCompYearly)) + geom_boxplot() #.
table(df$Trans) 

ggplot(data=df, aes(x=Sexuality, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Sexuality) 

ggplot(data=df, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #**
table(df$Ethnicity) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=Accessibility, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Accessibility)

ggplot(data=df, aes(x=MentalHealth, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$MentalHealth)


# Look at hierarchical predictor
ggplot(data=df, aes(x=US_State, y=logConvertedCompYearly)) + geom_boxplot() # different -> random intercepts


################################################################################
############################### 2. MODEL SELECTION #############################
################################################################################

# Drop unused columns
dropcols = c("LearnCode","YearsCode","YearsCodePro","DevType")
df[dropcols] = NULL

df %>% str

# convert logical variables from numeric back to logical
for (i in colnames(df)) {
  if (class(df[[i]]) == "numeric") {
    if (all(df[[i]] == df[[i]]^2)) {
      df[[i]] = as.logical(df[[i]])
    }
  }
}

############################## LINEAR MODEL FITTING ############################
# changing baseline for education level to "Masters" as recommended by Tego
df = df %>% mutate(
  EdLevel = relevel(
    EdLevel,
    "Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)"
  )
)

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
    `OnlineResources` +
    `School` +
    `Peers` +
    `OtherMethods` +
    `Bootcamp` +
    `SoftwareDevelopment` +
    `DataScience` +
    `Research` +
    `DevOps_Admin` +
    `ProductManager` +
    `SeniorExecutive` +
    `NonTechnicalRole`,
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
AIC(step_model) #13300.32

# Transform to original scale
coef = data.frame(exp(step_model$coefficients))
cf = data.frame(exp(confint(step_model)))
summaryprint = round(cbind(coef, cf),4)
summaryprint

# Significant Variables:
# Gender + Ethnicity + YearsCodeProNum + Age + OrgSize + EdLevel + 
# SeniorExecutive + DevOps_Admin + School + Research + OtherMethods

# BIC stepwise selection
# step_model = step(null_model, scope = formula(full_model), direction = 'forward', trace=0, k=log(n))
# summary(step_model) # same as stepwise

################################# INTERACTIONS #################################
# Look at groups of binary predictors
ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Gender, ncol=4)  #*

ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #**

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Ethnicity, ncol=4) #**

ggplot(df, aes(x=Gender, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #**

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Gender, ncol=4) #

ggplot(df, aes(x=Age, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #*

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Age, ncol=4) #*

ggplot(df, aes(x=OrgSize, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #*

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~OrgSize, ncol=4) #*

# Look at predictors by state
# sample 20 states to look at 
# df_sample = df$US_State
ggplot(df, aes(x=Gender, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~US_State, ncol=4) #*

ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~US_State, ncol=4) #*

# Significant Variables:
# Gender + Ethnicity + YearsCodeProNum + Age + OrgSize + EdLevel + 
# SeniorExecutive + DevOps_Admin + School + Research + OtherMethods


interact_model = lm(
  logConvertedCompYearly ~  Gender + 
    Ethnicity + 
    YearsCodeProNum + 
    Age + 
    OrgSize + 
    EdLevel + 
    SeniorExecutive + 
    DevOps_Admin + 
    School + 
    Research + 
    OtherMethods +
    SeniorExecutive:Ethnicity +
    SeniorExecutive:Gender +
    SeniorExecutive:Age +
    SeniorExecutive:OrgSize + 
    Ethnicity:YearsCodeProNum + 
    Gender:YearsCodeProNum +
    OrgSize:YearsCodeProNum,
  data = df
)

stepint_model = step(null_model, scope = list(upper=interact_model, lower=null_model), direction = 'both', trace=0, k=log(n))
summary(stepint_model) #nochange

########################## Pretty table & plot below ###########################

summary_step = summary(stepint_model)
summaryprint = data.frame(summary_step$coefficients)
colnames(summaryprint) = c("Estimate","Std. Error","t value", "Pr(>|t|)")
knitr::kable(summaryprint, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position"))

plot(stepint_model, which=1, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)
plot(stepint_model, which=2, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)
plot(stepint_model, which=5, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)

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

# Level = State (random intercept) ***
step_model = lm(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods,
  data = df
)

randint_model = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods + 
    (1 | US_State),
  data = df
)

randint_summary = summary(randint_model) 
AIC(randint_model) #13033.81

anova(randint_model, step_model) 
xtable(anova(randint_model, step_model))


# Level = State (random intercept + random slope by Ethnicity) NS
randslope_model1 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods + 
    (1 | US_State) + 
    (Ethnicity | US_State),
  data = df
)
anova(randslope_model1, randint_model) 

# Level = State (random intercept + random slope by Gender) NS
randslope_model2 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) +
    (Gender | US_State),
  data = df
)
anova(randslope_model2, randint_model) 

# Level = State (random intercept + random slope by OrgSize) *** 
# MOST SIGNIFICANT BUT WARNING => did not converge?
randslope_model3 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (OrgSize | US_State), 
  data = df)
anova(randslope_model3, randint_model) 

# Level = State (random intercept + random slope by YearsCodeProNum) 0.01044 *
randslope_model4 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (YearsCodeProNum | US_State), 
  data = df)
anova(randslope_model4, randint_model) 

# Level = State (random intercept + random slope by Age) 0.002854 **
# CHOSEN ONE
randslope_model5 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (Age | US_State), 
  data = df)
anova(randslope_model5, randint_model) 

# Level = State (random intercept + random slope by EdLevel) NS
randslope_model6 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (EdLevel | US_State), 
  data = df)
anova(randslope_model6, randint_model) 

# Level = State (random intercept + random slope by `Senior Executive (C-Suite, VP, etc.)`) 0.008111 **
randslope_model7 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (SeniorExecutive | US_State), 
  data = df)
anova(randslope_model7, randint_model) 

final_model = randslope_model5 
vif(final_model)
summary(final_model)
predict(final_model)

################################################################################
############################## 3. POOL IMPUTATIONS #############################
################################################################################

###################### JOINING ALL IMP DATA SETS INTO ONE ######################

df_complete = complete(imputed_norm, "all")

# Drop unused columns
df_complete[dropcols] = NULL
df_complete$logConvertedCompYearly = log(df_complete$ConvertedCompYearly)

df_complete %>% str

# convert logical variables from numeric back to logical
for (i in colnames(df_complete)) {
  if (class(df_complete[[i]]) == "numeric") {
    if (all(df_complete[[i]] == df_complete[[i]]^2)) {
      df_complete[[i]] = as.logical(df_complete[[i]])
    }
  }
}

# changing baseline for education level to "Masters" as recommended by Tego
df_complete = df_complete %>% mutate(
  EdLevel = relevel(
    EdLevel,
    "Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)"
  )
)

randint_model = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods + 
    (1 | US_State),
  data = df_complete
)

randslope_final = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (1 | US_State) + 
    (Age | US_State), 
  data = df_complete
)
anova(randslope_final, randint_model) 
summary(randslope_final)

################################ POOL MODELS ###################################


imp_model = with(
  data = imputed_norm,
  lmer(
    logConvertedCompYearly ~ Gender +
      Ethnicity +
      YearsCodeProNum +
      Age +
      OrgSize +
      EdLevel +
      SeniorExecutive +
      DevOps_Admin +
      School +
      Research +
      OtherMethods +
      (1 | US_State),
    data = df
  )
)

final_model_pooled = pool(imp_model)
summary(final_model_pooled)
saveRDS(final_model_pooled, "../20_intermediate_files/final_model.rds")


################################################################################


############################# Pretty table below ###############################
#***************************** DEPRECATED BELOW ********************************

summary_step = summary(final_model)
summaryprint = data.frame(summary_step$coefficients)[1:2]
summaryexpprint = data.frame(exp(summary_step$coefficients))[1:2]
confprint = data.frame(exp(confint(final_model)))[3:40,]

summarydf = data.frame(round(cbind(summaryprint, summaryexpprint, confprint),4))
colnames(summarydf) = c("Estimate","Std. Error","Estimate(Exp)","Std. Error(Exp)","2.5%(Exp)","97.5%(Exp)")
knitr::kable(summarydf, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position", "scale_down"))

dotplot(ranef(final_model))

ranefprint = data.frame(final_summary$varcor)
ranefprint = ranefprint[,-3]
ranefprint[is.na(ranefprint)] = ""
ranefprint[,3:4] = round(ranefprint[,3:4],4)
colnames(ranefprint) = c("Groups","Name","Variance","Std.Dev.")

knitr::kable(ranefprint, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position"))

############################## Export for python ###############################
# State-level random intercept plot
x = ranef(randint_model, condVar=TRUE)$US_State
xdf = data.frame(pointest=ranef(randint_model, condVar=TRUE)$US_State, err=as.vector(sqrt(attr(x, "postVar"))))
xdf$pointestimate = xdf$X.Intercept.
xdf$state = rownames(xdf)
xdf$X.Intercept. = NULL
write_parquet(xdf, "../20_intermediate_files/ranef_randint_bystate.parquet")
