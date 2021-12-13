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
library(miceadds)
library(sirt)
library(lme4)
library(arrow)


########################### TO EXPLORE MICEADDS LATER ##########################

# datlist <- miceadds::datlist_create(datlist)
# 
# #** fit lme4 model for all imputed datasets
# formula <- math ~ hisei + miceadds::gm( books, idschool ) + ( 1 | idschool )
# models <- list()
# M <- length(datlist)
# for (mm in 1:M){
#   models[[mm]] <- lme4::lmer( formula, data=datlist[[mm]], REML=FALSE)
# }
# #** statistical inference
# res1 <- miceadds::lmer_pool(models)
# summary(res1)

################################################################################
############################### 0. LOADING THE DATA ############################
################################################################################

imputed_norm = readRDS("../20_intermediate_files/imputed_norm.rds")


# factor_cols = c("US_State", "EdLevel", "OrgSize", "Age", "Gender", "Trans","Sexuality", "Ethnicity", "Accessibility", "MentalHealth") 
# df[factor_cols] = lapply(df[factor_cols], factor)


################################################################################
################################## 1. EDA ######################################
################################################################################

######################## EVALUATE ONE IMPUTED DATA SET #########################
head(df)
str(df)
dim(df)
summary(df)
colnames(df)


rand_sample = sample(1:5, 1); set.seed(42); rand_sample
df = complete(imputed_norm, rand_sample); df # just going to evaluate one for the project
df$logConvertedCompYearly = log(df$ConvertedCompYearly)


df %>% str

# convert logical variables from numeric back to logical
for (i in colnames(df)) {
  if (class(df[[i]]) == "numeric") {
    if (all(df[[i]] == df[[i]]^2)) {
      df[[i]] = as.logical(df[[i]])
    }
  }
}

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
hist(df$logConvertedCompYearly) # neat

# Look at numeric predictors
hist(df$YearsCodeProNum)
hist(log(df$YearsCodeProNum)) # wow looks great but predictors don't need to be normally distributed
ggplot(data=df, aes(x=YearsCodeProNum, y=logConvertedCompYearly)) + geom_point() #curved trend
   
# Look at numeric predictors
hist(df$logYearsCodeProNum)
ggplot(data=df, aes(x=logYearsCodeProNum, y=logConvertedCompYearly)) + geom_point() #curved trend

hist(df$YearsCodeNum) 
hist(log(df$YearsCodeNum)) # not bad
ggplot(data=df, aes(x=YearsCodeNum, y=logConvertedCompYearly)) + geom_point() #curved trend

# Look at factor predictors
ggplot(data=df, aes(x=Age1stCode, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$Age1stCode)

ggplot(data=df, aes(x=EdLevel, y=logConvertedCompYearly)) + geom_boxplot() #**
Gpotable(df$EdLevel) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=OrgSize, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$OrgSize) # maybe collapse into less levels #DONE

ggplot(data=df, aes(x=Currency, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$Currency) 

ggplot(data=df, aes(x=Age, y=logConvertedCompYearly)) + geom_boxplot() #***
table(df$Age) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #*
table(df$Gender) 

ggplot(data=df, aes(x=Trans, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Trans) 

ggplot(data=df, aes(x=Sexuality, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Sexuality) 

ggplot(data=df, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #**
table(df$Ethnicity) # maybe collapse into less levels # DONE

ggplot(data=df, aes(x=Accessibility, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$Accessibility)

ggplot(data=df, aes(x=MentalHealth, y=logConvertedCompYearly)) + geom_boxplot() # not significant
table(df$MentalHealth)


# Look at hierarchical predictor
ggplot(data=df, aes(x=US_State, y=logConvertedCompYearly)) + geom_boxplot() # different -> random intercepts


################################################################################
############################### 2. MODEL SELECTION #############################
################################################################################


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
  logConvertedCompYearly ~ 
    EdLevel +
    Age1stCode +
    OrgSize +
    Currency +
    Age +
    Gender +
    Trans +
    Sexuality +
    Ethnicity +
    Accessibility +
    MentalHealth +
    # YearsCodeNum + highly correlated with YearsCodeProNum
    YearsCodeProNum +
    OnlineResources +
    School +
    Peers +
    OtherMethods +
    Bootcamp +
    SoftwareDevelopment +
    DataScience +
    Research +
    DevOps_Admin +
    ProductManager +
    SeniorExecutive +
    NonTechnicalRole,
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
# SeniorExecutive + DevOps_Admin + School + Research + OtherMethods + OnlineResources

# BIC stepwise selection
# step_model = step(null_model, scope = formula(full_model), direction = 'forward', trace=0, k=log(n))
# summary(step_model) # same as stepwise

################################# INTERACTIONS #################################
# Look at groups of binary predictors
ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Gender, ncol=4)  #**

ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #**

# ############################# PRETTY PLOT ##########################
# ggplot(df,aes(x=Ethnicity, y=logConvertedCompYearly)) +
#   geom_boxplot(outlier.color = "#061953") +  
#   facet_wrap(~SeniorExecutive, ncol=4) + 
#   labs(title="log of Compensation by Ethnicity by SeniorExecutive",y="log of ConvertedCompYearly (USD)", x="Ethnicity") +
#   theme_classic() +
#   theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5))
# 
# ####################################################################

ggplot(df, aes(x=Gender, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #**

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Gender, ncol=4) #.

ggplot(df, aes(x=Age, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) # not significant

ggplot(df, aes(x=SeniorExecutive, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~Age, ncol=4) # not significant

ggplot(df, aes(x=OrgSize, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~SeniorExecutive, ncol=4) #.

# Look at predictors by state
# sample 20 states to look at 
sample_states = df$US_State %>% unique %>% sample(20)
df_sample = df[df$US_State.isin(sample_states)]

ggplot(df, aes(x=Gender, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~US_State, ncol=8) #*

ggplot(df, aes(x=Ethnicity, y=logConvertedCompYearly)) +
  geom_boxplot() +
  facet_wrap(~US_State, ncol=8) #*

# Significant Variables:
# Gender + Ethnicity + YearsCodeProNum + Age + OrgSize + EdLevel + 
# SeniorExecutive + DevOps_Admin + School + Research + OtherMethods + OnlineResources


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
    OnlineResources + 
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
summary(stepint_model) #nochange -> interactions not significant


########################## ANOVA TEST INTERACTIONS #############################

eth_int = lm(
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
    OnlineResources + 
    SeniorExecutive:Ethnicity,
  data = df
)
anova(step_model, eth_int) # not significant

gend_int = lm(
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
    OnlineResources + 
    SeniorExecutive:Gender,
  data = df
)
anova(step_model, gend_int) # not significant

gendeth_int = lm(
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
    OnlineResources + 
    Ethnicity:Gender,
  data = df
)
anova(step_model, gend_int) # not significant


########################## Pretty table & plot below ###########################

summary_step = summary(stepint_model)
summaryprint = data.frame(summary_step$coefficients)
colnames(summaryprint) = c("Estimate","Std. Error","t value", "Pr(>|t|)")
knitr::kable(summaryprint, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position"))

plot(step_model, which=1, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)
plot(step_model, which=2, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)
plot(step_model, which=5, pch=16, col='#061953', cex=0.8, sub="", frame.plot=FALSE)

############################# LINEAR MODEL ASSESSMENT ##########################

par(bty='n')
plot(step_model, which=1)                       # equal variance and independence assum problem
plot(step_model, which=2)                       # OOOOOUUFFF
plot(step_model, which=3)
plot(step_model, which=4)                       # no influential points?
plot(step_model, which=5)                       # several outliers but not influential
plot(df$YearsCodeNum, step_model$residuals)     # linearity assumption holds
plot(df$YearsCodeProNum, step_model$residuals)
plot(df$logYearsCodeProNum, step_model$residuals)
vif(step_model)           

num_cols = c(
  "YearsCodeProNum",
  "SeniorExecutive",
  "DevOps_Admin",
  "OtherMethods",
  "Research",
  "School",
  "OnlineResources"
)

cor(df[num_cols])

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
randint_summary
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
    (OrgSize | US_State), 
  data = df,
  control = lmerControl(optimizer="bobyqa")
)
anova(randslope_model3, randint_model) 

# Level = State (random intercept + random slope by YearsCodeProNum) 0.01044 *
randslope_model4 = lmer(
  logConvertedCompYearly ~ Gender +
    Ethnicity +
    #YearsCodeProNum +
    Age +
    OrgSize +
    EdLevel +
    SeniorExecutive +
    DevOps_Admin +
    School +
    Research +
    OtherMethods +
    (YearsCodeProNum | US_State), 
  data = df,
  control = lmerControl(optimizer="bobyqa"))
summary(randslope_model4)
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

# Level = State (random intercept + random slope by Senior Executive (C-Suite, VP, etc.)) 0.008111 **
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




########################### FINAL MODEL EVALUATION #############################


qqnorm(residuals) + qqline

final_model = randint_model

vif(final_model)
summary(final_model)
preds = predict(final_model)
truth = df$logConvertedCompYearly


############################# Pretty table below ###############################

summary_step = summary(final_model)
summaryprint = data.frame(summary_step$coefficients)[1:2]
summaryexpprint = data.frame(exp(summary_step$coefficients))[1:2]
confprint = data.frame(exp(confint(final_model)))[3:40,]

summarydf = data.frame(round(cbind(summaryprint, summaryexpprint, confprint),4))
colnames(summarydf) = c("Estimate","Std. Error","Estimate(Exp)","Std. Error(Exp)","2.5%(Exp)","97.5%(Exp)")
knitr::kable(summarydf, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position", "scale_down"))

dotplot(ranef(final_model))

ranefprint = data.frame(summary_step$varcor)
ranefprint = ranefprint[,-3]
ranefprint[is.na(ranefprint)] = ""
ranefprint[,3:4] = round(ranefprint[,3:4],4)
colnames(ranefprint) = c("Groups","Name","Variance","Std.Dev.")

knitr::kable(ranefprint, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position"))



######################## Demo Personas for Interpretation ######################

Gender = c("Man", "Woman")
Ethnicity = c("East Asian", "East Asian")
YearsCodeProNum = c(2, 2)
Age = c("25-34 years old", "25-34 years old")
OrgSize = c("10,000 or more employees", "10,000 or more employees")
EdLevel = c("Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)", "Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)")
SeniorExecutive = c(FALSE, FALSE)
DevOps_Admin = c(FALSE, FALSE)
School = c(TRUE, TRUE)
Research = c(FALSE, FALSE)
OtherMethods = c(FALSE, FALSE)
US_State = c("Washington", "Washington")

demo_personas = data.frame(Gender, Ethnicity, YearsCodeProNum, Age, OrgSize, EdLevel, SeniorExecutive, DevOps_Admin, School, Research, OtherMethods)

levels(demo_personas$Gender) = levels(df$Gender)
levels(demo_personas$Ethnicity) = levels(df$Ethnicity)
levels(demo_personas$Age) = levels(df$Age)
levels(demo_personas$OrgSize) = levels(df$OrgSize)
levels(demo_personas$EdLevel) = levels(df$EdLevel)
exp(predict(final_model, demo_personas))


############################## Export for python ###############################
# State-level random intercept plot
x = ranef(randint_model, condVar=TRUE)$US_State
xdf = data.frame(pointest=ranef(randint_model, condVar=TRUE)$US_State, err=as.vector(sqrt(attr(x, "postVar"))))
xdf$pointestimate = xdf$X.Intercept.
xdf$state = rownames(xdf)
xdf$X.Intercept. = NULL
write_parquet(xdf, "../20_intermediate_files/ranef_randint_bystate.parquet")

# To calc r-squared
preds = data.frame(predict(final_model))
truth = data.frame(df$logConvertedCompYearly)
ydf = cbind(preds, truth)
write_parquet(ydf, "../20_intermediate_files/predictions.parquet")

################################################################################
################################################################################
#NOTE FOR THE PURPOSE OF THE REPORT, WE USED JUST ONE DF FROM THE FIVE TO EVAL##
################# AND DID NOT USE ANY OF THE CODE BELOW DUE ####################
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
    (Age | US_State), 
  data = df_complete
)
anova(randslope_final, randint_model) 
summary(randslope_final)

################################ POOL MODELS ###################################

# Questions: How to change binary variable types back to logi?
# how to access additional parameters

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

help(lmer_pool)
lmer_pool(step_model)

lmer_pool2(imp_model)
final_model_pooled = pool(imp_model)
final_model_pooled$glanced
final_model_pooled$pooled
confint(final_model_pooled)
summary(final_model_pooled)
saveRDS(final_model_pooled, "../20_intermediate_files/final_model.rds")


################################################################################


################################################################################
######################### EXPERIMENT (DID NOT USE) #############################
################################################################################

df$ConvertedCompYearly %>% median

# df_low = df[df$ConvertedCompYearly <= (df$ConvertedCompYearly %>% median),]
# df_high = df[df$ConvertedCompYearly > (df$ConvertedCompYearly %>% median),]

df_low = df[df$SeniorExecutive == 0,]
df_high = df[df$SeniorExecutive != 0,]

hist(df_low$YearsCodeProNum)
hist(df_high$YearsCodeProNum)

hist(df_low$ConvertedCompYearly)
hist(df_high$ConvertedCompYearly)

table(df_low$SeniorExecutive)
table(df_high$SeniorExecutive)

hist(log(df_low$ConvertedCompYearly))
hist(log(df_high$ConvertedCompYearly))

table(df_low$Gender)
table(df_high$Gender)

table(df_low$Ethnicity)
table(df_high$Ethnicity)

ggplot(data=df_low, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #*
ggplot(data=df_high, aes(x=Ethnicity, y=logConvertedCompYearly)) + geom_boxplot() #*

ggplot(data=df_low, aes(x=Ethnicity, y=ConvertedCompYearly)) + geom_boxplot() #*
ggplot(data=df_high, aes(x=Ethnicity, y=ConvertedCompYearly)) + geom_boxplot() #*

ggplot(data=df_low, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #*
ggplot(data=df_high, aes(x=Gender, y=logConvertedCompYearly)) + geom_boxplot() #*


df %>% str

# convert logical variables from numeric back to logical
for (i in colnames(df_low)) {
  if (class(df_low[[i]]) == "numeric") {
    if (all(df_low[[i]] == df_low[[i]]^2)) {
      df_low[[i]] = as.logical(df_low[[i]])
    }
  }
}

for (i in colnames(df_high)) {
  if (class(df_high[[i]]) == "numeric") {
    if (all(df_high[[i]] == df_high[[i]]^2)) {
      df_high[[i]] = as.logical(df_high[[i]])
    }
  }
}


df_low = df_low %>% mutate(
  EdLevel = relevel(
    EdLevel,
    "Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)"
  )
)


df_high = df_high %>% mutate(
  EdLevel = relevel(
    EdLevel,
    "Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)"
  )
)


null_model_low = lm(logConvertedCompYearly ~ Gender + Ethnicity , data=df_low)
interact_model_low = lm(
  logConvertedCompYearly ~  EdLevel +
    Age1stCode +
    OrgSize +
    Currency +
    Age +
    Gender +
    Trans +
    Sexuality +
    Ethnicity +
    Accessibility +
    MentalHealth +
    # YearsCodeNum + highly correlated with YearsCodeProNum
    YearsCodeProNum +
    OnlineResources +
    School +
    Peers +
    OtherMethods +
    Bootcamp +
    SoftwareDevelopment +
    DataScience +
    Research +
    DevOps_Admin +
    ProductManager +
    NonTechnicalRole+
    Ethnicity:YearsCodeProNum + 
    Gender:YearsCodeProNum +
    OrgSize:YearsCodeProNum,
  data = df_low
)

"Master’s or Professional degree (M.A., M.S., M.Eng., MBA, JD, MD, etc.)"


n_low=nrow(df_low)
#BIC stepwise selection
step_model = step(null_model_low, scope = list(upper=interact_model_low, lower=null_model_low), direction = 'both', trace=0, k=log(n_low))
summary(step_model) # more parsimonious than AIC, slightly worse R^2 (0.4) -> CHOSEN ONE
AIC(step_model) #12761.55

par(bty='n')
plot(step_model, which=1)                       # equal variance and independence assum problem
plot(step_model, which=2)                       # OOOOOUUFFF
plot(step_model, which=3)
plot(step_model, which=4)                       # no influential points?
plot(step_model, which=5)                       # several outliers but not influential
plot(df_low$YearsCodeNum, step_model$residuals)     # linearity assumption holds
plot(df_low$YearsCodeProNum, step_model$residuals)
vif(step_model)


null_model_high = lm(logConvertedCompYearly ~ Gender + Ethnicity , data=df_high)
interact_model_high = lm(
  logConvertedCompYearly ~ EdLevel +
    Age1stCode +
    OrgSize +
    Age +
    Gender +
    Trans +
    Sexuality +
    Ethnicity +
    YearsCodeProNum +
    OnlineResources +
    School +
    Peers +
    OtherMethods +
    Bootcamp +
    SoftwareDevelopment +
    DataScience +
    Research +
    DevOps_Admin +
    ProductManager +
    NonTechnicalRole,
  data = df_high
)

n_high=nrow(df_high)
#BIC stepwise selection
step_model = step(null_model_high, scope = list(upper=interact_model_high, lower=null_model_high), direction = 'both', trace=0, k=log(n_high))
summary(step_model) # more parsimonious than AIC, slightly worse R^2 (0.4) -> CHOSEN ONE
AIC(step_model) #516

par(bty='n')
plot(step_model, which=1)                       
plot(step_model, which=2)                       
plot(step_model, which=3)
plot(step_model, which=4)                       
plot(step_model, which=5)                     
plot(df_high$YearsCodeNum, step_model$residuals)    
plot(df_high$YearsCodeProNum, step_model$residuals)
vif(step_model)

################################################################################
############################## END OF EXPERIMENT ###############################
################################################################################





