rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(kableExtra)

################################################################################
############################### 0. Loading the data ############################
################################################################################

df = read.csv("../00_source_data/survey_results_public.csv")

head(df)
str(df)
dim(df)
summary(df)

################################ First Filter ##################################

# remove programming stack/survey related questions
data.frame(colnames(df))
drop_cols_index = c(1, 17:38, 46:47)
colnames(df[drop_cols_index]) # check these columns
df = df[-drop_cols_index]

# filter for professional/employed developers
unique(df$MainBranch)
df = df %>% filter(MainBranch == "I am a developer by profession")
df$MainBranch = NULL
unique(df$Employment)
df = df %>% filter(Employment == "Employed full-time")
df$Employment = NULL

saveRDS(df,"../20_intermediate_files/initial_data_set.rds")

############################# Looking at US Only ###############################

df = readRDS("../20_intermediate_files/initial_data_set.rds")

sort(unique(df$Country))
df = df %>% filter(Country == "United States of America")

location_cols = c("Country", "UK_Country")
df[location_cols] = NULL


# look at N/As
sapply(df, function(z) sum(is.na(z))/length(z))
sapply(df, function(z) sum(is.na(z)))

# drop N/As, 
# TODO: look into imputations later
df = df %>% drop_na()

# look at response variable

df$compcompare = (df$CompTotal == df$ConvertedCompYearly)
df[df$compcompare == TRUE, c("CompTotal", "CompFreq", "ConvertedCompYearly", "Currency")]
df[df$compcompare == FALSE, c("CompTotal", "CompFreq", "ConvertedCompYearly", "Currency")]
unique(df$Currency)
ggplot(data=df, aes(x=ConvertedCompYearly)) + geom_histogram(bins = 50)  # few large outliers
percentile_cuttoff = quantile(df$ConvertedCompYearly, 0.95)
percentile_cuttoff
df %>% filter(ConvertedCompYearly >= percentile_cuttoff)  %>% c("CompTotal", "CompFreq", "ConvertedCompYearly", "Currency", "compcompare")
df %>% filter(ConvertedCompYearly <= percentile_cuttoff)

# Looks like ConvertedCompYearly is converted into yearly USD Salary
# However, some conversions seem sketchy as users might have mistakenly marked
# weekly/monthly when they inputted their yearly salary
# I will take the ConvertedCompYearly and only address mistakes in outliers

unique(df$EdLevel)
unique(df$Age1stCode)
unique(df$LearnCode) # multiple choice and busy
unique(df$YearsCode) # maybe keep one of the below
unique(df$YearsCodePro)
unique(df$DevType)
df %>% filter(grepl("Senior Executive (C-Suite, VP, etc.)", DevType))
df %>% filter(df %in% "Senior Executive (C-Suite, VP, etc.)")
unique(df$OrgSize)
df["Just me - I am a freelancer, sole proprietor, etc.", OrgSize]
unique(df$Age)
unique(df$Gender)
df[df$Trans != "Yes", "Gender"]
df[df$Gender == "Man" & df$Trans != "No", "Trans"]
unique(df$Sexuality)
unique(df$Ethnicity)
unique(df$Accessibility) #MC
unique(df$MentalHealth) #MC

factor_cols = 
data[factor_cols] <- lapply(data[factor_cols], factor)

round(df$CompTotal,0)
df[12,]

########################## Looking at all locations ############################


# drop location first for analysis, will get back to this 
location_cols = c("Country", "US_State", "UK_Country")
df[location_cols] = NULL
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# keep only complete data points not counting for location
df = df %>% drop_na()



