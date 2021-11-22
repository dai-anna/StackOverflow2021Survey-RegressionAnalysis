rm(list = ls())

library(ggplot2)
library(xtable)
library(knitr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(data.table)

################################################################################
################################# 0. LOAD THE DATA #############################
################################################################################

df = read.csv("../00_source_data/survey_results_public.csv")

head(df)
str(df)
dim(df)
summary(df)

################################ Initial Filter ##################################

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

# saveRDS(df, "../20_intermediate_files/initial_data.rds")


################################################################################
######################### 1. EXPLORE AND CLEAN THE DATA ########################
################################################################################

################################################################################
############################# US STATES APPROACH ###############################

df_us = readRDS("../20_intermediate_files/initial_data.rds")

sort(unique(df_us$Country))
df_us = df_us %>% filter(Country == "United States of America")

location_cols = c("Country", "UK_Country")
df_us[location_cols] = NULL

########################## EXAMINE RESPONSE VARIABLE ###########################

df_us$compcompare = (df_us$CompTotal == df_us$ConvertedCompYearly)
df_us[df_us$compcompare == TRUE, c("CompTotal", "CompFreq", "ConvertedCompYearly", "Currency")]
df_us[df_us$compcompare == FALSE, c("CompTotal", "CompFreq", "ConvertedCompYearly", "Currency")]
ggplot(data = df_us, aes(x = ConvertedCompYearly)) + geom_histogram(bins = 50)  # some LARGE outliers
# Looks like ConvertedCompYearly is converted into yearly USD Salary
# However, some conversions seem sketchy as users might have mistakenly marked
# weekly/monthly when they inputted their yearly salary
# I will take the ConvertedCompYearly and only use other columns to examine outliers

########################## ADDRESS RESPONSE OUTLIERS ###########################

unique(df_us$Currency)
# Currency converter (DIDN'T NEED IN THE END)
convertcurrency = function(x, from, to) {
  # 1 usd in each currency (using average exchange rate in 2021)
  values = c(3590.9951, 1.2505, 52.206, 5.3633, 1.0000)
  names(values) = c(
    "UGX\tUgandan shilling",
    "CAD\tCanadian dollar",
    "MKD\tMacedonian denar",
    "BRL\tBrazilian real",
    "USD\tUnited States dollar"
  )
  # calculate (convert from into USD, and divide by to)
  values[to] / (values[from] / x)
}

# Testing
# convertcurrency(100, "UGX\tUgandan shilling", "USD\tUnited States dollar")
# convertcurrency(100, "CAD\tCanadian dollar", "USD\tUnited States dollar")
# convertcurrency(100, "MKD\tMacedonian denar", "USD\tUnited States dollar")
# convertcurrency(100, "BRL\tBrazilian real", "USD\tUnited States dollar")


# Average software developers in the US according different sources are:
# $55-120K    ZipRecruiter
# $36-260K    Built In
# $63-100K    Zippia
# $56-86K     Salary.com
# I will use the lower and upper bound of this data to evaluate comp outlier treatment

# count of foreign currencies: only 7 people reported foreign currencies

df_us %>% filter(Currency != "USD\tUnited States dollar") %>% count
df_us %>% filter(Currency != "USD\tUnited States dollar")
# Looking at the different salaries, it looks like the UGX is most like mistyped USD as they are next to each other in the survey
outlier_index = df_us[df_us$Currency == "UGX\tUgandan shilling", c("ConvertedCompYearly", "CompTotal", "Currency")] %>% rownames
df_us[df_us$Currency ==
        "UGX\tUgandan shilling", "ConvertedCompYearly"] =
  df_us[df_us$Currency ==
          "UGX\tUgandan shilling", "CompTotal"]
df_us[df_us$Currency == "UGX\tUgandan shilling", "Currency"] = "USD\tUnited States dollar"
df_us[outlier_index,]

# Evaluate lower bound of total comp < around 10K, which is equivalent in our data to the bottom 0.2% of the data
# this is less than half of the lower bound of full-time developer salary according to our research
# 15 people heavily skews our distribution and entirely out of "normal" range, so we will drop them
sum(is.na(df_us$ConvertedCompYearly))

lower_bound_check = quantile(df_us$ConvertedCompYearly, 0.002, na.rm = TRUE)
lower_bound_check

df_us %>% filter(ConvertedCompYearly < lower_bound_check) %>% count
df_us = df_us %>% filter(is.na(df_us$ConvertedCompYearly)|df_us$ConvertedCompYearly >= lower_bound_check)

sum(is.na(df_us$ConvertedCompYearly))


ggplot(data = df_us, aes(x = ConvertedCompYearly)) + geom_histogram(bins = 50)

# Examine closely the upper bound of total comp > around 430K, which is top 5% of of dataset
# this is more than double the upper bound of full-time developer salary according to our research

upper_bound_check = quantile(df_us$ConvertedCompYearly, 0.975, na.rm = TRUE) ##### <<<<<<<<< .95 to use top 5% cutoff
upper_bound_check # this is already much higher than the
df_check = df_us %>%
  filter(!compcompare) %>%
  filter(ConvertedCompYearly >= upper_bound_check) %>%
  select(c(
    "CompTotal",
    "CompFreq",
    "ConvertedCompYearly",
    "Currency",
    "compcompare"
  )) # this yield 272 observations to check closely (where compfreq is not "Yearly")
#184 observations for 2.5%
# at 5%                                               # at 2.5%
min(df_check$CompTotal) # $11,000, could be weekly/monthly                    $41,250 highly unlikely to be weekly/monthly
max(df_check$CompTotal) # $850,000, probably mistake                          #850,000 Definitely mistake
min(df_check$ConvertedCompYearly) #550,000, could be true                     $1,560,000 highly improbable
max(df_check$ConvertedCompYearly) #21,822,250, which is definitely not real   $21,822,250 definitely mistake or troll

# we will take lower bound of yearly salary to say, if weekly/monthly salary is > low yearly salary, then it's probably yearly
# if not, a weekly salary of 36K would be 1,872,000 and a monthly salary of 36K would be 432,000

# we will take lower bound of yearly salary to say, if weekly/monthly salary is much > low yearly salary, then it's probably yearly
# if not, a weekly salary of 41,250 would be 2,145,000 and a monthly salary of 36K would be 495,000

df_us %>%
  filter(!compcompare) %>%
  filter(CompTotal > min(df_check$CompTotal)) %>% #36000 <<<<<< to use 5% threshold
  filter(ConvertedCompYearly >= upper_bound_check) %>%
  count # total 207 observations >= upper bound

df_us$ConvertedCompYearly[df_us$compcompare == FALSE &
                            df_us$CompTotal > min(df_check$CompTotal) &
                            #36000 <<<<<< to use 5% threshold
                            df_us$ConvertedCompYearly >= upper_bound_check & 
                            !is.na(df_us$ConvertedCompYearly)] =
  df_us$CompTotal[df_us$compcompare == FALSE &
                    df_us$CompTotal > min(df_check$CompTotal) &
                    #36000 <<<<<< to use 5% threshold
                    df_us$ConvertedCompYearly >= upper_bound_check &
                    !is.na(df_us$ConvertedCompYearly)]


sum(is.na(df_us$ConvertedCompYearly))


############## Plot the outcome ###############
ggplot(data = df_us, aes(x = ConvertedCompYearly)) + geom_histogram(bins = 50)
# OH DEAR OH DEAR

############## Log to the Rescue ###############
df_us$logConvertedCompYearly = log(df_us$ConvertedCompYearly)
ggplot(data = df_us, aes(x = logConvertedCompYearly)) + geom_histogram(bins = 50)  # few large outliers
# MUCH BETTER -> Issues due to N/As?

df_test = data.frame(df_us$ConvertedCompYearly) %>% drop_na()
df_test$df_us.ConvertedCompYearly
ggplot(data = df_test, aes(x = df_us.ConvertedCompYearly)) + geom_histogram(bins = 50)  # few large outliers
ggplot(data = df_test, aes(x = log(df_us.ConvertedCompYearly))) + geom_histogram(bins = 50)  # few large outliers

################################################################################
########################## ADD INTERMEDIATE DATA ###############################
# saveRDS(df_us, "../20_intermediate_files/clean_response.rds")

df_us = readRDS("../20_intermediate_files/clean_response.rds")
drop_cols = c("CompTotal", "CompFreq", "compcompare")
df_us[drop_cols] = NULL

################################################################################

############################# EXAMINE PREDICTORS ###############################

# Candidate for Hierarchy
table(df_us$US_State) # ok - state with least input is 2

####################### SINGLE SELECTION FACTOR VARIABLES ######################

df_us$Currency = as.factor(df_us$Currency)

# Ordinal? Factor Variables
table(df_us$EdLevel)
df_us$EdLevel = factor(
  df_us$EdLevel,
  order = FALSE,
  levels = c(
    "Primary/elementary school",
    "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
    "Some college/university study without earning a degree",
    "Associate degree (A.A., A.S., etc.)",
    "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
    "Master’s degree (M.A., M.S., M.Eng., MBA, etc.)",
    "Professional degree (JD, MD, etc.)",
    "Other doctoral degree (Ph.D., Ed.D., etc.)",
    "Something else"
  )
)
table(df_us$OrgSize)
df_us$OrgSize = factor(
  df_us$OrgSize,
  order = FALSE,
  levels = c(
    "Just me - I am a freelancer, sole proprietor, etc.",
    "2 to 9 employees",
    "10 to 19 employees",
    "20 to 99 employees",
    "100 to 499 employees",
    "500 to 999 employees",
    "1,000 to 4,999 employees",
    "5,000 to 9,999 employees",
    "10,000 or more employees",
    "I don’t know"
  )
)

table(df_us$Age)
df_us$Age = factor(
  df_us$Age,
  order = FALSE,
  levels = c(
    "Under 18 years old",
    "18-24 years old",
    "25-34 years old",
    "35-44 years old",
    "45-54 years old",
    "55-64 years old",
    "65 years or older",
    "Prefer not to say"
  )
)

# Factor Variable
table(df_us$Trans)
df_us$Trans[df_us$Trans == "Or, in your own words:"] = "Other"
df_us$Trans = factor(df_us$Trans)

# Experience related: keep one of the blow
table(df_us$Age1stCode)
df_us$Age1stCode = as.factor(df_us$Age1stCode)

unique(df_us$YearsCode)
df_us$YearsCodeNum = df_us$YearsCode
df_us$YearsCodeNum[df_us$YearsCodeNum == "Less than 1 year"] = 0
df_us$YearsCodeNum[df_us$YearsCodeNum == "More than 50 years"] = 51
df_us$YearsCodeNum = as.numeric(df_us$YearsCodeNum)
hist(df_us$YearsCodeNum)
hist(log(df_us$YearsCodeNum))

unique(df_us$YearsCodePro)
df_us$YearsCodeProNum = df_us$YearsCodePro
df_us$YearsCodeProNum[df_us$YearsCodeProNum == "Less than 1 year"] = 0
df_us$YearsCodeProNum[df_us$YearsCodeProNum == "More than 50 years"] = 51
df_us$YearsCodeProNum = as.numeric(df_us$YearsCodeProNum)
hist(df_us$YearsCodeProNum)
hist(log(df_us$YearsCodeProNum))


# Multi-select variables
# unique(df_us$LearnCode) # need to collapse
# unique(df_us$DevType) # need to collapse
# unique(df_us$Gender) # turn into single choice
# unique(df_us$Sexuality)  # turn into single choice
# unique(df_us$Ethnicity)  # turn into single choice
# unique(df_us$Accessibility)  # turn into single choice
# unique(df_us$MentalHealth)  # turn into single choice


################### CONVERT MULTISELECT TO MULTI-HOT-ENCODED ###################

# First convert to list and unlist to see unique variables
listvar = function(col) {
  newcol = strsplit(col, ";")
  return(newcol)
}

# unlist them as unique values
unlistvar = function(col) {
  uniques = col %>% unlist %>% unique
  return(uniques)
}

# collapse variable and unlist
listvar_multi = function(col, mmap) {
  newcol = strsplit(col, ";") %>% sapply(function(z) unique(mmap[z]))
  return(newcol)
}

multiselect_cols = c(
  "LearnCode",
  "DevType",
  "Gender",
  "Sexuality",
  "Ethnicity",
  "Accessibility",
  "MentalHealth"
)
# multiselect_cols_inspect = c(
#   "LearnCode_check",
#   "DevType_check",
#   "Gender_check",
#   "Sexuality_check",
#   "Ethnicity_check",
#   "Accessibility_check",
#   "MentalHealth_check"
# )
# 
# for (i in 1:length(multiselect_cols)) {
#   df_us[[multiselect_cols_inspect[i]]] = listvar(df_us[[multiselect_cols[i]]])
# }

# unlists into new columns
for (col in multiselect_cols) {
  df_us[[paste0(col,"_check")]] = listvar(df_us[[col]])
}

# Collapsing variables
unlistvar(df_us$LearnCode_check)
learncode_map = c(
  "Other online resources (ex: videos, blogs, etc)" = "Online Resources",
  "Online Forum" = "Online Resources",
  "Online Courses or Certification" = "Online Resources",
  "Other (please specify):" = "Other",
  "Books / Physical media" = "Other",
  "Colleague" = "Peers",
  "Friend or family member" = "Peers",
  "Coding Bootcamp" = "Coding Bootcamp",
  "School" = "School"
)
df_us$LearnCode_list = listvar_multi(df_us$LearnCode, learncode_map)

unlistvar(df_us$LearnCode_list)

#****************************VISIT*********************************#
for (uq in unlistvar(df_us$LearnCode_list)) {
  if(!is.na(df_us$LearnCode_list[1])) {
    df_us[[uq]] = sapply(df_us$LearnCode_list, function(z)
      uq %in% z)
    df_us[[uq]] %<>% as.logical
  }
}

df_us[25:36,]

df_us[!is.na(df_us$LearnCode_list), c("Online Resources",
                                     "School",
                                     "Peers",
                                     "Other",
                                     "Coding Bootcamp")]

df_us[is.na(df_us$LearnCode_list), c("Online Resources",
                                     "School",
                                     "Peers",
                                     "Other",
                                     "Coding Bootcamp")] = NA


df_us[c("LearnCode_check","LearnCode_list")] = NULL


unlistvar(df_us$DevType_check)
devtype_map = c(
  "Developer, embedded applications or devices" = "Software Development",
  "Developer, back-end" = "Software Development",
  "Developer, front-end" = "Software Development",
  "Developer, full-stack" = "Software Development",
  "Developer, desktop or enterprise applications" = "Software Development",
  "Developer, game or graphics" = "Software Development",
  "Developer, QA or test" = "Software Development",
  "Engineer, site reliability" = "Software Development",
  "Developer, mobile" = "Software Development",
  "Engineering manager" = "Software Development",
  "Designer" = "Software Development",
  "Engineer, data" = "Data Science",
  "Data scientist or machine learning specialist" = "Data Science",
  "Data or business analyst" = "Data Science",
  "Database administrator" = "DevOps and Admin",
  "DevOps specialist" = "DevOps and Admin",
  "System administrator" = "DevOps and Admin",
  "Scientist" = "Research",
  "Academic researcher" = "Research",
  "Marketing or sales professional" = "Other/Non-Technical",
  "Other (please specify):" = "Other/Non-Technical",
  "Product manager" = "Product manager",
  "Senior Executive (C-Suite, VP, etc.)" = "Senior Executive (C-Suite, VP, etc.)",
  "Educator" = "Other/Non-Technical",
  "Student" = "Other/Non-Technical"
)
df_us$DevType_list = listvar_multi(df_us$DevType, devtype_map)
unlistvar(df_us$DevType_list)

for (uq in unlistvar(df_us$DevType_list)) {
  if(!is.na(df_us$DevType_list)) {
    df_us[[uq]] = sapply(df_us$DevType_list, function(z)
      uq %in% z)
    df_us[[uq]] %<>% as.logical
  }
}

df_us[is.na(df_us$DevType_list), c(
  "Software Development",
  "Data Science",
  "Research",
  "DevOps and Admin",
  "Product manager" ,
  "Senior Executive (C-Suite, VP, etc.)",
  "Other/Non-Technical"
)]

df_us[is.na(df_us$DevType_list), c(
  "Software Development",
  "Data Science",
  "Research",
  "DevOps and Admin",
  "Product manager" ,
  "Senior Executive (C-Suite, VP, etc.)",
  "Other/Non-Technical"
)] = NA

df_us[c("DevType_check","DevType_list")] = NULL

############### CONVERT MULTI SELECT TO SINGLE SELECT BELOW ####################
# if multiple chosen, then "Other"
unlistvar(df_us$Gender_check)
df_us$Gender %>% unique

df_us$Gender[df_us$Gender != "Prefer not to say" &
                     df_us$Gender != "Woman" &
               df_us$Gender != "Man"] = "Other"
df_us$Gender = as.factor(df_us$Gender)
df_us$Gender_check = NULL

# Women might actually pretend to be men???


# Turn into single select
# if multiple chosen, then "Other"
unlistvar(df_us$Sexuality_check)

df_us$Sexuality %>% unique
# if only "Prefer not to say", "Straight / Heterosexual", "Bisexual", "Gay or Lesbian" -> no change
# if include "Prefer to self-describe:", "Queer" -> "Other"
# else -> "Bisexual" (includes bi and gay and lesbian)
df_us$Sexuality[sapply(df_us$Sexuality_check, function(z)
  as.logical(sum(
    c("Queer", "Prefer to self-describe:") %in% z
  )))] = "Other"
df_us$Sexuality[df_us$Sexuality != "Prefer not to say" &
                  df_us$Sexuality != "Straight / Heterosexual" &
                  df_us$Sexuality != "Other" &
                  df_us$Sexuality != "Gay or Lesbian"] = "Bisexual"
df_us$Sexuality = as.factor(df_us$Sexuality)
df_us$Sexuality_check = NULL



# Turn into single select
# if multiple chosen, then "Multiracial"

unlistvar(df_us$Ethnicity_check)
unique(df_us$Ethnicity)  # need to collapse

# if contain hispan -> hispan
# if only idk, Or, in your own words: -> other
# if contains Bi pr Multiracial -> Bi/Multiracial
# if multiselct -> bi/multiracial


ethinicity_helper = function(x) {
  if ("Hispanic or Latino/a/x" %in% x) {
    return("Hispanic or Latino/a/x")
  } else if (length(x) > 1) {
    return("Bi/Multiracial")
  } else if ("Biracial" %in% x) {
    return("Bi/Multiracial")
  } else if ("Multiracial" %in% x) {
    return("Bi/Multiracial")
  } else if (x %in% c(
    "Or, in your own words:",
    "I don't know",
    "Indigenous (such as Native American, Pacific Islander, or Indigenous Australian)",
    "Middle Eastern"
  )) {
    return("Other")
  } else {
    return(x)
  }
} # grouped Middle Eastern and Indigenous with Other, bc not enough data points

df_us$Ethnicity %>% unique
df_us$Ethnicity = sapply(df_us$Ethnicity_check, ethinicity_helper)
table(df_us$Ethnicity)
# count of list
sapply(df_us$Ethnicity_check, length) %>% table()
df_us$Ethnicity = as.factor(df_us$Ethnicity)
df_us$Ethnicity_check = NULL

# df_us$Ethnicity_clean = df_us$Ethnicity
# df_us$Ethnicity[233]
#
# if (is.element("Multiracial", df_us$Ethnicity_clean) {
#   df_us$Ethnicity_clean = "Multiracial"
# }
#
# for item in df_us$Ethnicity_check{
#   if ("Multiracial" %in% df_us$Ethnicity_clean) {
#     Ethnicity_clean[[item]] = "Multiracial")
#   }
# }
#
# for j in 1:dim(df_us)[1] {
#   if (is.element("Multiracial", df$Ethnicity_clean[j])) {
#     df$Ethnicity_clean[j] = "Multiracial"}}
#

# Turn into single select
unlistvar(df_us$Accessibility_check)
handicap = c(
  "I am unable to / find it difficult to walk or stand without assistance",
  "Or, in your own words:",
  "I am deaf / hard of hearing",
  "I am blind / have difficulty seeing",
  "I am unable to / find it difficult to type"
)
df_us$Accessibility[df_us$Accessibility != "Prefer not to say" &
                      df_us$Accessibility != "None of the above"] = "Handicapped"
df_us$Accessibility = as.factor(df_us$Accessibility)
df_us$Accessibility_check = NULL


# Turn into single select
unlistvar(df_us$MentalHealth_check) 
mentally_unhealthy = c(
  "I have a concentration and/or memory disorder (e.g. ADHD)",
  "I have an anxiety disorder",
  "I have a mood or emotional disorder (e.g. depression, bipolar disorder)",
  "I have autism / an autism spectrum disorder (e.g. Asperger's)",
  "Or, in your own words:"
)
df_us$MentalHealth[df_us$MentalHealth != "Prefer not to say" &
                      df_us$MentalHealth != "None of the above"] = "Suffer Mental Health Disorder(s)"
df_us$MentalHealth = as.factor(df_us$MentalHealth)
df_us$MentalHealth_check = NULL






# 5 variables from this. if these words show up in my list, then mark as 1
# is.element function if else

# TO PREDICT LEADERSHIP, WE NEED TO LOOK AT PROGRESS
# look at how quickly you progress to get to leadership




############################## DEAL WITH N/AS ##################################

############## Examine N/As ##############
sapply(df_us, function(z)
  (sum(is.na(z)) / length(z)))*100
sapply(df_us, function(z)
  sum(is.na(z)))

############## DROP/IMPUTE N/As ##############
# TODO: look into imputations later
df_us = df_us %>% drop_na()











################################################################################
############################# COUNTRIES APPROACH ###############################

# drop location first for analysis, will get back to this
df_world = readRDS("../20_intermediate_files/initial_data_set.rds")
state_level = c("US_State", "UK_Country")
df_world[state_level] = NULL


################################################################################
######################### CHOOSE ONE OF TWO APPROACHES #########################

# For now, I will go with the U.S. dataset
df = df_us

# keep only complete data points not counting for location
saveRDS(df, "../20_intermediate_files/clean_USdata.rds")
