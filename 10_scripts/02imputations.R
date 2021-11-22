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