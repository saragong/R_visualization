#### run.R ####
#### this script calls read.R, extract.R, recode.R, construct.R, and convert.R
#### in order to clean the dataset psedii.scrn.ABCDE.SAV
#### outputs: all_waves_wide.csv (panel data in wide format)
####          all_waves_long.csv (panel data in long format)

# initialize
rm(list=ls())
options(scipen=999)

# load packages
library(rstudioapi)
library(haven)
library(dplyr)
library(tibble)
library(sjlabelled)
library(stargazer)
library(ggplot2)
library(stringr)
library(panelr)
library(Hmisc)
library(plm)


# set path
current_path <- getActiveDocumentContext()$path
if (current_path != getwd())
{setwd(dirname(current_path ))}

# load and clean data
source("read.R") # slow, so run only once per session
source("extract.R")
source("recode.R")
source("construct.R") 
source("convert.R") # convert from wide to long

# output data as csv
write.csv(all_waves, "all_waves_wide.csv")
write.csv(all_waves_long, "all_waves_long.csv")

# end