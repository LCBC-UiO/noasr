## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(eval=F)

## ----"library", eval=TRUE------------------------------------------------
library(MOAS)
library(tidyverse) #Recommended


## ----"Load RData"--------------------------------------------------------
#  # Loads in the data, and also stores the name of the data.frame in the object `nm`
#  nm = load("LCBC/Projects/Cross_projects/MOAS/Data/MOAS.RData")
#  
#  # Get the data with the name `nm` and assign it to `data`
#  data = get(nm)
#  

## ----"help"--------------------------------------------------------------
#  # Base R
#  ?anova
#  ?t.test
#  
#  # MOAS functions
#  ?widen
#  ?fs_lmm

## ----"widen examplel1"---------------------------------------------------
#  widened_data = widen(data, by="Site_Name")

## ----"widen examplel2"---------------------------------------------------
#  widened_data1 = widen(data, by="Site_Name")
#  widened_data2 = widen(widened_data1, by="Project_Wave")

## ----"site_keeper example1"----------------------------------------------
#  simple_data = site_keeper(data, "long")
#  simple_data = site_keeper(data, "ousAvanto")
#  simple_data = site_keeper(data, "ourPrisma")

## ----"pipeExample"-------------------------------------------------------
#  simple_data = data %>%
#    filter(Project_Name %in% c("MemC","MemP")) %>%
#    na.col.rm() %>% # see section on Utility functions
#    site_keeper("ousPrisma") %>%
#    widen("Project_Wave")
#  

## ----"na.col.rm"---------------------------------------------------------
#  data2 = data %>% filter(Project_Name %in% "NCP") %>% na.col.rm()

## ----"count_chars", eval=F-----------------------------------------------
#  strings = c("This is a vector","containing two strings")
#  strings %>% count_chars()
#  

## ----"factor_times", eval=T, warning=F-----------------------------------
times_hhmm = c("22:40","19:30","08:21","02:47","11:45","13:12")
factor_times(times_hhmm)

## ----"calc_hour", eval=T, warning=F--------------------------------------
times_hhmm = c("22:40","19:30","08:21","02:47","11:45","13:12")
calc_hour(times_hhmm)

