# Many Labs 4 Analysis Script - Supplement
# Coder: Rick Klein raklein22@gmail.com
# OSF: https://osf.io/8ccnw/

#ANALYSIS SCRIPT - Supplement
# This script contains _most_ supplemental analyses. At the moment, some 
# remain in the main script, and this only includes analyses performed after
# the dissertation was accepted.

# INDEX
# 1. Analysis repeated for only pro- US author ratings (instead of difference score)
# 2. Analysis repeated for only anti- US author ratings
# 3. Analyses of how many participants preferred the pro- author vs the anti- author

# Open the .rproj file in R Studio to avoid setting the working directory.
# Otherwise, call setwd() with a path to the /ml4/ folder
# All file paths are relative from the working directory.

library(metafor)
library(metaSEM)
library(haven)
library(psych)
library(dplyr)
library(effsize)
library(GPArotation)
library(tidyverse)

################################################################################
#
# First suppl analysis: repeat the primary meta-analyses for pro- and anti- 
# ratings separately (as opposed to creating a difference score)
#
################################################################################

# I'm re-using the primary analysis script and just editing it. Admittedly, this
# is not the most efficient code, but it should work.

# Implementing pro-only first:

#read in deidentified aggregate dataset
merged <- readRDS("./data/public/merged_deidentified.rds")

###ANALYSIS 0: no exclusions###

#Function to generate required stats for meta-analysis. No exclusions.
analysis_pro0 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'tv']) #n for tv condition
  n_ms <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'ms']) #n for ms condition
  sd_tv <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv'], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms'], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv'], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms'], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$proauth_avg~data$ms_condition, subset = data$source==sitesource)
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#above function is run for each site identifier
riverside_results <- analysis_pro0(merged, "riverside")
azusa_results <- analysis_pro0(merged, "azusa")
cnj_results <- analysis_pro0(merged, "cnj")
illinois_results <- analysis_pro0(merged, "illinois")
ithaca_results <- analysis_pro0(merged, "ithaca")
kansas_inhouse_results <- analysis_pro0(merged, "kansas_inhouse")
occid_results <- analysis_pro0(merged, "occid")
pace_expert_results <- analysis_pro0(merged, "pace_expert")
sou_inhouse_results <- analysis_pro0(merged, "sou_inhouse")
ufl_results <- analysis_pro0(merged, "ufl")
upenn_results <- analysis_pro0(merged, "upenn")
uwmadison_expert_results <- analysis_pro0(merged, "uwmadison_expert")
uwmadison_inhouse_results <- analysis_pro0(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_pro0(merged, "wesleyan_inhouse")
wpi_results <- analysis_pro0(merged, "wpi")
kansas_expert_results <- analysis_pro0(merged, "kansas_expert")
plu_results <- analysis_pro0(merged, "plu")
ashland_results <- analysis_pro0(merged, "ashland")
vcu_results <- analysis_pro0(merged, "vcu")
byui_results <- analysis_pro0(merged, "byui")
pace_inhouse_results <- analysis_pro0(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_pro0 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

#Computing SE and sampling variance with metafor package.
# yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# n1i numeric number of participants in the intervention group
# m1i numeric mean number of days off work/school in the intervention group
# sd1i numeric standard deviation of the number of days off work/school in the intervention group
# n2i numeric number of participants in the control/comparison group
# m2i numeric mean number of days off work/school in the control/comparison group
# sd2i numeric standard deviation of the number of days off work/school in the control/comparison group
#Appends yi and vi to the data object.
combinedresults_pro0 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_pro0, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults_pro0, "./data/public/combinedresults_pro0.csv", row.names = FALSE)

###ANALYSIS 1: Exclusion set 1###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors)

#Function to generate required stats for meta-analysis.
analysis_pro1 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)]) #n for tv condition
  n_ms <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)]) #n for ms condition
  sd_tv <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert).
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$proauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#above function is run for each site identifier
riverside_results <- analysis_pro1(merged, "riverside")
azusa_results <- analysis_pro1(merged, "azusa")
cnj_results <- analysis_pro1(merged, "cnj")
illinois_results <- analysis_pro1(merged, "illinois")
ithaca_results <- analysis_pro1(merged, "ithaca")
kansas_inhouse_results <- analysis_pro1(merged, "kansas_inhouse")
occid_results <- analysis_pro1(merged, "occid")
pace_expert_results <- analysis_pro1(merged, "pace_expert")
sou_inhouse_results <- analysis_pro1(merged, "sou_inhouse")
ufl_results <- analysis_pro1(merged, "ufl")
upenn_results <- analysis_pro1(merged, "upenn")
uwmadison_expert_results <- analysis_pro1(merged, "uwmadison_expert")
uwmadison_inhouse_results <- analysis_pro1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_pro1(merged, "wesleyan_inhouse")
wpi_results <- analysis_pro1(merged, "wpi")
kansas_expert_results <- analysis_pro1(merged, "kansas_expert")
plu_results <- analysis_pro1(merged, "plu")
ashland_results <- analysis_pro1(merged, "ashland")
vcu_results <- analysis_pro1(merged, "vcu")
byui_results <- analysis_pro1(merged, "byui")
pace_inhouse_results <- analysis_pro1(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_pro1 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_pro1 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_pro1, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults_pro1, "./data/public/combinedresults_pro1.csv", row.names = FALSE)

###ANALYSIS 2: Exclusion set 2###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors
#3. Identify as White (race == 1)
#4. Born in USA (countryofbirth == 1)

#Function to generate required stats for meta-analysis.
analysis_pro2 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1]) #n for tv condition
  n_ms <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1]) #n for ms condition
  sd_tv <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$proauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#in-house sites don't necessarily have the data necessary to implement these exclusions
#Below, analysis1 (basic exclusions) is run for in-house, while analysis 2 is run for expert versions

#expert sites
riverside_results <- analysis_pro2(merged, "riverside")
cnj_results <- analysis_pro2(merged, "cnj")
occid_results <- analysis_pro2(merged, "occid")
pace_expert_results <- analysis_pro2(merged, "pace_expert")
uwmadison_expert_results <- analysis_pro2(merged, "uwmadison_expert")
kansas_expert_results <- analysis_pro2(merged, "kansas_expert")
ashland_results <- analysis_pro2(merged, "ashland")
vcu_results <- analysis_pro2(merged, "vcu")
byui_results <- analysis_pro2(merged, "byui")

#inhouse sites
azusa_results <- analysis_pro1(merged, "azusa")
illinois_results <- analysis_pro1(merged, "illinois")
ithaca_results <- analysis_pro1(merged, "ithaca")
kansas_inhouse_results <- analysis_pro1(merged, "kansas_inhouse")
sou_inhouse_results <- analysis_pro1(merged, "sou_inhouse")
ufl_results <- analysis_pro1(merged, "ufl")
upenn_results <- analysis_pro1(merged, "upenn")
uwmadison_inhouse_results <- analysis_pro1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_pro1(merged, "wesleyan_inhouse")
wpi_results <- analysis_pro1(merged, "wpi")
plu_results <- analysis_pro1(merged, "plu")
pace_inhouse_results <- analysis_pro1(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_pro2 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_pro2 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_pro2, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults_pro2, "./data/public/combinedresults_pro2.csv", row.names = FALSE)

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item

# Function to generate required stats for meta-analysis.
analysis_pro3 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7]) #n for tv condition
  n_ms <- length(data$proauth_avg[!is.na(data$proauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7]) #n for ms condition
  sd_tv <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$proauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$proauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) # results to be reported
  return(result)
}

# in-house sites don't necessarily have the data necessary to implement these exclusions
# Below, analysis1 (basic exclusions) is run for in-house, while analysis 3 is run for expert versions

# expert sites
riverside_results <- analysis_pro3(merged, "riverside")
cnj_results <- analysis_pro3(merged, "cnj")
occid_results <- analysis_pro3(merged, "occid")
pace_expert_results <- analysis_pro3(merged, "pace_expert")
uwmadison_expert_results <- analysis_pro3(merged, "uwmadison_expert")
kansas_expert_results <- analysis_pro3(merged, "kansas_expert")
ashland_results <- analysis_pro3(merged, "ashland")
vcu_results <- analysis_pro3(merged, "vcu")
byui_results <- analysis_pro3(merged, "byui")

# inhouse sites
azusa_results <- analysis_pro1(merged, "azusa")
illinois_results <- analysis_pro1(merged, "illinois")
ithaca_results <- analysis_pro1(merged, "ithaca")
kansas_inhouse_results <- analysis_pro1(merged, "kansas_inhouse")
sou_inhouse_results <- analysis_pro1(merged, "sou_inhouse")
ufl_results <- analysis_pro1(merged, "ufl")
upenn_results <- analysis_pro1(merged, "upenn")
uwmadison_inhouse_results <- analysis_pro1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_pro1(merged, "wesleyan_inhouse")
wpi_results <- analysis_pro1(merged, "wpi")
plu_results <- analysis_pro1(merged, "plu")
pace_inhouse_results <- analysis_pro1(merged, "pace_inhouse")

# merges results from above into a single data frame
combinedresults_pro3 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_pro3 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_pro3, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults_pro3, "./data/public/combinedresults_pro3.csv", row.names = FALSE)

# reads in csv files from above, just to confirm we can start with those files
combinedresults_pro0 <- read.csv("./data/public/combinedresults_pro0.csv")
combinedresults_pro1 <- read.csv("./data/public/combinedresults_pro1.csv")
combinedresults_pro2 <- read.csv("./data/public/combinedresults_pro2.csv")
combinedresults_pro3 <- read.csv("./data/public/combinedresults_pro3.csv")

# analyses repeated for each set of exclusion critera
# three-level random-effects meta-analysis in MetaSEM
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro0))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro1))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro2))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro3))
#Notes: I? for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 

# # forest plots for each
# ### All forest plots now outdated in favor of metaviz.R
# ### All forest plots now outdated in favor of metaviz.R
# ### All forest plots now outdated in favor of metaviz.R
# data <- combinedresults1
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb1.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-3.3, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()
# 
# data <- combinedresults2
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb2.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-5.1, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(6.6, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()
# 
# data <- combinedresults3
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb3.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-6, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(6.5, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()

# a covariate of study version (in-house or expert-designed) is added to create a three-level mixed-effects meta-analysis
# note the openMX status, sometimes indicates a potential problem
summary( mixed_pro0 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro0))
summary( mixed_pro1 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro1))
summary( mixed_pro2 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro2))
summary( mixed_pro3 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro3))
# Notes: The R? for the version predictor will be reported for both level 2 and level 3, although in this case version is a level 2 predictor so the level 3 R? will always be zero. 

# constraining the variance to test if it significantly worsens the model
summary( fixed_pro0 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro0, RE2.constraints=0, RE3.constraints=0))
summary( fixed_pro1 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro1, RE2.constraints=0, RE3.constraints=0))
summary( fixed_pro2 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro2, RE2.constraints=0, RE3.constraints=0))
summary( fixed_pro3 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_pro3, RE2.constraints=0, RE3.constraints=0))

# compare if there is a significant difference in model fit, chi square difference test
anova(mixed_pro0, fixed_pro0)
anova(mixed_pro1, fixed_pro1)
anova(mixed_pro2, fixed_pro2)
anova(mixed_pro3, fixed_pro3)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence.
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
data <- readRDS("./data/public/merged_deidentified.rds")
# selecting only expert labs
data <- subset(data, expert==1)

###ANALYSIS 0: no exclusions###
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White (race == 1)
data <- subset(data, data$race == 1)
# 4. Born in USA (countryofbirth == 1)
data <- subset(data, data$countryofbirth == 1)
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item
data <- subset(data, data$americanid >= 7)
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a positive value, reversing sign in the report

###Conducting a small meta-analysis of only the in-house data to provide a summary of those results in basic form.####
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
data <- read.csv("./data/public/combinedresults_pro1.csv")
# subset to in-house rows only
data <- subset(data, expert==0)
# conduct random effects meta-analyis
summary( meta(y = yi, v = vi, data = data))

# # forest plot
# dev.off()
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(x= data$yi, vi=data$vi, slab=data$location)
# par(cex=1, font=2)#bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# 
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/inhousemeta.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$location))
# par(cex=1, font=2) #bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()

# sample funnel plot 
# funnel(rma(yi= data$yi, vi=data$vi, slab=data$location))

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
# Read data
data <- merged
# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
# Selecting only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert)
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
data <- subset(data, data$source=="uwmadison_inhouse" | data$source=="cnj" | data$source=="kansas_expert" | data$source=="kansas_inhouse" | data$source=="pace_expert" | data$source=="vcu")
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
data$ms_condition <- factor(data$ms_condition, levels = c("ms", "tv"))
# Analyses using that subset
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this was previously incorrectly indicating a positive value? Had to manually reverse for dissertation but seems fine now





################################################################################
################################################################################
################################################################################
# Analyzing anti- ratings now
################################################################################
################################################################################
################################################################################






#read in deidentified aggregate dataset
merged <- readRDS("./data/public/merged_deidentified.rds")

###ANALYSIS 0: no exclusions###

#Function to generate required stats for meta-analysis. No exclusions.
analysis_anti0 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'tv']) #n for tv condition
  n_ms <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'ms']) #n for ms condition
  sd_tv <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv'], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms'], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv'], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms'], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$antiauth_avg~data$ms_condition, subset = data$source==sitesource)
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#above function is run for each site identifier
riverside_results <- analysis_anti0(merged, "riverside")
azusa_results <- analysis_anti0(merged, "azusa")
cnj_results <- analysis_anti0(merged, "cnj")
illinois_results <- analysis_anti0(merged, "illinois")
ithaca_results <- analysis_anti0(merged, "ithaca")
kansas_inhouse_results <- analysis_anti0(merged, "kansas_inhouse")
occid_results <- analysis_anti0(merged, "occid")
pace_expert_results <- analysis_anti0(merged, "pace_expert")
sou_inhouse_results <- analysis_anti0(merged, "sou_inhouse")
ufl_results <- analysis_anti0(merged, "ufl")
upenn_results <- analysis_anti0(merged, "upenn")
uwmadison_expert_results <- analysis_anti0(merged, "uwmadison_expert")
uwmadison_inhouse_results <- analysis_anti0(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_anti0(merged, "wesleyan_inhouse")
wpi_results <- analysis_anti0(merged, "wpi")
kansas_expert_results <- analysis_anti0(merged, "kansas_expert")
plu_results <- analysis_anti0(merged, "plu")
ashland_results <- analysis_anti0(merged, "ashland")
vcu_results <- analysis_anti0(merged, "vcu")
byui_results <- analysis_anti0(merged, "byui")
pace_inhouse_results <- analysis_anti0(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_anti0 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

#Computing SE and sampling variance with metafor package.
# yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# n1i numeric number of participants in the intervention group
# m1i numeric mean number of days off work/school in the intervention group
# sd1i numeric standard deviation of the number of days off work/school in the intervention group
# n2i numeric number of participants in the control/comparison group
# m2i numeric mean number of days off work/school in the control/comparison group
# sd2i numeric standard deviation of the number of days off work/school in the control/comparison group
#Appends yi and vi to the data object.
combinedresults_anti0 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                               sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_anti0, measure = "SMD", 
                               append = TRUE)

#saves .csv file
write.csv(combinedresults_anti0, "./data/public/combinedresults_anti0.csv", row.names = FALSE)

###ANALYSIS 1: Exclusion set 1###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors)

#Function to generate required stats for meta-analysis.
analysis_anti1 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)]) #n for tv condition
  n_ms <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)]) #n for ms condition
  sd_tv <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert).
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$antiauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#above function is run for each site identifier
riverside_results <- analysis_anti1(merged, "riverside")
azusa_results <- analysis_anti1(merged, "azusa")
cnj_results <- analysis_anti1(merged, "cnj")
illinois_results <- analysis_anti1(merged, "illinois")
ithaca_results <- analysis_anti1(merged, "ithaca")
kansas_inhouse_results <- analysis_anti1(merged, "kansas_inhouse")
occid_results <- analysis_anti1(merged, "occid")
pace_expert_results <- analysis_anti1(merged, "pace_expert")
sou_inhouse_results <- analysis_anti1(merged, "sou_inhouse")
ufl_results <- analysis_anti1(merged, "ufl")
upenn_results <- analysis_anti1(merged, "upenn")
uwmadison_expert_results <- analysis_anti1(merged, "uwmadison_expert")
uwmadison_inhouse_results <- analysis_anti1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_anti1(merged, "wesleyan_inhouse")
wpi_results <- analysis_anti1(merged, "wpi")
kansas_expert_results <- analysis_anti1(merged, "kansas_expert")
plu_results <- analysis_anti1(merged, "plu")
ashland_results <- analysis_anti1(merged, "ashland")
vcu_results <- analysis_anti1(merged, "vcu")
byui_results <- analysis_anti1(merged, "byui")
pace_inhouse_results <- analysis_anti1(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_anti1 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_anti1 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                               sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_anti1, measure = "SMD", 
                               append = TRUE)

#saves .csv file
write.csv(combinedresults_anti1, "./data/public/combinedresults_anti1.csv", row.names = FALSE)

###ANALYSIS 2: Exclusion set 2###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors
#3. Identify as White (race == 1)
#4. Born in USA (countryofbirth == 1)

#Function to generate required stats for meta-analysis.
analysis_anti2 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1]) #n for tv condition
  n_ms <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1]) #n for ms condition
  sd_tv <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$antiauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

#in-house sites don't necessarily have the data necessary to implement these exclusions
#Below, analysis1 (basic exclusions) is run for in-house, while analysis 2 is run for expert versions

#expert sites
riverside_results <- analysis_anti2(merged, "riverside")
cnj_results <- analysis_anti2(merged, "cnj")
occid_results <- analysis_anti2(merged, "occid")
pace_expert_results <- analysis_anti2(merged, "pace_expert")
uwmadison_expert_results <- analysis_anti2(merged, "uwmadison_expert")
kansas_expert_results <- analysis_anti2(merged, "kansas_expert")
ashland_results <- analysis_anti2(merged, "ashland")
vcu_results <- analysis_anti2(merged, "vcu")
byui_results <- analysis_anti2(merged, "byui")

#inhouse sites
azusa_results <- analysis_anti1(merged, "azusa")
illinois_results <- analysis_anti1(merged, "illinois")
ithaca_results <- analysis_anti1(merged, "ithaca")
kansas_inhouse_results <- analysis_anti1(merged, "kansas_inhouse")
sou_inhouse_results <- analysis_anti1(merged, "sou_inhouse")
ufl_results <- analysis_anti1(merged, "ufl")
upenn_results <- analysis_anti1(merged, "upenn")
uwmadison_inhouse_results <- analysis_anti1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_anti1(merged, "wesleyan_inhouse")
wpi_results <- analysis_anti1(merged, "wpi")
plu_results <- analysis_anti1(merged, "plu")
pace_inhouse_results <- analysis_anti1(merged, "pace_inhouse")

#merges results from above into a single data frame
combinedresults_anti2 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_anti2 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                               sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_anti2, measure = "SMD", 
                               append = TRUE)

# saves .csv file
write.csv(combinedresults_anti2, "./data/public/combinedresults_anti2.csv", row.names = FALSE)

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item

# Function to generate required stats for meta-analysis.
analysis_anti3 <- function(data, sitesource)
{
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7]) #n for tv condition
  n_ms <- length(data$antiauth_avg[!is.na(data$antiauth_avg) & data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7]) #n for ms condition
  sd_tv <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #sd for tv participants at that site
  sd_ms <- sd(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #sd for ms participants at that site
  mean_tv <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'tv' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #mean for tv participants at that site
  mean_ms <- mean(data$antiauth_avg[data$source==sitesource & data$ms_condition == 'ms' & (data$msincomplete == 0 | is.na(data$msincomplete)) & !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7], na.rm = TRUE) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$antiauth_avg~data$ms_condition, subset = data$source==sitesource & (data$msincomplete == 0 | is.na(data$msincomplete)) & (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) & data$race == 1 & data$countryofbirth == 1 & data$americanid >= 7))
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) # results to be reported
  return(result)
}

# in-house sites don't necessarily have the data necessary to implement these exclusions
# Below, analysis1 (basic exclusions) is run for in-house, while analysis 3 is run for expert versions

# expert sites
riverside_results <- analysis_anti3(merged, "riverside")
cnj_results <- analysis_anti3(merged, "cnj")
occid_results <- analysis_anti3(merged, "occid")
pace_expert_results <- analysis_anti3(merged, "pace_expert")
uwmadison_expert_results <- analysis_anti3(merged, "uwmadison_expert")
kansas_expert_results <- analysis_anti3(merged, "kansas_expert")
ashland_results <- analysis_anti3(merged, "ashland")
vcu_results <- analysis_anti3(merged, "vcu")
byui_results <- analysis_anti3(merged, "byui")

# inhouse sites
azusa_results <- analysis_anti1(merged, "azusa")
illinois_results <- analysis_anti1(merged, "illinois")
ithaca_results <- analysis_anti1(merged, "ithaca")
kansas_inhouse_results <- analysis_anti1(merged, "kansas_inhouse")
sou_inhouse_results <- analysis_anti1(merged, "sou_inhouse")
ufl_results <- analysis_anti1(merged, "ufl")
upenn_results <- analysis_anti1(merged, "upenn")
uwmadison_inhouse_results <- analysis_anti1(merged, "uwmadison_inhouse")
wesleyan_inhouse_results <- analysis_anti1(merged, "wesleyan_inhouse")
wpi_results <- analysis_anti1(merged, "wpi")
plu_results <- analysis_anti1(merged, "plu")
pace_inhouse_results <- analysis_anti1(merged, "pace_inhouse")

# merges results from above into a single data frame
combinedresults_anti3 <- rbind(
  ashland_results,
  azusa_results,
  cnj_results,
  illinois_results,
  ithaca_results,
  kansas_expert_results,
  kansas_inhouse_results,
  occid_results,
  pace_expert_results,
  plu_results,
  riverside_results,
  sou_inhouse_results,
  ufl_results,
  upenn_results,
  uwmadison_expert_results,
  uwmadison_inhouse_results,
  vcu_results,
  wesleyan_inhouse_results,
  wpi_results,
  byui_results,
  pace_inhouse_results
)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults_anti3 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                               sd1i = sd_ms, sd2i = sd_tv, data = combinedresults_anti3, measure = "SMD", 
                               append = TRUE)

# saves .csv file
write.csv(combinedresults_anti3, "./data/public/combinedresults_anti3.csv", row.names = FALSE)

# reads in csv files from above, just to confirm we can start with those files
combinedresults_anti0 <- read.csv("./data/public/combinedresults_anti0.csv")
combinedresults_anti1 <- read.csv("./data/public/combinedresults_anti1.csv")
combinedresults_anti2 <- read.csv("./data/public/combinedresults_anti2.csv")
combinedresults_anti3 <- read.csv("./data/public/combinedresults_anti3.csv")

# analyses repeated for each set of exclusion critera
# three-level random-effects meta-analysis in MetaSEM
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_anti0))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_anti1))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_anti2))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_anti3))
#Notes: I? for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 

# # forest plots for each
# ### All forest plots now outdated in favor of metaviz.R
# ### All forest plots now outdated in favor of metaviz.R
# ### All forest plots now outdated in favor of metaviz.R
# data <- combinedresults1
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb1.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-3.3, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()
# 
# data <- combinedresults2
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb2.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-5.1, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(6.6, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()
# 
# data <- combinedresults3
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/comb3.randomeffects.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$sitesource))
# par(cex=1, font=2) #bold font
# text(-6, 20.5, "Location",  pos=4) #adds location label using x, y coord
# text(6.5, 20.5, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()

# a covariate of study version (in-house or expert-designed) is added to create a three-level mixed-effects meta-analysis
# note the openMX status, sometimes indicates a potential problem
summary( mixed_anti0 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti0))
summary( mixed_anti1 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti1))
summary( mixed_anti2 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti2))
summary( mixed_anti3 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti3))
# Notes: The R? for the version predictor will be reported for both level 2 and level 3, although in this case version is a level 2 predictor so the level 3 R? will always be zero. 

# constraining the variance to test if it significantly worsens the model
summary( fixed_anti0 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti0, RE2.constraints=0, RE3.constraints=0))
summary( fixed_anti1 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti1, RE2.constraints=0, RE3.constraints=0))
summary( fixed_anti2 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti2, RE2.constraints=0, RE3.constraints=0))
summary( fixed_anti3 <- meta3(y=yi, v=vi, cluster=location, x=expert, data=combinedresults_anti3, RE2.constraints=0, RE3.constraints=0))

# compare if there is a significant difference in model fit, chi square difference test
anova(mixed_anti0, fixed_anti0)
anova(mixed_anti1, fixed_anti1)
anova(mixed_anti2, fixed_anti2)
anova(mixed_anti3, fixed_anti3)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence.
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
data <- readRDS("./data/public/merged_deidentified.rds")
# selecting only expert labs
data <- subset(data, expert==1)

###ANALYSIS 0: no exclusions###
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White (race == 1)
data <- subset(data, data$race == 1)
# 4. Born in USA (countryofbirth == 1)
data <- subset(data, data$countryofbirth == 1)
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item
data <- subset(data, data$americanid >= 7)
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a positive value, reversing sign in the report

###Conducting a small meta-analysis of only the in-house data to provide a summary of those results in basic form.####
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
data <- read.csv("./data/public/combinedresults_anti1.csv")
# subset to in-house rows only
data <- subset(data, expert==0)
# conduct random effects meta-analyis
summary( meta(y = yi, v = vi, data = data))

# # forest plot
# dev.off()
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(x= data$yi, vi=data$vi, slab=data$location)
# par(cex=1, font=2)#bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# 
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/inhousemeta.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= data$yi, vi=data$vi, slab=data$location))
# par(cex=1, font=2) #bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()

# sample funnel plot 
# funnel(rma(yi= data$yi, vi=data$vi, slab=data$location))

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
# Read data
data <- merged
# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
# Selecting only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert)
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
data <- subset(data, data$source=="uwmadison_inhouse" | data$source=="cnj" | data$source=="kansas_expert" | data$source=="kansas_inhouse" | data$source=="pace_expert" | data$source=="vcu")
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
data$ms_condition <- factor(data$ms_condition, levels = c("ms", "tv"))
# Analyses using that subset
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this was previously incorrectly indicating a positive value? Had to manually reverse for dissertation but seems fine now


#### Analyzing how much participants liked the pro and anti authors
# Read in data to start from scratch
merged <- readRDS("./data/public/merged_deidentified.rds")

# Uncomment either line if you want to subset to only in house or author advised sites
# merged <- filter(merged, expert == 1)
# merged <- filter(merged, expert == 0)

# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
merged <- subset(merged, (merged$msincomplete == 0 | is.na(merged$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
merged <- subset(merged, (!is.na(merged$prous3) & !is.na(merged$prous4) & !is.na(merged$prous5) & !is.na(merged$antius3) & !is.na(merged$antius4) & !is.na(merged$antius5)))

# 'merged_excl_2' further excludes participants as per exclusion set 2 (below)
merged_excl_2 <- subset(merged, (merged$race == 1 & merged$countryofbirth == 1) | merged$expert == 0)

# 'merged_excl_3' further excludes participants as per exclusion set 3 (below)
merged_excl_3 <- subset(merged_excl_2, merged_excl_2$americanid >= 7 | merged_excl_2$expert == 0)

# Investigating only participants who reported preference for the pro-US author.

# generate dfs for Author Advised and In House sites.
merged_aa <- filter(merged, expert == 1)
merged_ih <- filter(merged, expert == 0)

### Percent rating pro-author more highly than anti-author, basic exclusions
# In House
n_profav_ih <- sum(merged_ih$pro_minus_anti > 0)
n_antifav_ih <- sum(merged_ih$pro_minus_anti < 0)
n_nofav_ih <- sum(merged_ih$pro_minus_anti == 0)

pct_profav_ih <- (n_profav_ih/(n_profav_ih+n_antifav_ih+n_nofav_ih))*100
pct_antifav_ih <- (n_antifav_ih/(n_profav_ih+n_antifav_ih+n_nofav_ih))*100
pct_nofav_ih <- (n_nofav_ih/(n_profav_ih+n_antifav_ih+n_nofav_ih))*100

# Author Advised
n_profav_aa <- sum(merged_aa$pro_minus_anti > 0)
n_antifav_aa <- sum(merged_aa$pro_minus_anti < 0)
n_nofav_aa <- sum(merged_aa$pro_minus_anti == 0)

pct_profav_aa <- (n_profav_aa/(n_profav_aa+n_antifav_aa+n_nofav_aa))*100
pct_antifav_aa <- (n_antifav_aa/(n_profav_aa+n_antifav_aa+n_nofav_aa))*100
pct_nofav_aa <- (n_nofav_aa/(n_profav_aa+n_antifav_aa+n_nofav_aa))*100

# Subset to Author Advised participants who preferred the pro-US author 
# and examine if that finds the effect. Repeat for 3 exclusion sets.
# Datasets:
# merged_aa is basic exclusions
# merged_excl_2_aa is exclusion set 2
merged_excl_2_aa <- filter(merged_excl_2, expert == 1)
# merged_excl_3_aa is exclusion set 3
merged_excl_3_aa <- filter(merged_excl_3, expert == 1)

# Analyses for each
merged_aa_ttest <- t.test(merged_aa$pro_minus_anti~merged_aa$ms_condition)
merged_aa_desc <- describeBy(merged_aa$pro_minus_anti, group = merged_aa$ms_condition)
merged_aa_effsize <- effsize::cohen.d(merged_aa$pro_minus_anti~merged_aa$ms_condition,pooled=TRUE,paired=FALSE,na.rm=TRUE, hedges.correction=TRUE,conf.level=0.95)

# Analyses using that subset
merged_excl_2_aa_ttest <- t.test(merged_excl_2_aa$pro_minus_anti~merged_excl_2_aa$ms_condition)
merged_excl_2_aa_desc <- describeBy(merged_excl_2_aa$pro_minus_anti, group = merged_excl_2_aa$ms_condition)
merged_excl_2_aa_effsize <- effsize::cohen.d(merged_excl_2_aa$pro_minus_anti~merged_excl_2_aa$ms_condition,pooled=TRUE,paired=FALSE,na.rm=TRUE, hedges.correction=TRUE,conf.level=0.95)

# Analyses using that subset
merged_excl_3_aa_ttest <- t.test(merged_excl_3_aa$pro_minus_anti~merged_excl_3_aa$ms_condition)
merged_excl_3_aa_desc <- describeBy(merged_excl_3_aa$pro_minus_anti, group = merged_excl_3_aa$ms_condition)
merged_excl_3_aa_effsize <- effsize::cohen.d(merged_excl_3_aa$pro_minus_anti~merged_excl_3_aa$ms_condition,pooled=TRUE,paired=FALSE,na.rm=TRUE, hedges.correction=TRUE,conf.level=0.95)

############
# Exploratory analysis without two sites where ms condition had lower sadness
############
# Note: I think it's quite possible this result could simply be due to random
# variation. However, as below we see including/excluding these samples has 
# no effect on overall results

library(tidyverse)
library(metaSEM)

# Read in files in case you want to start from here.
# these are per-site results using the three exclusion criteria as before
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# For simplicity I'm only examining the univariate random-effect meta-analysis.
# Filtering out kansas_expert and byui sites
# saving these results to a .txt file with sink()
sink("./output/supplement_nokansas_nobyui.txt")
summary( meta(y=yi, v=vi, data=filter(combinedresults1, sitesource != "kansas_expert" & sitesource != "byui")))
summary( meta(y=yi, v=vi, data=filter(combinedresults2, sitesource != "kansas_expert" & sitesource != "byui")))
summary( meta(y=yi, v=vi, data=filter(combinedresults3, sitesource != "kansas_expert" & sitesource != "byui")))
sink()
