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

# 1. Meta-analyze pro- and anti-ratings separately (as opposed to difference score) ----

# This is adapted from 002_ml4analysis.R

#read in deidentified aggregate dataset
merged <- readRDS("./data/public/merged_deidentified_subset.rds")

# Function for analyzing pro- and anti-ratings separately a la analyse() ----
analyse_separately <- function(data) {
  # Make means, sds, and ns
  sumstats <- group_by(data, location, source, ms_condition) %>% 
    summarize(n_pro = sum(!is.na(proauth_avg)),
              mean_pro = mean(proauth_avg, na.rm = T),
              sd_pro = sd(proauth_avg, na.rm = T),
              n_anti = sum(!is.na(antiauth_avg)),
              mean_anti = mean(antiauth_avg, na.rm = T),
              sd_anti = sd(antiauth_avg, na.rm = T),
              expert = first(expert)
    ) %>% 
    pivot_longer(cols = n_pro:sd_anti) %>% 
    unite(name, name, ms_condition) %>% 
    pivot_wider(names_from = name,
                values_from = value) 
  
  # Make t, df, and pval
  # NOTE: p-value is two-tailed
  # nhst <- group_by(data, source) %>% 
  #   summarize(t_pro  = t.test(proauth_avg ~ ms_condition)$statistic,
  #             df_pro = t.test(proauth_avg ~ ms_condition)$parameter,
  #             p.value_pro = t.test(proauth_avg ~ ms_condition)$p.value,
  #             t_anti  = t.test(antiauth_avg ~ ms_condition)$statistic,
  #             df_anti = t.test(antiauth_avg ~ ms_condition)$parameter,
  #             p.value_anti = t.test(antiauth_avg ~ ms_condition)$p.value)
  
  # combine stats
  # dat <- left_join(sumstats, nhst, by = "source")
  
  # pivot it around to make it easier to calculate ESes
  dat <- sumstats %>% 
    # gather columns and separate pro-author from anti-author
    pivot_longer(n_pro_ms:sd_anti_tv) %>% 
    separate(name, into = c("stat", "outcome", "condition"), sep = "_", extra = "merge") %>%
    unite(col = "stat", stat, condition) %>% 
    pivot_wider(names_from = stat, values_from = value)
  
  # use metafor::escalc() to add effect size and precision
  dat <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                sd1i = sd_ms, sd2i = sd_tv, data = dat, measure = "SMD", 
                append = TRUE)
  
  # make pretty names per site
  dat <- mutate(dat, 
                sitesource_label = case_when(source == "ufl" ~ "University of Florida",
                                             source == "occid" ~ "Occidental College",
                                             source == "ashland" ~ "Ashland University",
                                             source == "ithaca" ~ "Ithaca College",
                                             source == "riverside" ~ "University of California, Riverside",
                                             source == "wesleyan_inhouse" ~ "Wesleyan University",
                                             source == "uwmadison_expert" ~ "University of Wisconsin",
                                             source == "uwmadison_inhouse" ~ "University of Wisconsin",
                                             source == "vcu" ~ "Virginia Commonwealth University",
                                             source == "sou_inhouse" ~ "Southern Oregon University",
                                             source == "plu" ~ "Pacific Lutheran University",
                                             source == "byui" ~ "Brigham Young University - Idaho",
                                             source == "azusa" ~ "Azusa Pacific University",
                                             source == "cnj" ~ "The College of New Jersey",
                                             source == "wpi" ~ "Worcester Polytechnic Institute",
                                             source == "illinois" ~ "University of Illinois",
                                             source == "kansas_expert" ~ "University of Kansas",
                                             source == "kansas_inhouse" ~ "University of Kansas",
                                             source == "upenn" ~ "University of Pennsylvania",
                                             source == "pace_inhouse" ~ "Pace University",
                                             source == "pace_expert" ~ "Pace University")
  )
  
  # return analysed dataset
  return(dat)
}


# Create the datasets under each exclusion rule ----
combinedresults_sep0 <- filter(merged, !is.na(pro_minus_anti)) %>% 
  analyse_separately()
combinedresults_sep1 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER1 == T | expert == 0) %>% 
  analyse_separately()
combinedresults_sep2 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER2 == T | expert == 0) %>% 
  analyse_separately()
combinedresults_sep3 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER3 == T | expert == 0) %>%  
  analyse_separately()

# separate into "pro" and "anti" datasets.
combinedresults_pro0 <- filter(combinedresults_sep0, outcome == "pro")
combinedresults_pro1 <- filter(combinedresults_sep1, outcome == "pro")
combinedresults_pro2 <- filter(combinedresults_sep2, outcome == "pro")
combinedresults_pro3 <- filter(combinedresults_sep3, outcome == "pro")

combinedresults_anti0 <- filter(combinedresults_sep0, outcome == "anti")
combinedresults_anti1 <- filter(combinedresults_sep1, outcome == "anti")
combinedresults_anti2 <- filter(combinedresults_sep2, outcome == "anti")
combinedresults_anti3 <- filter(combinedresults_sep3, outcome == "anti")

# saves all to .csv file
write.csv(combinedresults_pro0, "./data/public/combinedresults_pro0.csv", row.names = FALSE)
write.csv(combinedresults_pro1, "./data/public/combinedresults_pro1.csv", row.names = FALSE)
write.csv(combinedresults_pro2, "./data/public/combinedresults_pro2.csv", row.names = FALSE)
write.csv(combinedresults_pro3, "./data/public/combinedresults_pro3.csv", row.names = FALSE)


# analyze pro-author ratings ----
# reads in csv files from above, just to confirm we can start with those files
combinedresults_pro0 <- read.csv("./data/public/combinedresults_pro0.csv")
combinedresults_pro1 <- read.csv("./data/public/combinedresults_pro1.csv")
combinedresults_pro2 <- read.csv("./data/public/combinedresults_pro2.csv")
combinedresults_pro3 <- read.csv("./data/public/combinedresults_pro3.csv")

# analyses repeated for each set of exclusion critera
# three-level random-effects meta-analysis in MetaSEM
# TODO: drop cluster=location due to OpenMX status = 5
# TODO: refactor this code to parallel that of 002, 
#    saving objects as fixed1_ih_pro, mixed2_aa_pro, etc
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro0))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro1))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro2))
summary( meta3(y=yi, v=vi, cluster=location, data=combinedresults_pro3))
#Notes: I? for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 

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

# TODO: I, Joe, don't know what to do with all this vvvv
# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence.
# This is a simple alternative and useful for most stringent exclusion criteria 
#    which drastically reduces overall N (exclusion set 3)
# read in .rds data
data <- readRDS("./data/public/merged_deidentified_subset.rds")
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
data <- filter(data, pass_ER1 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
data <- filter(data, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$proauth_avg~data$ms_condition)
describeBy(data$proauth_avg, group = data$ms_condition)
effsize::cohen.d(data$proauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
data <- filter(data, pass_ER3 == T)
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

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
# Read data, applying exclusion criteria 1
data <- filter(merged, pass_ER1 == T)
# Selecting only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert)
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
data <- subset(data, data$source %in% c("uwmadison_inhouse", "cnj", "kansas_expert", "kansas_inhouse",
                                        "pace_expert", "vcu"))
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
# Analyzing anti- ratings now
################################################################################

#read in deidentified aggregate dataset
merged <- readRDS("./data/public/merged_deidentified_subset.rds")

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
data <- readRDS("./data/public/merged_deidentified_subset.rds")
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
data <- filter(data, pass_ER1 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
data <- filter(data, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
data <- filter(data, pass_ER3 == T)
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

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
# Read data, applying exclusion criteria 1
data <- filter(merged, pass_ER1 == T)
# Selecting only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert)
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
data <- subset(data, data$source %in% c("uwmadison_inhouse", "cnj", "kansas_expert", "kansas_inhouse",
                                        "pace_expert", "vcu"))
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
data$ms_condition <- factor(data$ms_condition, levels = c("ms", "tv"))
# Analyses using that subset
t.test(data$antiauth_avg~data$ms_condition)
describeBy(data$antiauth_avg, group = data$ms_condition)
effsize::cohen.d(data$antiauth_avg~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this was previously incorrectly indicating a positive value? Had to manually reverse for dissertation but seems fine now


## 2. Analyzing how much participants liked the pro and anti authors ----
# Read in data to start from scratch
merged <- readRDS("./data/public/merged_deidentified_subset.rds")

# Uncomment either line if you want to subset to only in house or author advised sites
# merged <- filter(merged, expert == 1)
# merged <- filter(merged, expert == 0)

# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
merged <- filter(merged, pass_ER1 == T)

# 'merged_excl_2' further excludes participants as per exclusion set 2 (below)
merged_excl_2 <- filter(merged, pass_ER2 == T)

# 'merged_excl_3' further excludes participants as per exclusion set 3 (below)
merged_excl_3 <- filter(merged_excl_2, pass_ER3 == T)

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


# 3. Exploratory analysis excluding two sites where ms condition had lower sadness ----
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
summary( meta(y=yi, v=vi, data=filter(combinedresults1, source != "kansas_expert" & source != "byui")))
summary( meta(y=yi, v=vi, data=filter(combinedresults2, source != "kansas_expert" & source != "byui")))
summary( meta(y=yi, v=vi, data=filter(combinedresults3, source != "kansas_expert" & source != "byui")))
sink()
