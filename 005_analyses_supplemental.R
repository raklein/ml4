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
  sumstats_pro <- group_by(data, location, source, ms_condition) %>% 
    summarize(n = sum(!is.na(proauth_avg)),
              mean = mean(proauth_avg, na.rm = T),
              sd = sd(proauth_avg, na.rm = T),
              expert = first(expert)
    ) %>% 
    pivot_longer(cols = c(n, mean, sd)) %>% 
    unite(name, name, ms_condition) %>% 
    pivot_wider(names_from = name,
                values_from = value) %>% 
    mutate(outcome = "pro")
  
  sumstats_anti <- group_by(data, location, source, ms_condition) %>% 
    summarize(n = sum(!is.na(antiauth_avg)),
              mean = mean(antiauth_avg, na.rm = T),
              sd = sd(antiauth_avg, na.rm = T),
              expert = first(expert)
    ) %>% 
    pivot_longer(cols = c(n, mean, sd)) %>% 
    unite(name, name, ms_condition) %>% 
    pivot_wider(names_from = name,
                values_from = value) %>% 
    mutate(outcome = "anti")
    
  sumstats <- bind_rows(sumstats_pro, sumstats_anti)
  
  # Make t, df, and pval
  # NOTE: p-value is two-tailed
  nhst_pro <- group_by(data, source) %>%
    summarize(t  = t.test(proauth_avg ~ ms_condition)$statistic,
              df = t.test(proauth_avg ~ ms_condition)$parameter,
              p.value = t.test(proauth_avg ~ ms_condition)$p.value) %>% 
    mutate(outcome = "pro")
  
  nhst_anti <- group_by(data, source) %>%
    summarize(t  = t.test(antiauth_avg ~ ms_condition)$statistic,
              df = t.test(antiauth_avg ~ ms_condition)$parameter,
              p.value = t.test(antiauth_avg ~ ms_condition)$p.value) %>% 
    mutate(outcome = "anti")
  
  nhst <- bind_rows(nhst_pro, nhst_anti)

  # combine stats
  dat <- left_join(sumstats, nhst, by = c("source", "outcome"))
  
  # use metafor::escalc() to add effect size and precision
  # Appends yi and vi to the data object
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
  
  # arrange by location and source rather than by outcome
  dat <- arrange(dat, location, source)
  
  # return analysed dataset
  return(dat)
}


# Create the datasets under each exclusion rule ----
# Additionally centers expert for contrast coding
combinedresults_sep0 <- filter(merged, !is.na(pro_minus_anti)) %>% 
  analyse_separately() %>% 
  mutate(expert.ctr = ifelse(expert == 1, 1, -1))
combinedresults_sep1 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER1 == T | expert == 0) %>% 
  analyse_separately() %>% 
  mutate(expert.ctr = ifelse(expert == 1, 1, -1))
combinedresults_sep2 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER2 == T | expert == 0) %>% 
  analyse_separately() %>% 
  mutate(expert.ctr = ifelse(expert == 1, 1, -1))
combinedresults_sep3 <- filter(merged, !is.na(pro_minus_anti),
                               pass_ER3 == T | expert == 0) %>%  
  analyse_separately() %>% 
  mutate(expert.ctr = ifelse(expert == 1, 1, -1))

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

write.csv(combinedresults_anti0, "./data/public/combinedresults_anti0.csv", row.names = FALSE)
write.csv(combinedresults_anti1, "./data/public/combinedresults_anti1.csv", row.names = FALSE)
write.csv(combinedresults_anti2, "./data/public/combinedresults_anti2.csv", row.names = FALSE)
write.csv(combinedresults_anti3, "./data/public/combinedresults_anti3.csv", row.names = FALSE)

# analyze pro-author ratings ----
# reads in csv files from above, just to confirm we can start with those files
combinedresults_pro0 <- read.csv("./data/public/combinedresults_pro0.csv")
combinedresults_pro1 <- read.csv("./data/public/combinedresults_pro1.csv")
combinedresults_pro2 <- read.csv("./data/public/combinedresults_pro2.csv")
combinedresults_pro3 <- read.csv("./data/public/combinedresults_pro3.csv")

# analyses repeated for each set of exclusion criteria
# three-level random-effects meta-analysis in MetaSEM
# TODO: refactor this code to parallel that of 002, 
#    saving objects as fixed1_ih_pro, mixed2_aa_pro, etc
random0_pro <- meta(y=yi, v=vi, data=combinedresults_pro0)
random1_pro <- meta(y=yi, v=vi, data=combinedresults_pro1)
random2_pro <- meta(y=yi, v=vi, data=combinedresults_pro2)
random3_pro <- meta(y=yi, v=vi, data=combinedresults_pro3)
#Notes: I^2 for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 

# a covariate of study version (in-house or expert-designed) is added to create a three-level mixed-effects meta-analysis
mixed0_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro0)
mixed1_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro1)
mixed2_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro2)
mixed3_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro3)

# constraining the variance to test if it significantly worsens the model
fixed0_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro0, RE.constraints=0)
fixed1_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro1, RE.constraints=0)
fixed2_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro2, RE.constraints=0)
fixed3_pro <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_pro3, RE.constraints=0)
# compare if there is a significant difference in model fit, chi square difference test
fit_comparison_0_pro <- anova(mixed0_pro, fixed0_pro)
fit_comparison_1_pro <- anova(mixed1_pro, fixed1_pro)
fit_comparison_2_pro <- anova(mixed2_pro, fixed2_pro)
fit_comparison_3_pro <- anova(mixed3_pro, fixed3_pro)

# TODO: Do I need to repeat this for in-house and author-advised, separately?









# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence ----
# This is a simple alternative and useful for most stringent exclusion criteria 
#    which drastically reduces overall N (exclusion set 3)
# read in .rds data
dat <- readRDS("./data/public/merged_deidentified_subset.rds")
# selecting only expert labs
dat <- subset(dat, expert==1)

###ANALYSIS 0: no exclusions###
# t.test and descriptive statistics per condition from psych package
t.test(dat$proauth_avg ~ dat$ms_condition)
describeBy(dat$proauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$proauth_avg ~ dat$ms_condition,
                 pooled=TRUE, paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
dat <- filter(dat, pass_ER1 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$proauth_avg~dat$ms_condition)
describeBy(dat$proauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$proauth_avg ~ dat$ms_condition,
                 pooled=TRUE, paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
dat <- filter(dat, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$proauth_avg~dat$ms_condition)
describeBy(dat$proauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$proauth_avg ~ dat$ms_condition,
                 pooled=TRUE, paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 3: Exclusion set 3###
dat <- filter(dat, pass_ER3 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$proauth_avg~dat$ms_condition)
describeBy(dat$proauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$proauth_avg ~ dat$ms_condition,
                 pooled=TRUE, paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

# Conducting a meta-analysis of only the in-house data to provide a summary of those results in basic form. ----
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
dat <- read.csv("./data/public/combinedresults_pro1.csv")
# conduct random effects meta-analysis of in-house only
filter(dat, expert == 0) %>% 
  meta(y = yi, v = vi, data = .) %>% 
  summary()

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Filtering for only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert) # Excluded for insufficient N; see bottom of 001_data_cleaning.R
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
dat <- filter(dat, source %in% c("uwmadison_inhouse", "cnj", "kansas_expert", "kansas_inhouse",
                                        "pace_expert", "vcu"))
# meta-analyze
rma(yi = yi, vi = vi, data = dat)

# Analyze anti-author ratings ----

#read in deidentified aggregate dataset
merged <- readRDS("./data/public/merged_deidentified_subset.rds")

# reads in csv files from above, just to confirm we can start with those files
combinedresults_anti0 <- read.csv("./data/public/combinedresults_anti0.csv")
combinedresults_anti1 <- read.csv("./data/public/combinedresults_anti1.csv")
combinedresults_anti2 <- read.csv("./data/public/combinedresults_anti2.csv")
combinedresults_anti3 <- read.csv("./data/public/combinedresults_anti3.csv")

# analyses repeated for each set of exclusion critera
# NB: three-level random-effects meta-analysis in MetaSEM had OpenMx status == 5
#    So I dropped arg "cluster = location" and went to meta() instead of meta3()
random0_anti <- meta(y=yi, v=vi, data=combinedresults_anti0)
random1_anti <- meta(y=yi, v=vi, data=combinedresults_anti1)
random2_anti <- meta(y=yi, v=vi, data=combinedresults_anti2)
random3_anti <- meta(y=yi, v=vi, data=combinedresults_anti3)
#Notes: I^2 for level 2 indicates the percent of total variance explained by effects within sites, 
#   and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 

# a covariate of study version (in-house or expert-designed) is added to create a mixed-effects meta-analysis
mixed0_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti0)
mixed1_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti1)
mixed2_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti2)
mixed3_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti3)
# Notes: The R? for the version predictor will be reported for both level 2 and level 3, 
#   although in this case version is a level 2 predictor so the level 3 R? will always be zero. 

# constraining the variance to test if it significantly worsens the model
summary( fixed0_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti0, RE.constraints=0))
summary( fixed1_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti1, RE.constraints=0))
summary( fixed2_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti2, RE.constraints=0))
summary( fixed3_anti <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults_anti3, RE.constraints=0))

# compare if there is a significant difference in model fit, chi square difference test
fit_comparison_0_anti <- anova(mixed0_anti, fixed0_anti)
fit_comparison_1_anti <- anova(mixed1_anti, fixed1_anti)
fit_comparison_2_anti <- anova(mixed2_anti, fixed2_anti)
fit_comparison_3_anti <- anova(mixed3_anti, fixed3_anti)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence. ----
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
dat <- readRDS("./data/public/merged_deidentified_subset.rds")
# selecting only expert labs
dat <- subset(dat, expert==1)

###ANALYSIS 0: no exclusions###
# t.test and descriptive statistics per condition from psych package
t.test(dat$antiauth_avg~dat$ms_condition)
describeBy(dat$antiauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$antiauth_avg~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 1: Exclusion set 1###
dat <- filter(dat, pass_ER1 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$antiauth_avg~dat$ms_condition)
describeBy(dat$antiauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$antiauth_avg~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
dat <- filter(dat, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$antiauth_avg~dat$ms_condition)
describeBy(dat$antiauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$antiauth_avg~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
dat <- filter(dat, pass_ER3 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$antiauth_avg~dat$ms_condition)
describeBy(dat$antiauth_avg, group = dat$ms_condition)
effsize::cohen.d(dat$antiauth_avg~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a positive value, reversing sign in the report

# Conducting a meta-analysis of only the in-house data to provide a summary of those results in basic form. ----
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
dat <- read.csv("./data/public/combinedresults_anti1.csv")
# conduct random effects meta-analysis of in-house only
filter(dat, expert==0) %>% 
  meta(y = yi, v = vi, data = .) %>% 
  summary()

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Filtering for only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert) # Again, was excluded for insufficient N given prereg
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
dat <- subset(dat, dat$source %in% c("uwmadison_inhouse", "cnj", "kansas_expert", "kansas_inhouse",
                                        "pace_expert", "vcu"))
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
rma(yi = yi, vi = vi, data = dat)


# Save all model objects to .RData for loading into 008 RMarkdown ----
save(random1_pro, random2_pro, random3_pro,
     mixed1_pro, mixed2_pro, mixed3_pro,
     fixed1_pro, fixed2_pro, fixed3_pro,
     fit_comparison_0_pro, fit_comparison_1_pro,
     fit_comparison_2_pro, fit_comparison_3_pro,
     # no in-house vs. author-advised stuff here
     random1_anti, random2_anti, random3_anti,
     mixed1_anti, mixed2_anti, mixed3_anti,
     fixed1_anti, fixed2_anti, fixed3_anti,
     fit_comparison_0_anti, fit_comparison_1_anti,
     fit_comparison_2_anti, fit_comparison_3_anti,
     # no in-house vs. author-advised stuff here
     file = "supplementary_results.RData"
)

## 2. Analyzing how much participants liked the pro and anti authors ----
# TODO: This is not referenced in the RMD -- can it be cut? Or can someone clarify the goal?
# Read in data to start from scratch
merged <- readRDS("./data/public/merged_deidentified_subset.rds")

# Uncomment either line if you want to subset to only in house or author advised sites

# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
merged <- filter(merged, !is.na(pro_minus_anti), 
                 pass_ER1 == T | expert == 0)
# 'merged_excl_2' further excludes participants as per exclusion set 2 (below)
merged_excl_2 <- filter(merged, !is.na(pro_minus_anti),
                        pass_ER2 == T | expert == 0)
# 'merged_excl_3' further excludes participants as per exclusion set 3 (below)
merged_excl_3 <- filter(merged_excl_2, !is.na(pro_minus_anti),
                        pass_ER3 == T | expert == 0)

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

# TODO: Figure out what the authors wanted to have run here.
#    It looks like they wanted to restrict analyses to ERs 1-3, expert == 1, pro_minus_anti > 0
# Subset to Author Advised participants who preferred the pro-US author 
# and examine if that finds the effect. Repeat for 3 exclusion sets.
# Datasets:
# merged_aa is basic exclusions
merged_aa <- filter(merged, expert == 1)
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
# TODO: This is not mentioned in the RMD. 
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
