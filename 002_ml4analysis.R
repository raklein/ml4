# Many Labs 4 Analysis Script
# Coder: Rick Klein raklein22@gmail.com
# OSF: https://osf.io/8ccnw/

# Written in RStudio Version 1.1.463, and R version 3.5.2

#ANALYSIS SCRIPT

# Open the .rproj file in R Studio to avoid setting the working directory.
# Otherwise, call setwd() with a path to the /ml4/ folder
# All file paths are relative from the working directory.

# uncomment below lines to install packages

# install.packages("metafor")
# install.packages("metaSEM")
# install.packages("haven")
# install.packages("psych")
# install.packages("dplyr")
# install.packages("effsize")
# install.packages("GPArotation")
# install.packages("tidyverse")

library(metafor)
library(metaSEM)
library(haven)
library(psych)
library(dplyr)
library(effsize)
library(GPArotation)
library(tidyverse)

## NOTE: some analyses below require the full "merged" dataset, 
#    not deidentified (mostly due to age and gender variables). 
#This is private due to participant confidentiality concerns, 
#    but inquire with Rick raklein22@gmail.com if you need it. 
#    (Typically requires IRB approval from your local institution 
#         indicating you'll keep the data properly protected)
merged <- readRDS("./data/processed_data/merged_subset.rds")

#alternatively, you can run it with the public data and get most results
#merged <- readRDS("./data/public/merged_deidentified_subset.rds")

# Also note you can load the merged_subset.rds or merged_full.rds data
# file. The former adheres strictly to the prereg, the latter
# contains all data (e.g., the analysis for the original preprint)

# Note that you will need to adjust this script where indicated if 
# you're using the full dataset.

# Analyse function:
#    Use dplyr::summarize to calculate summary stats per cell per site per exclusion rules
analyse <- function(data) {
  # Make means, sds, and ns
  sumstats <- group_by(data, location, source, ms_condition) %>% 
    summarize(n = n(),
              mean = mean(pro_minus_anti),
              sd = sd(pro_minus_anti),
              expert = first(expert)
    ) %>% 
    pivot_longer(cols = c(n, mean, sd)) %>% 
    unite(name, name, ms_condition) %>% 
    pivot_wider(names_from = name,
                values_from = value) %>% 
    # Calculate Cohen's d given unequal effect sizes (Borenstein et al. 2009, p 26, formulae 4.18 & 4.19)
    mutate(s_within = sqrt(((n_ms - 1)*sd_ms^2 + (n_tv-1)*sd_tv^2) / (n_ms + n_tv - 2)), #pooled SD
           d_diff = (mean_ms - mean_tv)/s_within # effect size of difference
           ) 
  
  # Make t, df, and pval
  # NOTE: p-value is two-tailed
  nhst <- group_by(data, source) %>% 
    summarize(t  = t.test(pro_minus_anti ~ ms_condition)$statistic,
              df = t.test(pro_minus_anti ~ ms_condition)$parameter,
              p.value = t.test(pro_minus_anti ~ ms_condition)$p.value)
  
  # combine stats
  dat <- left_join(sumstats, nhst, by = "source")
  
  # use metafor::escalc() to add effect size and precision
  # Appends yi and vi to the data object.
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

# Run analyse() on each exclusion set to generate summary stats per cell per site
combinedresults0 <- filter(merged, !is.na(pro_minus_anti)) %>% 
  analyse()

combinedresults1 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER1 == T | expert == 0) %>% 
  analyse()

combinedresults2 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER2 == T | expert == 0) %>% 
  analyse()

combinedresults3 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER3 == T | expert == 0) %>% 
  analyse()

#save all to .csv file
write.csv(combinedresults0, "./data/public/combinedresults0.csv", row.names = FALSE)
write.csv(combinedresults1, "./data/public/combinedresults1.csv", row.names = FALSE)
write.csv(combinedresults2, "./data/public/combinedresults2.csv", row.names = FALSE)
write.csv(combinedresults3, "./data/public/combinedresults3.csv", row.names = FALSE)

# metaSEM analyses ----
# reads in csv files from above, just to confirm we can start with those files
# Additionally centers expert for contrast coding
combinedresults0 <- read.csv("./data/public/combinedresults0.csv") %>% 
  mutate(expert.ctr = ifelse(expert, 1, -1))
combinedresults1 <- read.csv("./data/public/combinedresults1.csv") %>% 
  mutate(expert.ctr = ifelse(expert, 1, -1))
combinedresults2 <- read.csv("./data/public/combinedresults2.csv") %>% 
  mutate(expert.ctr = ifelse(expert, 1, -1))
combinedresults3 <- read.csv("./data/public/combinedresults3.csv") %>% 
  mutate(expert.ctr = ifelse(expert, 1, -1))

# analyses repeated for each set of exclusion criteria
# This was originally a three-level random-effects meta-analysis in MetaSEM
# had OpenMX status1: 5 so we had to drop the 'cluster = location' argument (not enough datapoints per location -- max = 2, most = 1)
# So, now it's a univariate random-effects metaanalysis
random0 <- meta(y=yi, v=vi, data=combinedresults0)
random1 <- meta(y=yi, v=vi, data=combinedresults1)
random2 <- meta(y=yi, v=vi, data=combinedresults2)
random3 <- meta(y=yi, v=vi, data=combinedresults3)

#Notes: Intercept1 is the grand mean effect size. 
# for the 3 level meta, I? for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 
# Now that it's a simple meta, all of these meta-analytic stats (tau, q, I2) refer to variability among all effect sizes (e.g., ignores that in 3 cases these are two nested within a particular university).

# a covariate of study version (in-house or expert-designed) is added to create a mixed effects model.
mixed0 <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults0)
mixed1 <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults1)
mixed2 <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults2)
mixed3 <- meta(y=yi, v=vi, x=expert.ctr, data=combinedresults3)

# Notes: Intercept1 is still the grand mean estimate, 
#    Slope1_1 represents the difference between versions

# Notes: In the old 3-level metasem, 
#    The R^2 for the version predictor will be reported for both level 2 and level 3, 
#    although in this case version is a level 2 predictor so the level 3 R^2 will always be zero. 

# Now, we're going to compare the random effects model to a fixed effects model separately for 
# Author Advised vs In House sites. If this improves model fit for one but not the other, 
#    that suggests that model shows greater variability in effect sizes. 
#    There are likely better ways to do this.

# split Author Advised from In House results
combinedresults1_ih <- filter(combinedresults1, expert == 0)

combinedresults1_aa <- filter(combinedresults1, expert == 1)
combinedresults2_aa <- filter(combinedresults2, expert == 1)
combinedresults3_aa <- filter(combinedresults3, expert == 1)

# constrain the variance across sites to zero (perform fixed effects model)
fixed1_ih <- meta(y=yi, v=vi, data=combinedresults1_ih, RE.constraints=0)

fixed1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa, RE.constraints=0)
fixed2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa, RE.constraints=0)
fixed3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa, RE.constraints=0)

# repeat random effects model for just this subset
random1_ih <- meta(y=yi, v=vi, data=combinedresults1_ih)

random1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa)
random2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa) 
# RESOLVED, NOT A BUG: OpenMx status1 == 5
# "5 means that the Hessian at the solution is not convex. 
#    There is likely a better solution, but the optimizer is stuck
#    in a region of confusing geometry (like a saddle point)."
# I think the issue is that Tau2 is estimated as very small.
#    It doesn't seem to affect the intercept when I play w/ RE.start and RE.lbound,
#    and Tau2 will go as small as RE.lbound will allow it
# summary(random2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa,
#                            RE.lbound = 1e-50))
# summary(random2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa,
#                            RE.start = 1, RE.lbound = 1e-100))
# I have decided to report the results as they are, ignoring the error code,
#  because the results are that Tau2_1_1 is basically zero,
#  and the error code does not influence the estimate of the intercept
random3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa)

# compare if there is a significant difference in model fit, chi square difference test
fit_comparison_1_ih <- anova(random1_ih, fixed1_ih)

fit_comparison_1_aa <- anova(random1_aa, fixed1_aa)
fit_comparison_2_aa <- anova(random2_aa, fixed2_aa)
fit_comparison_3_aa <- anova(random3_aa, fixed3_aa)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence ----
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
dat <- readRDS("./data/public/merged_deidentified_subset.rds") # can also choose to use merged_deidentified_full
# selecting only expert labs
dat <- subset(dat, expert==1)

###ANALYSIS 0: no special exclusions###
dat <- subset(dat, !is.na(dat$pro_minus_anti))
# t.test and descriptive statistics per condition from psych package
t.test(dat$pro_minus_anti~dat$ms_condition)
describeBy(dat$pro_minus_anti, group = dat$ms_condition)
effsize::cohen.d(dat$pro_minus_anti~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)



###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors)
dat <- subset(dat, pass_ER1 == T) 
# t.test and descriptive statistics per condition from psych package
t.test(dat$pro_minus_anti~dat$ms_condition)
describeBy(dat$pro_minus_anti, group = dat$ms_condition)
effsize::cohen.d(dat$pro_minus_anti~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White (race == 1)
# 4. Born in USA (countryofbirth == 1)
dat <- subset(dat, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$pro_minus_anti~dat$ms_condition)
describeBy(dat$pro_minus_anti, group = dat$ms_condition)
effsize::cohen.d(dat$pro_minus_anti~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item
dat <- subset(dat, pass_ER3 == T)
# t.test and descriptive statistics per condition from psych package
t.test(dat$pro_minus_anti~dat$ms_condition)
describeBy(dat$pro_minus_anti, group = dat$ms_condition)
effsize::cohen.d(dat$pro_minus_anti~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a positive value, reversing sign in the report

###Conducting a small meta-analysis of only the in-house data to provide a summary of those results in basic form.####
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
dat <- read.csv("./data/public/combinedresults1.csv")
# subset to in-house rows only
dat <- subset(dat, expert==0)
# conduct random effects meta-analyis
summary( meta(y = yi, v = vi, data = dat))

# # forest plot
# dev.off()
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(x= dat$yi, vi=dat$vi, slab=dat$location)
# par(cex=1, font=2)#bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# 
# # same forst plot, but using rma so it plots the aggregate
# dev.off()
# png("./output/inhousemeta.png", type='cairo')
# par(mar=c(4,4,1,4)) #decreasing margins
# forest(rma(yi= dat$yi, vi=dat$vi, slab=dat$location))
# par(cex=1, font=2) #bold font
# text(-3.3, 13, "Location",  pos=4) #adds location label using x, y coord
# text(3.8, 13, "SMD [95% CI]", pos=2) #adds standardized mean diff label using x y coord
# dev.off()

# sample funnel plot 
# funnel(rma(yi= dat$yi, vi=dat$vi, slab=dat$location))

# Aggregate participants characteristics
# Converting to numeric, will lose uninterpretable codes
merged$age <- as.numeric(as.character(merged$age)) # 60 have age as a range e.g. "18-24"
merged$gender <- as.numeric(as.character(merged$gender)) # 10 have gender 3 or N or "non-binary"
merged$race <- as.numeric(as.character(merged$race))

# Read data
dat <- merged
# Applying exclusion criteria 0 and 1
dat <- subset(dat, !is.na(pro_minus_anti) & (pass_ER1 == T | expert == 0))

# get counts per gender (1 = woman, 2 = men, 3 = something else)
with(dat, table(gender), useNA = 'always')
# get counts per race (1 = white, 2 = black / AfrAm, 3 = Native American, 4 = Asian, 
#                      5 = Native Hawaiian or Pacific Islander, 6 = something else)
with(dat, table(race), useNA = 'always')
# create percentages
with(dat, table(race), useNA = 'always') %>% 
  prop.table() * 100

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
# Selecting only the below sites:
#University of Wisconsin, Madison, WI (in-house)
#The College of New Jersey
#University of Kansas (Expert)
#University of Kansas (in-house)
#Pace University (expert)
#Virginia Commonwealth University, Richmond, VA
# Note: not all of these exist in the subsetted dataset
dat <- subset(dat, source %in% c("uwmadison_inhouse", "cnj", "kansas_expert",
                                   "kansas_inhouse", "pace_expert", "vcu"))
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
dat$ms_condition <- factor(dat$ms_condition, levels = c("ms", "tv"))
# Analyses using that subset
t.test(dat$pro_minus_anti~dat$ms_condition)
describeBy(dat$pro_minus_anti, group = dat$ms_condition)
effsize::cohen.d(dat$pro_minus_anti~dat$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) 
#this was previously incorrectly indicating a positive value? 
#   Had to manually reverse for dissertation but seems fine now

# Computing alpha for the essay author ratings, basic exclusions ----
# Read data
dat <- merged
# Applying exclusion criteria 0 and 1
dat <- subset(dat, !is.na(dat$pro_minus_anti) & (pass_ER1 == T | expert == 0))

# create a data frame of only pro-us ratings for the alpha() function
pro_df <- data.frame(dat$prous3, dat$prous4, dat$prous5)
psych::alpha(pro_df)
omega(pro_df) # Omega may be more appropriate

# create a data frame of only anti-us ratings for the alpha() function
anti_df <- data.frame(dat$antius3, dat$antius4, dat$antius5)
psych::alpha(anti_df)
omega(anti_df)

# generating demographics cross-tabs ----
# TODO: This section is a WIP
# FIXME: Implement demographic counting in the appropriate fashion 
#    (pre-exclusion, post-exclusion, whatever)

# Currently, demographics seem to be calculated for group after application of ER1 to expert sites
#    (non-expert sites just get a pass on this, I guess)
merged <- readRDS("./data/processed_data/merged_subset.rds")
# demographics are to be calculated after exclusion rule 1
demos <- filter(merged, !is.na(pro_minus_anti),
                pass_ER1 == T | expert == 0)

# FIXME: is there a category for multiracial? 6? something else?

demos_race <- demos %>% 
  select(race) %>% 
  mutate(race = as.numeric(as.character(race))) %>% # coerce to numeric categories
  group_by(race) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(n / sum(n)* 100, 2))

demos_gender <- demos %>% 
  select(gender) %>% 
  mutate(gender = as.numeric(as.character(gender))) %>% # coerce to numeric categories
  group_by(gender) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(n / sum(n)* 100, 2))

# save to file
write_csv(demos_race, "./data/processed_data/demos_race.csv")
write_csv(demos_gender, "./data/processed_data/demos_gender.csv")

# Save all model objects to .RData for loading into 006 RMarkdown
save(random1, random2, random3,
     mixed1, mixed2, mixed3,
     fixed1_ih, fixed1_aa, fixed2_aa, fixed3_aa,
     random1_ih, random1_aa, random2_aa, random3_aa,
     fit_comparison_1_ih, fit_comparison_1_aa, fit_comparison_2_aa, fit_comparison_3_aa,
     file = "results.RData")
