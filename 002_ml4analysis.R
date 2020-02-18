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

# Use dplyr::summarize to calculate summary stats per cell per site per exclusion rules
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
    # TODO: check about denominator for d given unequal cell sizes
    mutate(d_diff = (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2)) #computes Cohen's D effect size
  
  # Make t, df, and pval
  # NOTE: p-value is two-tailed
  nhst <- group_by(data, source) %>% 
    summarize(t  = t.test(pro_minus_anti ~ ms_condition)$statistic,
              df = t.test(pro_minus_anti ~ ms_condition)$parameter,
              p.value = t.test(pro_minus_anti ~ ms_condition)$p.value)
  
  # combine stats
  dat <- left_join(sumstats, nhst, by = "source")
  
  # make pretty names per site
  dat <- mutate(dat, 
                case_when(source == "ufl" ~ "University of Florida",
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
                          source == "byui" ~ "Brigham Young University – Idaho",
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

combinedresults0 <- filter(merged, !is.na(pro_minus_anti)) %>% 
  analyse()

combinedresults1 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER1 == T) %>% 
  analyse()

combinedresults2 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER2 == T) %>% 
  analyse()

combinedresults3 <- filter(merged, !is.na(pro_minus_anti),
                           pass_ER3 == T) %>% 
  analyse()


### Note: If you're using the subsetted dataset, the below section
# will give errors due to missing sources. You can safely ignore them,
# it's simply that some lines are not running.

###ANALYSIS 0: no exclusions ----
#Computing SE and sampling variance with metafor package.
# yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# n1i numeric number of participants in the intervention group
# m1i numeric mean number of days off work/school in the intervention group
# sd1i numeric standard deviation of the number of days off work/school in the intervention group
# n2i numeric number of participants in the control/comparison group
# m2i numeric mean number of days off work/school in the control/comparison group
# sd2i numeric standard deviation of the number of days off work/school in the control/comparison group
#Appends yi and vi to the data object.
combinedresults0 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults0, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults0, "./data/public/combinedresults0.csv", row.names = FALSE)

###ANALYSIS 1: Exclusion set 1 ----
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults1 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults1, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults1, "./data/public/combinedresults1.csv", row.names = FALSE)

###ANALYSIS 2: Exclusion set 2 ----
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors
#3. Identify as White (race == 1)
#4. Born in USA (countryofbirth == 1)

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults2 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults2, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults2, "./data/public/combinedresults2.csv", row.names = FALSE)

###ANALYSIS 3: Exclusion set 3----
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item

# This uses the metafor package to compute yi (the standardized mean difference effect size) and vi (the sampling variance) to be used in meta-analysis.
# Appends this to the data object.
combinedresults3 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults3, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults3, "./data/public/combinedresults3.csv", row.names = FALSE)

# metaSEM analyses ----
# reads in csv files from above, just to confirm we can start with those files
combinedresults0 <- read.csv("./data/public/combinedresults0.csv")
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# analyses repeated for each set of exclusion critera
# This was originally a three-level random-effects meta-analysis in MetaSEM
# had OpenMX status1: 5 so we had to drop the 'cluster = location' argument (not enough datapoints per location -- max = 2, most = 1)
# So, now it's a univariate random-effects metaanalysis
summary( meta(y=yi, v=vi, data=combinedresults0))
summary( meta(y=yi, v=vi, data=combinedresults1))
summary( meta(y=yi, v=vi, data=combinedresults2))
summary( meta(y=yi, v=vi, data=combinedresults3))

#Notes: Intercept1 is the grand mean effect size. 
# for the 3 level meta, I? for level 2 indicates the percent of total variance explained by effects within sites, and I? for level 3 indicates the percent of total variance accounted for by differences between sites. 
# Now that it's a simple meta, all of these meta-analytic stats (tau, q, I2) refer to variablity among all effect sizes (e.g., ignores that in 3 cases these are two nested within a particular university).

# a covariate of study version (in-house or expert-designed) is added to create a mixed effects model.
summary(mixed0 <- meta(y=yi, v=vi, x=expert, data=combinedresults0))
summary(mixed1 <- meta(y=yi, v=vi, x=expert, data=combinedresults1))
summary(mixed2 <- meta(y=yi, v=vi, x=expert, data=combinedresults2))
summary(mixed3 <- meta(y=yi, v=vi, x=expert, data=combinedresults3))

# Notes: Intercept1 is still the grand mean estimate, Slope1_1 represents the difference between versions

# Notes: In the old 3-level metasem, The R? for the version predictor will be reported for both level 2 and level 3, although in this case version is a level 2 predictor so the level 3 R? will always be zero. 

# Now, we're going to compare the random effects model to a fixed effects model separately for 
# Author Advised vs In House sites. If this improves model fit for one but not the other, that suggests that model shows greater variability in effect sizes. There are likely better ways to do this.

# split Author Advised from In House results
combinedresults0_ih <- filter(combinedresults0, expert == 0)
combinedresults1_ih <- filter(combinedresults1, expert == 0)
combinedresults2_ih <- filter(combinedresults2, expert == 0)
combinedresults3_ih <- filter(combinedresults3, expert == 0)

combinedresults0_aa <- filter(combinedresults0, expert == 1)
combinedresults1_aa <- filter(combinedresults1, expert == 1)
combinedresults2_aa <- filter(combinedresults2, expert == 1)
combinedresults3_aa <- filter(combinedresults3, expert == 1)

# constrain the variance across sites to zero (perform fixed effects model)
summary(fixed0_ih <- meta(y=yi, v=vi, data=combinedresults0_ih, RE.constraints=0))
summary(fixed1_ih <- meta(y=yi, v=vi, data=combinedresults1_ih, RE.constraints=0))
summary(fixed2_ih <- meta(y=yi, v=vi, data=combinedresults2_ih, RE.constraints=0))
#summary(fixed3_ih <- meta(y=yi, v=vi, data=combinedresults3_ih, RE.constraints=0)) # error, no data in cr3_ih

summary(fixed0_aa <- meta(y=yi, v=vi, data=combinedresults0_aa, RE.constraints=0))
summary(fixed1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa, RE.constraints=0))
summary(fixed2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa, RE.constraints=0))
summary(fixed3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa, RE.constraints=0))

# repeat random effects model for just this subset
summary(random0_ih <- meta(y=yi, v=vi, data=combinedresults0_ih))
summary(random1_ih <- meta(y=yi, v=vi, data=combinedresults1_ih))
summary(random2_ih <- meta(y=yi, v=vi, data=combinedresults2_ih))
#summary(random3_ih <- meta(y=yi, v=vi, data=combinedresults3_ih)) # error, no data

summary(random0_aa <- meta(y=yi, v=vi, data=combinedresults0_aa))
summary(random1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa))
summary(random2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa))
summary(random3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa))

# compare if there is a significant difference in model fit, chi square difference test
anova(random0_ih, fixed0_ih)
anova(random1_ih, fixed1_ih)
anova(random2_ih, fixed2_ih)
#anova(random3_ih, fixed3_ih)

anova(random0_aa, fixed0_aa)
anova(random1_aa, fixed1_aa)
anova(random2_aa, fixed2_aa)
anova(random3_aa, fixed3_aa)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence ----
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
data <- readRDS("./data/public/merged_deidentified_subset.rds") # can also choose to use merged_deidentified_full
# selecting only expert labs
data <- subset(data, expert==1)

###ANALYSIS 0: no special exclusions###
data <- subset(data, !is.na(data$pro_minus_anti))
# t.test and descriptive statistics per condition from psych package
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)



###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, pass_ER1 == T) 
# t.test and descriptive statistics per condition from psych package
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 2: Exclusion set 2###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White (race == 1)
# 4. Born in USA (countryofbirth == 1)
data <- subset(data, pass_ER2 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a negative value, I'm not sure why but it should be positive from the group means

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item
data <- subset(data, pass_ER3 == T)
# t.test and descriptive statistics per condition from psych package
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this is incorrectly indicating a positive value, reversing sign in the report

###Conducting a small meta-analysis of only the in-house data to provide a summary of those results in basic form.####
# Read in summary .csv which used basic exclusion rules, Exclusion Set 1
data <- read.csv("./data/public/combinedresults1.csv")
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

# Aggregate participants characteristics
# Converting to numeric
merged$age <- as.numeric(as.character(merged$age))
merged$gender <- as.numeric(as.character(merged$gender))
merged$race <- as.numeric(as.character(merged$race))

# Read data
data <- merged
# Applying exclusion criteria 0 and 1
data <- subset(data, !is.na(pro_minus_anti) & pass_ER1 == T)

# get counts per gender (1 = woman, 2 = men, 3 = something else)
with(data, table(gender), useNA = 'always')
# get counts per race (1 = white, 2 = black / AfrAm, 3 = Native American, 4 = Asian, 
#                      5 = Native Hawaiian or Pacific Islander, 6 = something else)
with(data, table(race), useNA = 'always')
# create percentaes
with(data, table(race), useNA = 'always') %>% 
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
data <- subset(data, source %in% c("uwmadison_inhouse", "cnj", "kansas_expert",
                                   "kansas_inhouse", "pace_expert", "vcu"))
# Applying the same levels fix as earlier, only because it caused problems in 
# cohen.d() below. May not be necessary anymore.
data$ms_condition <- factor(data$ms_condition, levels = c("ms", "tv"))
# Analyses using that subset
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this was previously incorrectly indicating a positive value? Had to manually reverse for dissertation but seems fine now

# Computing alpha for the essay author ratings, basic exclusions
# Read data
data <- merged
# Applying exclusion criteria 0 and 1
data <- subset(data, !is.na(data$pro_minus_anti) & pass_ER1 == T)

# TODO: Pace inhouse used a different (not 1-7 or 1-9) scale.
#     Need to restrict these analyses to same scales.

# create a data frame of only pro-us ratings for the alpha() function
pro_df <- data.frame(data$prous3,data$prous4,data$prous5)
psych::alpha(pro_df)
omega(pro_df) # Omega may be more appropriate

# create a data frame of only anti-us ratings for the alpha() function
anti_df <- data.frame(data$antius3,data$antius4,data$antius5)
psych::alpha(anti_df)
omega(anti_df)

###### Adding a clearer site label for tables ----

# Read per-site results according to the three exclusions criteria levels
combinedresults0 <- read.csv("./data/public/combinedresults0.csv")
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# overwrite .csv files
write.csv(combinedresults0, "./data/public/combinedresults0.csv", row.names = FALSE)
write.csv(combinedresults1, "./data/public/combinedresults1.csv", row.names = FALSE)
write.csv(combinedresults2, "./data/public/combinedresults2.csv", row.names = FALSE)
write.csv(combinedresults3, "./data/public/combinedresults3.csv", row.names = FALSE)

