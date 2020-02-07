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

## NOTE: some analyses below require the full "merged" dataset, not deidentified (mostly due to age and gender variables). This is private due to participant confidentiality concerns, but inquire with Rick raklein22@gmail.com if you need it. (Typically requires IRB approval from your local institution indicating you'll keep the data properly protected)
merged <- readRDS("./data/processed_data/merged.rds")
#alternatively, you can run it with the public data and get most results
#merged <- readRDS("./data/public/merged_deidentified.rds")

#Function to generate required stats for meta-analysis.
analysis <- function(data, exclusionrule, sitesource)
{
  
  # Create exclusion rules
  # Exclusion rule 0: no special exclusions
  index0 <- !is.na(data$pro_minus_anti) & data$source==sitesource # filter for data from site & has primary DV
  # Exclusion rule 1:
  #1. Wrote something for both writing prompts
  #2. Completed all six items evaluating the essay authors)
  index1 <- index0 & # filter for previous conditions
    (data$msincomplete == 0 | is.na(data$msincomplete)) & # P completed both prompts, or prompts marked as NA
    !is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & # P provided all 3 ratings of pro-us essay
    !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5) # P provided all 3 ratings of anti-us essay
  # Exclusion rule 2:
  #1. Wrote something for both writing prompts
  #2. Completed all six items evaluating the essay authors
  #3. Identify as White (race == 1)
  #4. Born in USA (countryofbirth == 1)
  index2 <- index1 & # filter for previous Exclusion1 conditions
    (data$race == 1 & !is.na(data$race)) & # white ps, NA race discarded
    (data$countryofbirth == 1 & !is.na(data$countryofbirth)) # US-born Ps, NA race discarded
  # Exclusion rule 3
  # 1. Wrote something for both writing prompts
  # 2. Completed all six items evaluating the essay authors
  # 3. Identify as White
  # 4. Born in USA
  # 5. Score a 7 or higher on the American Identity item
  index3 <- index2 & # filter for previous Exclusion conditions
    (data$americanid >= 7 & !is.na(data$americanid)) # strongly ID as american, NAs discarded
  
  # choose index based on user-specified exclusion rule
  if(exclusionrule == "e0") {index <- index0
  } else if(exclusionrule == "e1") {index <- index1
  } else if (exclusionrule == "e2") {index <- index2
  } else if (exclusionrule == "e3") {index <- index3
  } else stop("Must specify exclusion rule: e0, e1, e2, or e3")
  
  # create statistics after filtering for cases that match index
  location <- merged$location[data$source==sitesource][1] #saves first row from location variable
  n_tv    <- length(data$pro_minus_anti[index & data$ms_condition == 'tv']) #n for tv condition
  n_ms    <- length(data$pro_minus_anti[index & data$ms_condition == 'ms']) #n for ms condition
  sd_tv   <-     sd(data$pro_minus_anti[index & data$ms_condition == 'tv']) #sd for tv participants at that site
  sd_ms   <-     sd(data$pro_minus_anti[index & data$ms_condition == 'ms']) #sd for ms participants at that site
  mean_tv <-   mean(data$pro_minus_anti[index & data$ms_condition == 'tv']) #mean for tv participants at that site
  mean_ms <-   mean(data$pro_minus_anti[index & data$ms_condition == 'ms']) #mean for ms participants at that site
  expert <- mean(merged$expert[data$source==sitesource]) #shortcut to indicate whether site is expert or not (0 = inhouse 1 = expert)
  d_diff <- (mean_ms - mean_tv)/ sqrt((sd_ms^2+sd_tv^2)/2) #computes Cohen's D effect size
  nhst <- t.test(data$pro_minus_anti~data$ms_condition, subset = index)
  t <- nhst$statistic
  df <- nhst$parameter
  p.value <- nhst$p.value
  result <- data.frame(location, sitesource, expert, n_tv, mean_tv, sd_tv, n_ms, mean_ms, sd_ms, d_diff, t, df, p.value) #results to be reported
  return(result)
}

###ANALYSIS 0: no exclusions###
#above function is run for each site identifier
riverside_results         <- analysis(merged, "e0", "riverside")
azusa_results             <- analysis(merged, "e0", "azusa")
cnj_results               <- analysis(merged, "e0", "cnj")
illinois_results          <- analysis(merged, "e0", "illinois")
ithaca_results            <- analysis(merged, "e0", "ithaca")
kansas_inhouse_results    <- analysis(merged, "e0", "kansas_inhouse")
occid_results             <- analysis(merged, "e0", "occid")
pace_expert_results       <- analysis(merged, "e0", "pace_expert")
sou_inhouse_results       <- analysis(merged, "e0", "sou_inhouse")
ufl_results               <- analysis(merged, "e0", "ufl")
upenn_results             <- analysis(merged, "e0", "upenn")
uwmadison_expert_results  <- analysis(merged, "e0", "uwmadison_expert")
uwmadison_inhouse_results <- analysis(merged, "e0", "uwmadison_inhouse")
wesleyan_inhouse_results  <- analysis(merged, "e0", "wesleyan_inhouse")
wpi_results               <- analysis(merged, "e0", "wpi")
kansas_expert_results     <- analysis(merged, "e0", "kansas_expert")
plu_results               <- analysis(merged, "e0", "plu")
ashland_results           <- analysis(merged, "e0", "ashland")
vcu_results               <- analysis(merged, "e0", "vcu")
byui_results              <- analysis(merged, "e0", "byui")
pace_inhouse_results      <- analysis(merged, "e0", "pace_inhouse")

#merges results from above into a single data frame
combinedresults0 <- rbind(
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
combinedresults0 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults0, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults0, "./data/public/combinedresults0.csv", row.names = FALSE)

###ANALYSIS 1: Exclusion set 1###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors)


#above function is run for each site identifier
riverside_results         <- analysis(merged, "e1", "riverside")
azusa_results             <- analysis(merged, "e1", "azusa")
cnj_results               <- analysis(merged, "e1", "cnj")
illinois_results          <- analysis(merged, "e1", "illinois")
ithaca_results            <- analysis(merged, "e1", "ithaca")
kansas_inhouse_results    <- analysis(merged, "e1", "kansas_inhouse")
occid_results             <- analysis(merged, "e1", "occid")
pace_expert_results       <- analysis(merged, "e1", "pace_expert")
sou_inhouse_results       <- analysis(merged, "e1", "sou_inhouse")
ufl_results               <- analysis(merged, "e1", "ufl")
upenn_results             <- analysis(merged, "e1", "upenn")
uwmadison_expert_results  <- analysis(merged, "e1", "uwmadison_expert")
uwmadison_inhouse_results <- analysis(merged, "e1", "uwmadison_inhouse")
wesleyan_inhouse_results  <- analysis(merged, "e1", "wesleyan_inhouse")
wpi_results               <- analysis(merged, "e1", "wpi")
kansas_expert_results     <- analysis(merged, "e1", "kansas_expert")
plu_results               <- analysis(merged, "e1", "plu")
ashland_results           <- analysis(merged, "e1", "ashland")
vcu_results               <- analysis(merged, "e1", "vcu")
byui_results              <- analysis(merged, "e1", "byui")
pace_inhouse_results      <- analysis(merged, "e1", "pace_inhouse")

#merges results from above into a single data frame
combinedresults1 <- rbind(
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
combinedresults1 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults1, measure = "SMD", 
                           append = TRUE)

#saves .csv file
write.csv(combinedresults1, "./data/public/combinedresults1.csv", row.names = FALSE)

###ANALYSIS 2: Exclusion set 2###
#1. Wrote something for both writing prompts
#2. Completed all six items evaluating the essay authors
#3. Identify as White (race == 1)
#4. Born in USA (countryofbirth == 1)

#in-house sites don't necessarily have the data necessary to implement these exclusions
#Below, analysis1 (basic exclusions) is run for in-house, while analysis 2 is run for expert versions

#expert sites
riverside_results         <- analysis(merged, "e2", "riverside")
cnj_results               <- analysis(merged, "e2", "cnj")
occid_results             <- analysis(merged, "e2", "occid")
pace_expert_results       <- analysis(merged, "e2", "pace_expert")
uwmadison_expert_results  <- analysis(merged, "e2", "uwmadison_expert")
kansas_expert_results     <- analysis(merged, "e2", "kansas_expert")
ashland_results           <- analysis(merged, "e2", "ashland")
vcu_results               <- analysis(merged, "e2", "vcu")
byui_results              <- analysis(merged, "e2", "byui")

#inhouse sites
azusa_results             <- analysis(merged, "e1", "azusa")
illinois_results          <- analysis(merged, "e1", "illinois")
ithaca_results            <- analysis(merged, "e1", "ithaca")
kansas_inhouse_results    <- analysis(merged, "e1", "kansas_inhouse")
sou_inhouse_results       <- analysis(merged, "e1", "sou_inhouse")
ufl_results               <- analysis(merged, "e1", "ufl")
upenn_results             <- analysis(merged, "e1", "upenn")
uwmadison_inhouse_results <- analysis(merged, "e1", "uwmadison_inhouse")
wesleyan_inhouse_results  <- analysis(merged, "e1", "wesleyan_inhouse")
wpi_results               <- analysis(merged, "e1", "wpi")
plu_results               <- analysis(merged, "e1", "plu")
pace_inhouse_results      <- analysis(merged, "e1", "pace_inhouse")

#merges results from above into a single data frame
combinedresults2 <- rbind(
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
combinedresults2 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults2, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults2, "./data/public/combinedresults2.csv", row.names = FALSE)

###ANALYSIS 3: Exclusion set 3###
# 1. Wrote something for both writing prompts
# 2. Completed all six items evaluating the essay authors
# 3. Identify as White
# 4. Born in USA
# 5. Score a 7 or higher on the American Identity item

# in-house sites don't necessarily have the data necessary to implement these exclusions
# Below, analysis1 (basic exclusions) is run for in-house, while analysis 3 is run for expert versions

# expert sites
riverside_results         <- analysis(merged, "e3", "riverside")
cnj_results               <- analysis(merged, "e3", "cnj")
occid_results             <- analysis(merged, "e3", "occid")
pace_expert_results       <- analysis(merged, "e3", "pace_expert")
uwmadison_expert_results  <- analysis(merged, "e3", "uwmadison_expert")
kansas_expert_results     <- analysis(merged, "e3", "kansas_expert")
ashland_results           <- analysis(merged, "e3", "ashland")
vcu_results               <- analysis(merged, "e3", "vcu")
byui_results              <- analysis(merged, "e3", "byui")

#inhouse sites
azusa_results             <- analysis(merged, "e1", "azusa")
illinois_results          <- analysis(merged, "e1", "illinois")
ithaca_results            <- analysis(merged, "e1", "ithaca")
kansas_inhouse_results    <- analysis(merged, "e1", "kansas_inhouse")
sou_inhouse_results       <- analysis(merged, "e1", "sou_inhouse")
ufl_results               <- analysis(merged, "e1", "ufl")
upenn_results             <- analysis(merged, "e1", "upenn")
uwmadison_inhouse_results <- analysis(merged, "e1", "uwmadison_inhouse")
wesleyan_inhouse_results  <- analysis(merged, "e1", "wesleyan_inhouse")
wpi_results               <- analysis(merged, "e1", "wpi")
plu_results               <- analysis(merged, "e1", "plu")
pace_inhouse_results      <- analysis(merged, "e1", "pace_inhouse")

# merges results from above into a single data frame
combinedresults3 <- rbind(
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
combinedresults3 <- escalc(n1i = n_ms, n2i = n_tv, m1i = mean_ms, m2i = mean_tv, 
                           sd1i = sd_ms, sd2i = sd_tv, data = combinedresults3, measure = "SMD", 
                           append = TRUE)

# saves .csv file
write.csv(combinedresults3, "./data/public/combinedresults3.csv", row.names = FALSE)

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
summary(fixed3_ih <- meta(y=yi, v=vi, data=combinedresults3_ih, RE.constraints=0))

summary(fixed0_aa <- meta(y=yi, v=vi, data=combinedresults0_aa, RE.constraints=0))
summary(fixed1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa, RE.constraints=0))
summary(fixed2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa, RE.constraints=0))
summary(fixed3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa, RE.constraints=0))

# repeat random effects model for just this subset
summary(random0_ih <- meta(y=yi, v=vi, data=combinedresults0_ih))
summary(random1_ih <- meta(y=yi, v=vi, data=combinedresults1_ih))
summary(random2_ih <- meta(y=yi, v=vi, data=combinedresults2_ih))
summary(random3_ih <- meta(y=yi, v=vi, data=combinedresults3_ih))

summary(random0_aa <- meta(y=yi, v=vi, data=combinedresults0_aa))
summary(random1_aa <- meta(y=yi, v=vi, data=combinedresults1_aa))
summary(random2_aa <- meta(y=yi, v=vi, data=combinedresults2_aa))
summary(random3_aa <- meta(y=yi, v=vi, data=combinedresults3_aa))

# compare if there is a significant difference in model fit, chi square difference test
anova(random0_ih, fixed0_ih)
anova(random1_ih, fixed1_ih)
anova(random2_ih, fixed2_ih)
anova(random3_ih, fixed3_ih)

anova(random0_aa, fixed0_aa)
anova(random1_aa, fixed1_aa)
anova(random2_aa, fixed2_aa)
anova(random3_aa, fixed3_aa)

# Repeating analyses of "expert" sites in the aggregate, ignoring site dependence.
# This is a simple alternative and useful for most stringent exclusion criteria which drastically reduces overall N (exclusion set 3)
# read in .rds data
data <- readRDS("./data/public/merged_deidentified.rds")
# selecting only expert labs
data <- subset(data, expert==1)

###ANALYSIS 0: no exclusions###
# t.test and descriptive statistics per condition from psych package
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95)

###ANALYSIS 1: Exclusion set 1###
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & 
                      !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
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
data <- subset(data, data$race == 1 & !is.na(data$race))
# 4. Born in USA (countryofbirth == 1)
data <- subset(data, data$countryofbirth == 1 & !is.na(data$race))
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
data <- subset(data, data$americanid >= 7 & !is.na(data$americanid))
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
# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) 
                      & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))
length(which(data$gender== 1)) #number of women
length(which(data$gender== 2)) #number of men
length(which(data$gender== 3)) #other responses
sd(data$age, na.rm=TRUE)
mean(data$age, na.rm=TRUE)
length(which(data$race == 1)) #num White
length(which(data$race == 1))/nrow(data)*100 #percent White, using the length of the source variable (assigned to all sessions) for total N
length(which(data$race == 2)) #num Black or African American 
length(which(data$race == 2))/nrow(data)*100 #percent Black
length(which(data$race == 3)) #num American Indian or Alaska Native
length(which(data$race == 3))/nrow(data)*100 #percent American Indian/Alaska Native
length(which(data$race == 4)) #num Asian
length(which(data$race == 4))/nrow(data)*100 #percent Asian
length(which(data$race == 5)) #num Native Hawaiian or Pacific Islander
length(which(data$race == 5))/nrow(data)*100 #percent Native Hawaiian or Pacific Islander
length(which(data$race == 6)) #num Other
length(which(data$race == 6))/nrow(data)*100 #percent other

# Focused analysis of sites with "expert" or "a lot of knowledge about TMT" leads
# Still using exclusion set 1
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
t.test(data$pro_minus_anti~data$ms_condition)
describeBy(data$pro_minus_anti, group = data$ms_condition)
effsize::cohen.d(data$pro_minus_anti~data$ms_condition,pooled=TRUE,paired=FALSE,
                 na.rm=TRUE, hedges.correction=TRUE,
                 conf.level=0.95) #this was previously incorrectly indicating a positive value? Had to manually reverse for dissertation but seems fine now

# Computing alpha for the essay author ratings, basic exclusions
# Read data
data <- merged
# Applying exclusion criteria 1
# 1. Wrote something for both writing prompts
data <- subset(data, (data$msincomplete == 0 | is.na(data$msincomplete)))
# 2. Completed all six items evaluating the essay authors)
data <- subset(data, (!is.na(data$prous3) & !is.na(data$prous4) & !is.na(data$prous5) & !is.na(data$antius3) & !is.na(data$antius4) & !is.na(data$antius5)))

# create a data frame of only pro-us ratings for the alpha() function
pro_df <- data.frame(data$prous3,data$prous4,data$prous5)
psych::alpha(pro_df)
omega(pro_df) # Omega may be more appropriate

# create a data frame of only anti-us ratings for the alpha() function
anti_df <- data.frame(data$antius3,data$antius4,data$antius5)
psych::alpha(anti_df)
omega(anti_df)

###### Adding a clearer site label for tables

# Read per-site results according to the three exclusions criteria levels
combinedresults0 <- read.csv("./data/public/combinedresults0.csv")
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# Tidy up the sitesource variable into a descriptive label
combinedresults0$sitesource_label[combinedresults0$sitesource == "ufl"] <- "University of Florida"
combinedresults0$sitesource_label[combinedresults0$sitesource == "occid"] <- "Occidental College"
combinedresults0$sitesource_label[combinedresults0$sitesource == "ashland"] <- "Ashland University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "ithaca"] <- "Ithaca College"
combinedresults0$sitesource_label[combinedresults0$sitesource == "riverside"] <- "University of California, Riverside"
combinedresults0$sitesource_label[combinedresults0$sitesource == "wesleyan_inhouse"] <- "Wesleyan University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "uwmadison_expert"] <- "University of Wisconsin"
combinedresults0$sitesource_label[combinedresults0$sitesource == "uwmadison_inhouse"] <- "University of Wisconsin"
combinedresults0$sitesource_label[combinedresults0$sitesource == "vcu"] <- "Virginia Commonwealth University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "sou_inhouse"] <- "Southern Oregon University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "plu"] <- "Pacific Lutheran University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "byui"] <- "Brigham Young University – Idaho"
combinedresults0$sitesource_label[combinedresults0$sitesource == "azusa"] <- "Azusa Pacific University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "cnj"] <- "The College of New Jersey"
combinedresults0$sitesource_label[combinedresults0$sitesource == "wpi"] <- "Worcester Polytechnic Institute"
combinedresults0$sitesource_label[combinedresults0$sitesource == "illinois"] <- "University of Illinois"
combinedresults0$sitesource_label[combinedresults0$sitesource == "kansas_expert"] <- "University of Kansas"
combinedresults0$sitesource_label[combinedresults0$sitesource == "kansas_inhouse"] <- "University of Kansas"
combinedresults0$sitesource_label[combinedresults0$sitesource == "upenn"] <- "University of Pennsylvania"
combinedresults0$sitesource_label[combinedresults0$sitesource == "pace_inhouse"] <- "Pace University"
combinedresults0$sitesource_label[combinedresults0$sitesource == "pace_expert"] <- "Pace University"

combinedresults1$sitesource_label[combinedresults1$sitesource == "ufl"] <- "University of Florida"
combinedresults1$sitesource_label[combinedresults1$sitesource == "occid"] <- "Occidental College"
combinedresults1$sitesource_label[combinedresults1$sitesource == "ashland"] <- "Ashland University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "ithaca"] <- "Ithaca College"
combinedresults1$sitesource_label[combinedresults1$sitesource == "riverside"] <- "University of California, Riverside"
combinedresults1$sitesource_label[combinedresults1$sitesource == "wesleyan_inhouse"] <- "Wesleyan University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "uwmadison_expert"] <- "University of Wisconsin"
combinedresults1$sitesource_label[combinedresults1$sitesource == "uwmadison_inhouse"] <- "University of Wisconsin"
combinedresults1$sitesource_label[combinedresults1$sitesource == "vcu"] <- "Virginia Commonwealth University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "sou_inhouse"] <- "Southern Oregon University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "plu"] <- "Pacific Lutheran University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "byui"] <- "Brigham Young University – Idaho"
combinedresults1$sitesource_label[combinedresults1$sitesource == "azusa"] <- "Azusa Pacific University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "cnj"] <- "The College of New Jersey"
combinedresults1$sitesource_label[combinedresults1$sitesource == "wpi"] <- "Worcester Polytechnic Institute"
combinedresults1$sitesource_label[combinedresults1$sitesource == "illinois"] <- "University of Illinois"
combinedresults1$sitesource_label[combinedresults1$sitesource == "kansas_expert"] <- "University of Kansas"
combinedresults1$sitesource_label[combinedresults1$sitesource == "kansas_inhouse"] <- "University of Kansas"
combinedresults1$sitesource_label[combinedresults1$sitesource == "upenn"] <- "University of Pennsylvania"
combinedresults1$sitesource_label[combinedresults1$sitesource == "pace_inhouse"] <- "Pace University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "pace_expert"] <- "Pace University"

combinedresults2$sitesource_label[combinedresults2$sitesource == "ufl"] <- "University of Florida"
combinedresults2$sitesource_label[combinedresults2$sitesource == "occid"] <- "Occidental College"
combinedresults2$sitesource_label[combinedresults2$sitesource == "ashland"] <- "Ashland University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "ithaca"] <- "Ithaca College"
combinedresults2$sitesource_label[combinedresults2$sitesource == "riverside"] <- "University of California, Riverside"
combinedresults2$sitesource_label[combinedresults2$sitesource == "wesleyan_inhouse"] <- "Wesleyan University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "uwmadison_expert"] <- "University of Wisconsin"
combinedresults2$sitesource_label[combinedresults2$sitesource == "uwmadison_inhouse"] <- "University of Wisconsin"
combinedresults2$sitesource_label[combinedresults2$sitesource == "vcu"] <- "Virginia Commonwealth University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "sou_inhouse"] <- "Southern Oregon University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "plu"] <- "Pacific Lutheran University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "byui"] <- "Brigham Young University – Idaho"
combinedresults2$sitesource_label[combinedresults2$sitesource == "azusa"] <- "Azusa Pacific University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "cnj"] <- "The College of New Jersey"
combinedresults2$sitesource_label[combinedresults2$sitesource == "wpi"] <- "Worcester Polytechnic Institute"
combinedresults2$sitesource_label[combinedresults2$sitesource == "illinois"] <- "University of Illinois"
combinedresults2$sitesource_label[combinedresults2$sitesource == "kansas_expert"] <- "University of Kansas"
combinedresults2$sitesource_label[combinedresults2$sitesource == "kansas_inhouse"] <- "University of Kansas"
combinedresults2$sitesource_label[combinedresults2$sitesource == "upenn"] <- "University of Pennsylvania"
combinedresults2$sitesource_label[combinedresults2$sitesource == "pace_inhouse"] <- "Pace University"
combinedresults2$sitesource_label[combinedresults2$sitesource == "pace_expert"] <- "Pace University"

combinedresults3$sitesource_label[combinedresults3$sitesource == "ufl"] <- "University of Florida"
combinedresults3$sitesource_label[combinedresults3$sitesource == "occid"] <- "Occidental College"
combinedresults3$sitesource_label[combinedresults3$sitesource == "ashland"] <- "Ashland University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "ithaca"] <- "Ithaca College"
combinedresults3$sitesource_label[combinedresults3$sitesource == "riverside"] <- "University of California, Riverside"
combinedresults3$sitesource_label[combinedresults3$sitesource == "wesleyan_inhouse"] <- "Wesleyan University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "uwmadison_expert"] <- "University of Wisconsin"
combinedresults3$sitesource_label[combinedresults3$sitesource == "uwmadison_inhouse"] <- "University of Wisconsin"
combinedresults3$sitesource_label[combinedresults3$sitesource == "vcu"] <- "Virginia Commonwealth University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "sou_inhouse"] <- "Southern Oregon University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "plu"] <- "Pacific Lutheran University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "byui"] <- "Brigham Young University – Idaho"
combinedresults3$sitesource_label[combinedresults3$sitesource == "azusa"] <- "Azusa Pacific University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "cnj"] <- "The College of New Jersey"
combinedresults3$sitesource_label[combinedresults3$sitesource == "wpi"] <- "Worcester Polytechnic Institute"
combinedresults3$sitesource_label[combinedresults3$sitesource == "illinois"] <- "University of Illinois"
combinedresults3$sitesource_label[combinedresults3$sitesource == "kansas_expert"] <- "University of Kansas"
combinedresults3$sitesource_label[combinedresults3$sitesource == "kansas_inhouse"] <- "University of Kansas"
combinedresults3$sitesource_label[combinedresults3$sitesource == "upenn"] <- "University of Pennsylvania"
combinedresults3$sitesource_label[combinedresults3$sitesource == "pace_inhouse"] <- "Pace University"
combinedresults3$sitesource_label[combinedresults3$sitesource == "pace_expert"] <- "Pace University"

# overwrite .csv files
write.csv(combinedresults0, "./data/public/combinedresults0.csv", row.names = FALSE)
write.csv(combinedresults1, "./data/public/combinedresults1.csv", row.names = FALSE)
write.csv(combinedresults2, "./data/public/combinedresults2.csv", row.names = FALSE)
write.csv(combinedresults3, "./data/public/combinedresults3.csv", row.names = FALSE)