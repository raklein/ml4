# Many Labs 4 Analysis Script for Experimenter Survey data
# Coder: Rick Klein raklein22@gmail.com
# OSF: https://osf.io/8ccnw/

# Written in RStudio Version 1.1.463, and R version 3.5.2

#The dataset is inherently sensitive so no deidentified dataset was created.
#Contact Rick Klein (raklein22@gmail.com) for more information or to possibly use the dataset.

#ANALYSIS SCRIPT FOR EXPERIMENTER SURVEY

# Open the .rproj file in R Studio to avoid setting the working directory.
# Otherwise, call setwd() with a path to the /ml4/ folder
# All file paths are relative from the working directory.

library(tidyverse)

# Reading in experimenter survey, this was converted from .csv to .rds due to column names
# Original .csv is retained in the same directory
exp_surv <- readRDS("./data/raw_site_data/experimenter survey/exp_surv.rds") %>% 
  # mutate factors back to character
  mutate_if(is.factor, .funs = as.character)
# Variable names for an overview
names(exp_surv)
# Make nicer names
names(exp_surv) <- c("Timestamp", "Site", "email", "role", "degree", "years_exp", "pubs", "citations",
                     "tmt_know", "tmt_exp", "tmt_believe", "rooting", "rep_likely", "analyzed")
# Exclude responses from sites with n < 60, since they are not used in the confirmatory analysis
exp_surv <- filter(exp_surv, !(Site %in% c("Southern Oregon University, Ashland, Oregon",
                                           "Azusa Pacific University",
                                           "University of Kansas (Expert)",
                                           "Ashland University, Ashland, OH")))
# Glance at summary stats
select(exp_surv, years_exp:tmt_believe, rep_likely) %>% summary()

# Counts and percentages of different levels of 'knowledge about TMT'
exp_surv$tmt_know <- ifelse(exp_surv$tmt_know == "", NA, exp_surv$tmt_know) # set "" to NA
tab.exp_know <- with(exp_surv, table(tmt_know, useNA = 'ifany'))
tab.exp_know_pct  <- prop.table(tab.exp_know)*100
exp_knowl_expert      <- tab.exp_know[grep("Expert", dimnames(tab.exp_know)$tmt_know)]
exp_knowl_alot        <- tab.exp_know[grep("A lot", dimnames(tab.exp_know)$tmt_know)]
exp_knowl_some        <- tab.exp_know[grep("Some", dimnames(tab.exp_know)$tmt_know)]
exp_knowl_alittle     <- tab.exp_know[grep("A little", dimnames(tab.exp_know)$tmt_know)]
exp_knowl_none        <- tab.exp_know[grep("None", dimnames(tab.exp_know)$tmt_know)]
exp_knowl_na          <- tail(tab.exp_know, 1)
exp_knowl_expert_pct  <- tab.exp_know_pct[grep("Expert", dimnames(tab.exp_know_pct)$tmt_know)]
exp_knowl_alot_pct    <- tab.exp_know_pct[grep("A lot", dimnames(tab.exp_know_pct)$tmt_know)]
exp_knowl_some_pct    <- tab.exp_know_pct[grep("Some", dimnames(tab.exp_know_pct)$tmt_know)]
exp_knowl_alittle_pct <- tab.exp_know_pct[grep("A little", dimnames(tab.exp_know_pct)$tmt_know)]
exp_knowl_none_pct    <- tab.exp_know_pct[grep("None", dimnames(tab.exp_know_pct)$tmt_know)]
exp_knowl_na_pct      <- tail(tab.exp_know_pct, 1)

# Experimenter "rooting for success/failure" data are messy, so I'll code them here
# Manually recording these free responses as "neither"
exp_surv <- mutate(exp_surv, 
                   rooting_coded = case_when(rooting %in% c("Happy either way.  Though for simple fear of our field imploding I root for replication!",
                                                            "I'm rooting finding the true state of the world",
                                                            "I am ambivalent. ",
                                                            "No dog in that race (ie no preference at all)",
                                                            "No opinion",
                                                            "No preference! ",
                                                            "no real view, though very interested to see if my site finds it less than others as we are not very \"pro-America\"",
                                                            "I don't care") ~ "Neither",
                                             rooting == "" ~ NA_character_,
                                             T ~ as.character(rooting))
)

# Counts rooting for, against, and neither
tab.rooting <- with(exp_surv, table(rooting_coded, useNA = 'ifany'))
tab.rooting_pct <- prop.table(tab.rooting)*100

# experimenter knowledge
with(exp_surv, table(tmt_know, useNA = 'ifany'))

# estimated success, excluding data-peekers
filter(exp_surv, analyzed == "No") %>% 
  summarize(mean = mean(rep_likely, na.rm = T),
            sd = sd(rep_likely, na.rm = T))

