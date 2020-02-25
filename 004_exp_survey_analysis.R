# Many Labs 4 Analysis Script for Experimenter Survey data
# Coder: Rick Klein raklein22@gmail.com
# OSF: https://osf.io/8ccnw/

##### NOTE -- THIS SCRIPT IS POTENTIALLY OUT OF DATE. WROTE THESE ANALYSES #### 
##### DIRECTLY INTO THE RMARKDOWN RESULTS SECTION: ml4_papaja_results.docx ####

# Written in RStudio Version 1.1.463, and R version 3.5.2

#The dataset is inherently sensitive so no deidentified dataset was created.
#Contact Rick Klein (raklein22@gmail.com) for more information or to possibly use the dataset.

#ANALYSIS SCRIPT FOR EXPERIMENTER SURVEY

# Open the .rproj file in R Studio to avoid setting the working directory.
# Otherwise, call setwd() with a path to the /ml4/ folder
# All file paths are relative from the working directory.


# Reading in experimenter survey, this was converted from .csv to .rds due to column names
# Original .csv is retained in the same directory
exp_surv <- readRDS("./data/raw_site_data/experimenter survey/exp_surv.rds")

# Variable names for an overview
names(exp_surv)

# Mean years of research experience
mean(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)

# Range years of research experience
range(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)

# SD years of research experience
sd(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)

# Mean estimated likelihood of replication success
mean(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)

# SD estimated likelihood of replication success
sd(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)

# Range estimated likelihood of replication success
range(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)

# Counts and percentages of different levels of 'knowledge about TMT'
exp_knowl_expert <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "Expert (highly knowledgeable, know the published literature and more)")
exp_knowl_alot <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "A lot (know theory in-depth, read many papers)")
exp_knowl_some <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "Some (familiar with, read a few papers)")
exp_knowl_alittle <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "A little (know about TMT, but not in-depth)")
exp_knowl_none <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "None (never heard of TMT until this project)")
exp_knowl_na <- sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "")

exp_knowl_expert_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "Expert (highly knowledgeable, know the published literature and more)")/nrow(exp_surv))*100
exp_knowl_alot_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "A lot (know theory in-depth, read many papers)")/nrow(exp_surv))*100
exp_knowl_some_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "Some (familiar with, read a few papers)")/nrow(exp_surv))*100
exp_knowl_alittle_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "A little (know about TMT, but not in-depth)")/nrow(exp_surv))*100
exp_knowl_none_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "None (never heard of TMT until this project)")/nrow(exp_surv))*100
exp_knowl_na_pct <- (sum(exp_surv$How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project. == "")/nrow(exp_surv))*100

# A simpler way:
# with(exp_surv,
#      table(How.much.knowledge.did.you.have.about.Terror.Management.Theory..prior.to.joining.this.project.,
#            useNA = 'ifany'))


# Experimenter "rooting for success/failure" data are messy, so I'll code them here
# Manually recoding some responses, providing coding here for verifiability
# Copy over all responses
exp_surv$rooting_coded <- exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT.
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "Happy either way.  Though for simple fear of our field imploding I root for replication!"] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "I'm rooting finding the true state of the world"] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "I am ambivalent. "] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "No dog in that race (ie no preference at all)"] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "No opinion"] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "No preference! "] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "no real view, though very interested to see if my site finds it less than others as we are not very \"pro-America\""] <- "Neither"
exp_surv$rooting_coded[exp_surv$Overall..are.you.rooting.for.the.cumulative.results.of.this.project.to.provide.evidence.for.or.against.TMT. == "I don't care"] <- "Neither"

# Counts rooting for, against, and neither
exp_rooting_for <- sum(exp_surv$rooting_coded == "For TMT")
exp_rooting_against <- sum(exp_surv$rooting_coded == "Against TMT")
exp_rooting_neither <- sum(exp_surv$rooting_coded == "Neither")
exp_rooting_na <-sum(exp_surv$rooting_coded == "")

# percentages
exp_rooting_for_pct <- (sum(exp_surv$rooting_coded == "For TMT")/nrow(exp_surv))*100
exp_rooting_against_pct <- (sum(exp_surv$rooting_coded == "Against TMT")/nrow(exp_surv))*100
exp_rooting_neither_pct <- (sum(exp_surv$rooting_coded == "Neither")/nrow(exp_surv))*100
exp_rooting_na_pct <- (sum(exp_surv$rooting_coded == "")/nrow(exp_surv))*100

