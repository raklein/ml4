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


#reading in experimenter survey, this was converted from .csv to .rds due to column names
# .csv is retained in the same directory
exp_surv <- readRDS("./data/raw_site_data/experimenter survey/exp_surv.rds")
names(exp_surv)
mean(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
range(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
sd(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
mean(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
sd(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
range(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
