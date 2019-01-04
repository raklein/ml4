#This is rcode for the Many Labs 4 analysis of data collected about the experimenters.
#The dataset is inherently sensitive so no deidentified dataset was created.
#Contact Rick Klein (raklein22@gmail.com) for more information or to possibly use the dataset.

#Reproducibility: Change working directory, and datafiles are called from relative paths
#Written in RStudio Version 1.0.136, and R version 3.3.3

#ANALYSIS SCRIPT FOR EXPERIMENTER SURVEY

#set working directory
setwd("C:/Users/Rick/Desktop/Google Drive/Many Labs 4/data")

#reading in experimenter survey, this was converted from .csv to .rds due to column names
exp_surv <- readRDS("exp_surv.rds")
names(exp_surv)
mean(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
range(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
sd(exp_surv$How.many.years.of.experience.do.you.have.in.psychological.research.)
mean(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
sd(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
range(exp_surv$In.your.opinion..how.likely.is.it.that.overall.this.project..Many.Labs.4..will.successfully.replicate.Terror.Management.Theory...please.enter.a...between.0.and.100., na.rm=TRUE)
