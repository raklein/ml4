# Many Labs 4 data cleaning script
# Coder: Rick Klein raklein22@gmail.com
# OSF: https://osf.io/8ccnw/

# Function: Merges data from many sites involved in the project into one
# standardized data file for analysis. Raw data are required to run this script
# (placed under /data/raw_site_data). These contain confidential data thus are
# not available publicly. Contact Rick to obtain data.

# Open the .rproj file in R Studio to avoid setting the working directory. 
# All file paths are relative from the working directory.

# See output/sessionInfo_data_cleaning.txt for version information

# uncomment these lines to install required packages:

# install.packages("metafor")
# install.packages("metaSEM")
# install.packages("haven")
# install.packages("psych")
# install.packages("tidyverse")
# install.packages("effsize")

library(metafor)
library(metaSEM)
library(haven)
library(psych)
library(tidyverse)
library(effsize)

# Save information about package versions to a file, may help others 
# reproduce results.
writeLines(capture.output(sessionInfo()), "./output/sessionInfo_data_cleaning.txt")

# Read in data template, teams were asked to format their data in a similar fashion. Key found in data.key.docx.
template <- read.csv("./data/public/data.template.csv", header = TRUE, stringsAsFactor = FALSE)

# Below, I reformat all data from individual sites into this format.
# Note: For in-house sites, they often have variables corresponding
# to those above, but they are not always in a clear format. When in doubt,
# I left them out, although these could be added later with closer examination.

# Read in Occidental College Expert data
occid <- read.csv("./data/raw_site_data/Occidental College Expert/Oxy_TMT_data.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition, 1 = expert 0 = in-house
occid$expert <- 1
# add site identifier
occid$source <- "occid"
# add location, usually identical to source
occid$location <- "occid"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# occid <- readRDS("./data/raw_site_data/Occidental College Expert/occid.rds")

# read in College of New Jersey expert data
cnj <- read_sav("./data/raw_site_data/The College of New Jersey expert/TCNJ ML4 completed data set.sav")
# add column indicating it's from the expert condition
cnj$expert <- 1
# add site identifier
cnj$source <- "cnj"
# add location, usually identical to source
cnj$location <- "cnj"
# renaming columns to match template
names(cnj)[names(cnj) == 'participantnumber'] <- 'participantnum'
names(cnj)[names(cnj) == 'condition'] <- 'ms_condition'
names(cnj)[names(cnj) == 'essayorder'] <- 'dv_order'
names(cnj)[names(cnj) == 'provalid'] <- 'prous1'
names(cnj)[names(cnj) == 'proagree'] <- 'prous2'
names(cnj)[names(cnj) == 'prointelligent'] <- 'prous3'
names(cnj)[names(cnj) == 'prolike'] <- 'prous4'
names(cnj)[names(cnj) == 'proknowledge'] <- 'prous5'
names(cnj)[names(cnj) == 'antivalid'] <- 'antius1'
names(cnj)[names(cnj) == 'antiagree'] <- 'antius2'
names(cnj)[names(cnj) == 'antiintelligent'] <- 'antius3'
names(cnj)[names(cnj) == 'antilike'] <- 'antius4'
names(cnj)[names(cnj) == 'antiknowledge'] <- 'antius5'
names(cnj)[names(cnj) == 'country'] <- 'countryofbirth'
names(cnj)[names(cnj) == 'ideology'] <- 'politicalid'
names(cnj)[names(cnj) == 'patriotism'] <- 'americanid'
names(cnj)[names(cnj) == 'writingpromptblank'] <- 'msincomplete'
# dropping last three columns with data we don't need (filter and averages which we will compute ourselves)
drops <- c("filter_$","proAmericanrating", "antiAmericanrating")
cnj <- cnj[ , !(names(cnj) %in% drops)]
# recode responses to template, can see coding in original .sav file
# convert columns to character type to make this change
cnj <- data.frame(lapply(cnj, as.character), stringsAsFactors=FALSE)
# 1 = pro- essay first, 2 = anti-essay first
cnj$dv_order[cnj$dv_order==1] <- "pro-first"
cnj$dv_order[cnj$dv_order==2] <- "anti-first"
# 0 = TV condition, 1 = MS condition
cnj$ms_condition[cnj$ms_condition==0] <- "tv"
cnj$ms_condition[cnj$ms_condition==1] <- "ms"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# cnj <- readRDS("./data/raw_site_data/The College of New Jersey expert/cnj.rds")

# read in UC riverside expert data
riverside <- read.csv("./data/raw_site_data/UC riverside expert/ML4 data combined.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
riverside$expert <- 1
# add site identifier
riverside$source <- "riverside"
# add location, usually identical to source
riverside$location <- "riverside"
# adding two missing variables from template as NA
riverside$date <- NA
riverside$time <- NA
# riverside site used "0" for blank responses
riverside[riverside == 0] <- NA
riverside$msincomplete <- 0 # this one variable actually used 0 as a value, so changing it back
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# riverside <- readRDS("./data/raw_site_data/UC riverside expert/riverside.rds")

# read in UW madison expert data
uwmadison_expert <- read.csv("./data/raw_site_data/UWmadison expert/Paper-and-pencil condition_Data Entry complete_UW-Madison.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
uwmadison_expert$expert <- 1
# add site identifier
uwmadison_expert$source <- "uwmadison_expert"
# add location, similar to source but omits "_expert"
uwmadison_expert$location <- "uwmadison"
# recoding gender
uwmadison_expert$gender[uwmadison_expert$gender=="F"] <- 1
uwmadison_expert$gender[uwmadison_expert$gender=="M"] <- 2
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# uwmadison_expert <- readRDS("./data/raw_site_data/UWmadison expert/uwmadison_expert.rds")

# read in Azusa in-house data
azusa <- read_sav("./data/raw_site_data/azusa inhouse/TMT_replication_APU site.sav")
# add column indicating it's from the inhouse condition
azusa$expert <- 0
# add site identifier
azusa$source <- "azusa"
# add location, usually identical to source
azusa$location <- "azusa"
# cleaning up variable names
names(azusa)[names(azusa) == 'part_ID'] <- 'participantnum'
names(azusa)[names(azusa) == 'Date'] <- 'date'
names(azusa)[names(azusa) == 'TIme'] <- 'time'
names(azusa)[names(azusa) == 'MS_condition'] <- 'ms_condition'
names(azusa)[names(azusa) == 'p_valid'] <- 'prous1'
names(azusa)[names(azusa) == 'p_agree'] <- 'prous2'
names(azusa)[names(azusa) == 'p_intel'] <- 'prous3'
names(azusa)[names(azusa) == 'p_like'] <- 'prous4'
names(azusa)[names(azusa) == 'p_knowl'] <- 'prous5'
names(azusa)[names(azusa) == 'a_valid'] <- 'antius1'
names(azusa)[names(azusa) == 'a_agree'] <- 'antius2'
names(azusa)[names(azusa) == 'a_intel'] <- 'antius3'
names(azusa)[names(azusa) == 'a_liking'] <- 'antius4'
names(azusa)[names(azusa) == 'a_knowl'] <- 'antius5'
names(azusa)[names(azusa) == 'country_of_birth'] <- 'countryofbirth'
names(azusa)[names(azusa) == 'race_r_TEXT'] <- 'race_other'
# race.azusa coded differently from template: 1. white 2. black 3. hispanic 4. asian 5. biracial 6. other
names(azusa)[names(azusa) == 'race'] <- 'race.azusa'
# convert to string to make recoding changes
azusa <- data.frame(lapply(azusa, as.character), stringsAsFactors=FALSE)
# 1 = ms 2 = tv
azusa$ms_condition[azusa$ms_condition==2] <- "tv"
azusa$ms_condition[azusa$ms_condition==1] <- "ms"
# recoding gender
azusa$gender[azusa$gender==2] <- "female"
azusa$gender[azusa$gender==1] <- "male"
azusa$gender[azusa$gender=="female"] <- "1"
azusa$gender[azusa$gender=="male"] <- "2"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# azusa <- readRDS("./data/raw_site_data/azusa inhouse/azusa.rds")

# read in Ithaca in-house data
ithaca <- read.csv("./data/raw_site_data/ithaca inhouse/Ithaca data.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
ithaca$expert <- 0
# add site identifier
ithaca$source <- "ithaca"
# add location, usually identical to source
ithaca$location <- "ithaca"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# ithaca <- readRDS("./data/raw_site_data/ithaca inhouse/ithaca.rds")

# read in wpi in-house data
wpi <- read.csv("./data/raw_site_data/wpi inhouse/A13.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
wpi$expert <- 0
# add site identifier
wpi$source <- "wpi"
# add location, usually identical to source
wpi$location <- "wpi"
# race.wpi may be coded differently from template
names(wpi)[names(wpi) == 'race'] <- 'race.wpi'
# politicalid.wpi may be coded differently from template
names(wpi)[names(wpi) == 'politicalid'] <- 'politicalid.wpi'
# change some variable names to match template
names(wpi)[names(wpi) == 'countryofbirth (187 = US)'] <- 'countryofbirth'
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# wpi <- readRDS("./data/raw_site_data/wpi inhouse/wpi.rds")

# read in ufl in-house data
ufl <- read_sav("./data/raw_site_data/ufl inhouse/ml4.clean.sav")
# add column indicating it's from the inhouse condition
ufl$expert <- 0
# add site identifier
ufl$source <- "ufl"
# add location, usually identical to source
ufl$location <- "ufl"
# convert to strings for needed recoding
ufl <- data.frame(lapply(ufl, as.character), stringsAsFactors=FALSE)
# rename to match template
names(ufl)[names(ufl) == 'proamericaneval1'] <- 'prous1'
names(ufl)[names(ufl) == 'proamericaneval2'] <- 'prous2'
names(ufl)[names(ufl) == 'proamericaneval3'] <- 'prous3'
names(ufl)[names(ufl) == 'proamericaneval4'] <- 'prous4'
names(ufl)[names(ufl) == 'proamericaneval5'] <- 'prous5'
names(ufl)[names(ufl) == 'antiamericaneval1'] <- 'antius1'
names(ufl)[names(ufl) == 'antiamericaneval2'] <- 'antius2'
names(ufl)[names(ufl) == 'antiamericaneval3'] <- 'antius3'
names(ufl)[names(ufl) == 'antiamericaneval4'] <- 'antius4'
names(ufl)[names(ufl) == 'antiamericaneval5'] <- 'antius5'
names(ufl)[names(ufl) == 'citizenship'] <- 'countryofbirth'
names(ufl)[names(ufl) == 'condition'] <- 'ms_condition'
# recode to match template
ufl$dv_order[ufl$dv_order=="1"] <- "pro-first"
ufl$dv_order[ufl$dv_order=="2"] <- "anti-first"
ufl$ms_condition[ufl$ms_condition=="1"] <- "tv"
ufl$ms_condition[ufl$ms_condition=="2"] <- "ms"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# ufl <- readRDS("./data/raw_site_data/ufl inhouse/ufl.rds")

# read in illinois in-house data
illinois <- read.csv("./data/raw_site_data/universityillinois inhouse/ML4_Storage_Dataset.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
illinois$expert <- 0
# add site identifier
illinois$source <- "illinois"
# add location, usually identical to source
illinois$location <- "illinois"
names(illinois)[names(illinois) == 'dv.order'] <- 'dv_order'
names(illinois)[names(illinois) == 'msincomplete..1...incomplete..0...complete.'] <- 'msincomplete'
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# illinois <- readRDS("./data/raw_site_data/universityillinois inhouse/illinois.rds")

# read in upenn inhouse data
upenn <- read.csv("./data/raw_site_data/upenn inhouse/Greenberg Data Recoded.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
upenn$expert <- 0
# add site identifier
upenn$source <- "upenn"
# add location, usually identical to source
upenn$location <- "upenn"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# upenn <- readRDS("./data/raw_site_data/upenn inhouse/upenn.rds")

# read in UWmadison inhouse data
# note: this was giving me encoding errors so I manually recoded to UTF-8,
# original .csv in /old
uwmadison_inhouse <- read.csv("./data/raw_site_data/UWmadison inhouse/Terror_Management.csv", header = TRUE, stringsAsFactor = FALSE, encoding = "UTF-8")
# add column indicating it's from the inhouse condition
uwmadison_inhouse$expert <- 0
# add site identifier
uwmadison_inhouse$source <- "uwmadison_inhouse"
# add location, usually identical to source
uwmadison_inhouse$location <- "uwmadison"
# convert to strings for needed recoding
uwmadison_inhouse <- data.frame(lapply(uwmadison_inhouse, as.character), stringsAsFactors=FALSE)
# rename to match template
# these are from 1-7
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.liked.the.author.of.the.first.essay'] <- 'prous4'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.author.of.the.first.essay.was.intelligent'] <- 'prous3'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.author.of.the.first.essay.was.knowledgeable'] <- 'prous5'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.agree.with.the.arguments.of.the.first.essay'] <- 'prous2'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.arguments.in.the.first.essay.were.valid'] <- 'prous1'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.liked.the.author.of.the.second.essay'] <- 'antius4'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.author.of.the.second.essay.was.intelligent'] <- 'antius3'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.author.of.the.second.essay.was.knowledgeable'] <- 'antius5'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.agree.with.the.arguments.of.the.second.essay'] <- 'antius2'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'Click.to.write.the.question.text.I.think.the.arguments.in.the.second.essay.were.valid'] <- 'antius1'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'What.is.your.gender'] <- 'gender'
names(uwmadison_inhouse)[names(uwmadison_inhouse) == 'How.old.are.you.'] <- 'age'
# no condition variable exists, so creating one based on whether they responded to the tv or ms prompt
uwmadison_inhouse$ms_condition <- NA
uwmadison_inhouse$ms_condition[nchar(uwmadison_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you.) > 1] <- "ms"
uwmadison_inhouse$ms_condition[nchar(uwmadison_inhouse$X.Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you.) > 1] <- "tv"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# uwmadison_inhouse <- readRDS("./data/raw_site_data/UWmadison inhouse/uwmadison_inhouse.rds")

# read in kansas inhouse data
kansas_inhouse <- read.csv("./data/raw_site_data/kansas inhouse/in house condition, Kansas, TMT replication.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
kansas_inhouse$expert <- 0
# add site identifier
kansas_inhouse$source <- "kansas_inhouse"
# add location, usually identical to source
kansas_inhouse$location <- "kansas"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# kansas_inhouse <- readRDS("./data/raw_site_data/kansas inhouse/kansas_inhouse.rds")

# read in Pace expert data
pace_expert <- read.csv("./data/raw_site_data/pace expert/mancini_gosnell_Pace_expert_TMT_replication_data_FINAL_2017.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
pace_expert$expert <- 1
# add site identifier
pace_expert$source <- "pace_expert"
# add location, usually identical to source
pace_expert$location <- "pace"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# pace_expert <- readRDS("./data/raw_site_data/pace expert/pace_expert.rds")

# read in wesleyan inhouse data
# double-check politicalID coding
wesleyan_inhouse <- read.csv("./data/raw_site_data/wesleyan inhouse/schmidtmsrep.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
wesleyan_inhouse$expert <- 0
# add site identifier
wesleyan_inhouse$source <- "wesleyan_inhouse"
# add location, usually identical to source
wesleyan_inhouse$location <- "wesleyan"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# wesleyan_inhouse <- readRDS("./data/raw_site_data/wesleyan inhouse/wesleyan_inhouse.rds")

# read in sou inhouse data
sou_inhouse <- read.csv("./data/raw_site_data/sou inhouse/data to send.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
sou_inhouse$expert <- 0
# add site identifier
sou_inhouse$source <- "sou_inhouse"
sou_inhouse$location <- "sou"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# sou_inhouse <- readRDS("./data/raw_site_data/sou inhouse/sou_inhouse.rds")

# read in Kansas expert data
kansas_expert <- read.csv("./data/raw_site_data/kansas expert/Swanson_KUdata.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
kansas_expert$expert <- 1
# add site identifier
kansas_expert$source <- "kansas_expert"
kansas_expert$location <- "kansas"
# convert to strings for needed recoding
kansas_expert <- data.frame(lapply(kansas_expert, as.character), stringsAsFactors=FALSE)
# rename to match template
names(kansas_expert)[names(kansas_expert) == 'PROMPT'] <- 'ms_condition'
names(kansas_expert)[names(kansas_expert) == 'ORDER_first'] <- 'dv_order'
names(kansas_expert)[names(kansas_expert) == 'P_ID'] <- 'participantnum'
# recode to match template
kansas_expert$dv_order[kansas_expert$dv_order=="Anti-US"] <- "anti-first"
kansas_expert$dv_order[kansas_expert$dv_order=="Pro-US"] <- "pro-first"
kansas_expert$ms_condition[kansas_expert$ms_condition=="Death"|kansas_expert$ms_condition=="death"] <- "ms"
kansas_expert$ms_condition[kansas_expert$ms_condition=="TV"|kansas_expert$ms_condition=="Tv"|kansas_expert$ms_condition=="tv"] <- "tv"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# kansas_expert <- readRDS("./data/raw_site_data/kansas expert/kansas_expert.rds")

# read in PLU inhouse data
plu <- read.csv("./data/raw_site_data/plu/Pacific Lutheran University Many Labs 4 Clean Data.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
plu$expert <- 0
# add site identifier
plu$source <- "plu"
plu$location <- "plu"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# plu <- readRDS("./data/raw_site_data/plu/plu.rds")

# read in ashland expert data
ashland <- read.csv("./data/raw_site_data/ashland/ML 4 AU Data Chartier & Brady.csv", header = TRUE, stringsAsFactor = FALSE)
# it's reading a bunch of empty cells, selecting only those with actual data
ashland <- ashland[1:56,]
# add column indicating it's from the expert condition
ashland$expert <- 1
# add site identifier
ashland$source <- "ashland"
# renaming to fit template
ashland$dv_order[ashland$dv_order=="neg-first"] <- "anti-first"
ashland$location <- "ashland"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# ashland <- readRDS("./data/raw_site_data/ashland/ashland.rds")

# read in vcu expert data
vcu <- read.csv("./data/raw_site_data/vcu/manylabsVCU.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
vcu$expert <- 1
# add site identifier
vcu$source <- "vcu"
vcu$location <- "vcu"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# vcu <- readRDS("./data/raw_site_data/vcu/vcu.rds")

# read in BYU in-house data
byui <- read.csv("./data/raw_site_data/byui_expert/ML4 BYUI Wiggins.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
byui$expert <- 1
# add site identifier
byui$source <- "byui"
# add location, usually identical to source
byui$location <- "byui"

# read in Pace in-house data
# Note: I saved the original data (TMT.xlsx) but did some manual reformatting
# to TMT.csv
pace_inhouse <- read.csv("./data/raw_site_data/pace_inhouse/TMT.csv", sep = ";", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the in-house condition
pace_inhouse$expert <- 0
# add site identifier
pace_inhouse$source <- "pace_inhouse"
# add location, usually identical to source
pace_inhouse$location <- "pace"
# change column names to match template
# author A is always anti-us and author P is always pro-us
names(pace_inhouse)[names(pace_inhouse) == 'How.much.do.you.like.Author.P.'] <- 'prous4'
names(pace_inhouse)[names(pace_inhouse) == 'How.intelligent.is.Author.P.'] <- 'prous3'
names(pace_inhouse)[names(pace_inhouse) == 'How.knowledgeable.about.America.is.Author.P.'] <- 'prous5'
names(pace_inhouse)[names(pace_inhouse) == 'How.much.do.you.agree.with.Author.P.s.essay.'] <- 'prous2'
names(pace_inhouse)[names(pace_inhouse) == 'How.valid..true.or.logical..are.Author.P.s.arguments.'] <- 'prous1'
names(pace_inhouse)[names(pace_inhouse) == 'How.much.do.you.like.Author.A.'] <- 'antius4'
names(pace_inhouse)[names(pace_inhouse) == 'How.intelligent.is.Author.A.'] <- 'antius3'
names(pace_inhouse)[names(pace_inhouse) == 'How.knowledgeable.about.America.is.Author.A.'] <- 'antius5'
names(pace_inhouse)[names(pace_inhouse) == 'How.much.do.you.agree.with.Author.A.s.essay.'] <- 'antius2'
names(pace_inhouse)[names(pace_inhouse) == 'How.valid..true.or.logical..are.Author.A.s.arguments.'] <- 'antius1'
names(pace_inhouse)[names(pace_inhouse) == 'What.is.your.gender.'] <- 'gender'
names(pace_inhouse)[names(pace_inhouse) == 'What.is.your.age.'] <- 'age'
# no condition variable exists, so creating one based on whether they responded to the tv or ms prompt
pace_inhouse$ms_condition <- NA
pace_inhouse$ms_condition[nchar(pace_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you.) > 1] <- "ms"
pace_inhouse$ms_condition[nchar(pace_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you.) > 1] <- "tv"
# For users having trouble with .csv: can uncomment below line and read processed .rds
# pace_inhouse <- readRDS("./data/raw_site_data/pace_inhouse/TMT.rds")

# convert all dataframe columns to character for easier merging
riverside <- data.frame(lapply(riverside, as.character), stringsAsFactors=FALSE)
uwmadison_expert <- data.frame(lapply(uwmadison_expert, as.character), stringsAsFactors=FALSE)
occid <- data.frame(lapply(occid, as.character), stringsAsFactors=FALSE)
cnj <- data.frame(lapply(cnj, as.character), stringsAsFactors=FALSE)
azusa <- data.frame(lapply(azusa, as.character), stringsAsFactors=FALSE)
ithaca <- data.frame(lapply(ithaca, as.character), stringsAsFactors=FALSE)
wpi <- data.frame(lapply(wpi, as.character), stringsAsFactors=FALSE)
ufl <- data.frame(lapply(ufl, as.character), stringsAsFactors=FALSE)
illinois <- data.frame(lapply(illinois, as.character), stringsAsFactors=FALSE)
upenn <- data.frame(lapply(upenn, as.character), stringsAsFactors=FALSE)
uwmadison_inhouse <- data.frame(lapply(uwmadison_inhouse, as.character), stringsAsFactors=FALSE)
kansas_inhouse <- data.frame(lapply(kansas_inhouse, as.character), stringsAsFactors=FALSE)
pace_expert <- data.frame(lapply(pace_expert, as.character), stringsAsFactors=FALSE)
wesleyan_inhouse <- data.frame(lapply(wesleyan_inhouse, as.character), stringsAsFactors=FALSE)
sou_inhouse <- data.frame(lapply(sou_inhouse, as.character), stringsAsFactors=FALSE)
kansas_expert <- data.frame(lapply(kansas_expert, as.character), stringsAsFactors=FALSE)
plu <- data.frame(lapply(plu, as.character), stringsAsFactors=FALSE)
ashland <- data.frame(lapply(ashland, as.character), stringsAsFactors=FALSE)
vcu <- data.frame(lapply(vcu, as.character), stringsAsFactors=FALSE)
byui <- data.frame(lapply(byui, as.character), stringsAsFactors=FALSE)
pace_inhouse <- data.frame(lapply(pace_inhouse, as.character), stringsAsFactors=FALSE)

# merging data frames vertically
merged <- bind_rows(template,
                    riverside, 
                    uwmadison_expert, 
                    cnj, 
                    occid, 
                    azusa, 
                    ithaca, 
                    wpi, 
                    ufl, 
                    illinois, 
                    upenn, 
                    uwmadison_inhouse, 
                    kansas_inhouse, 
                    pace_expert,
                    wesleyan_inhouse,
                    sou_inhouse,
                    kansas_expert,
                    plu,
                    ashland,
                    vcu,
                    byui,
                    pace_inhouse
)

# convert columns back to numeric/factor where needed
merged$prous1 <- as.numeric(as.character(merged$prous1))
merged$prous2 <- as.numeric(as.character(merged$prous2))
merged$prous3 <- as.numeric(as.character(merged$prous3))
merged$prous4 <- as.numeric(as.character(merged$prous4))
merged$prous5 <- as.numeric(as.character(merged$prous5))
merged$antius1 <- as.numeric(as.character(merged$antius1))
merged$antius2 <- as.numeric(as.character(merged$antius2))
merged$antius3 <- as.numeric(as.character(merged$antius3))
merged$antius4 <- as.numeric(as.character(merged$antius4))
merged$antius5 <- as.numeric(as.character(merged$antius5))
merged$expert <- as.numeric(as.character(merged$expert))
merged$ms_condition <- as.factor(as.character(merged$ms_condition))
merged$msincomplete <- as.numeric(as.character(merged$msincomplete))
merged$countryofbirth <- as.numeric(as.character(merged$countryofbirth))
merged$ethnicity <- as.numeric(as.character(merged$ethnicity))
merged$race <- as.numeric(as.character(merged$race))
merged$americanid <- as.numeric(as.character(merged$americanid))

# We appear to have some entirely NA rows, except meta data. To address this I'm
# going to remove responses that are blank in all of these variables:
# 1) did not respond to any of the author rating items and 2) also did not have
# an experimental condition assignment. 
# I think this is a conservative approach that does not lose any real data, at trade-off
# of retaining some rows with little/no information.
merged <- subset(merged, (!is.na(merged$prous3) | !is.na(merged$prous4) | !is.na(merged$prous5) | !is.na(merged$antius3) | !is.na(merged$antius4) | !is.na(merged$antius5) | !is.na(merged$ms_condition)))

# If you skip these lines, you'll later find we have an issue with the # of levels
# in data$ms_condition. This is a common problem where a "phantom" level with
# zero measurements will appear in a factor. I'll demonstrate the problem and 
# fix it below.
levels(merged$ms_condition)
summary(merged$ms_condition)
# Note the phantom third level with zero observations. Need to drop it.
merged$ms_condition <- factor(merged$ms_condition, levels = c("ms", "tv"))

merged <- filter(merged, ms_condition == "ms" | ms_condition == "tv")

# compute primary indexes (mean of pro-US author ratings minus mean of anti-US author ratings)
merged$proauth_avg <- rowMeans(merged[, c('prous3','prous4','prous5')], na.rm = TRUE)
merged$antiauth_avg <- rowMeans(merged[, c('antius3','antius4','antius5')], na.rm = TRUE)
merged$pro_minus_anti <- merged$proauth_avg - merged$antiauth_avg # primary outcome variable, higher scores = greater preference for pro-US author

# save .csv
write.csv(merged, "./data/processed_data/merged.csv",row.names=FALSE)
# save .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged, "./data/processed_data/merged.rds")

# Create deidentified dataset by dropping the following variables
merged_deidentified <- merged

# dropping open text responses used at some sites, possibly identifying
merged_deidentified$MS1 <- NULL 
merged_deidentified$MS2 <- NULL
merged_deidentified$control1 <- NULL
merged_deidentified$control2 <- NULL
merged_deidentified$race_6_TEXT <- NULL
merged_deidentified$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you. <- NULL
merged_deidentified$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and... <- NULL
merged_deidentified$Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you. <- NULL
merged_deidentified$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and... <- NULL
merged_deidentified$The.one.thing.I.fear.the.most.about.my.death.is. <- NULL
merged_deidentified$My.scariest.thoughts.about.my.death.are. <- NULL
merged_deidentified$race.elaborate <- NULL
merged_deidentified$politicalviews_other <- NULL
merged_deidentified$Did.anything.in.the.survey.strike.you.as.odd.or.unusual. <- NULL
merged_deidentified$Sometimes.in.psychology.studies..participants.believe.there.is.more.going.on.than.meets.the.eye.... <- NULL
merged_deidentified$controlresponse2 <- NULL
merged_deidentified$mortalityresponse1 <- NULL
merged_deidentified$mortalityresponse2 <- NULL
merged_deidentified$controlresponse1 <- NULL
merged_deidentified$Television1 <- NULL
merged_deidentified$Television2 <- NULL
merged_deidentified$SubtleOwnDeath1 <- NULL
merged_deidentified$SubtleOwnDeath2 <- NULL
merged_deidentified$major <- NULL
merged_deidentified$The.one.thing.I.fear.most.about.my.death.is. <- NULL
merged_deidentified$My.scariest.thoughts.about.death.are. <- NULL
merged_deidentified$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television.and.o... <- NULL
merged_deidentified$The.one.thing.I.fear.most.about.television.is. <- NULL
merged_deidentified$My.scariest.thoughts.about.television.are. <- NULL
merged_deidentified$Language <- NULL
merged_deidentified$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and.once.you.are.physically.dead. <- NULL
merged_deidentified$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and.once.you.have.physically.watched.television. <- NULL

# dropping Qualtrics-recorded location data, and similar
merged_deidentified$LocationLatitude <- NULL
merged_deidentified$LocationLongitude <- NULL
merged_deidentified$LocationAccuracy <- NULL
merged_deidentified$IP.Address <- NULL

# dropping what may be ID numbers of some kind
merged_deidentified$WBL_ID <- NULL 
merged_deidentified$Respondent.ID <- NULL
merged_deidentified$Collector.ID <- NULL

# dropping potentially triangulating data
merged_deidentified$age <- NULL
merged_deidentified$time <- NULL
merged_deidentified$gender <- NULL
merged_deidentified$ethnicity <- NULL
merged_deidentified$ethnicity..1...White.Caucasian..2...Middle.Eastern..3...Asian.Pacific.Islander..4...African.American.Black..5...Hispanic.Latino..6...Indigenous.Aboriginal..7...Would.Rather.Not.Say..8...Other <- NULL
merged_deidentified$politicalid <- NULL
merged_deidentified$politicalid.wpi <- NULL
merged_deidentified$politicalview..1.Republican..2...Democrat..3...Independent..4...Other..5...No.Preference. <- NULL
merged_deidentified$politicalparty..1...Republican..2...Democrat..3...Libertarian..4...Green..5...Constitution..6...Independent..7...I.don.t.identify.with.a.political.party..8...Other. <- NULL
merged_deidentified$countryofbirth..187...US. <- NULL
merged_deidentified$birthcountry <- NULL
merged_deidentified$raceombmulti <- NULL
merged_deidentified$In.what.country.were.you.born. <- NULL
merged_deidentified$race.azusa <- NULL
merged_deidentified$race.wpi <- NULL
merged_deidentified$raceomb <- NULL

# for good measure, let's drop all unique questions asked by sites (hard to police exactly what was asked at each site)
merged_deidentified <- select(merged_deidentified, participantnum:location, proauth_avg:pro_minus_anti)

# save deidentified .csv
write.csv(merged_deidentified, "./data/public/merged_deidentified.csv",row.names=FALSE)
# save deidentified .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged_deidentified, "./data/public/merged_deidentified.rds")
