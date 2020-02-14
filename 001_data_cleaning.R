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
# writeLines(capture.output(sessionInfo()), "./output/sessionInfo_data_cleaning.txt")

# Read in data template, teams were asked to format their data in a similar fashion. 
# Key found in data.key.docx.
template <- read.csv("./data/public/data.template.csv", header = TRUE, stringsAsFactor = FALSE)

# Below, I reformat all data from individual sites into this format.
# Note: For in-house sites, they often have variables corresponding
# to those above, but they are not always in a clear format. When in doubt,
# I left them out, although these could be added later with closer examination.

# Read in Occidental College Expert data ----
occid <- read.csv("./data/raw_site_data/Occidental College Expert/Oxy_TMT_data.csv", 
                  header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition, 1 = expert 0 = in-house
occid$expert <- 1
# add site identifier
occid$source <- "occid"
# add location, usually identical to source
occid$location <- "occid"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# occid <- readRDS("./data/raw_site_data/Occidental College Expert/occid.rds")

# read in College of New Jersey expert data ----
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

# read in UC riverside expert data ----
riverside <- read.csv("./data/raw_site_data/UC riverside expert/ML4 data combined.csv", 
                      header = TRUE, stringsAsFactor = FALSE)
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
riverside$msincomplete[is.na(riverside$msincomplete)] <- 0 # this variable actually used 0 as a value, so changing it back
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# riverside <- readRDS("./data/raw_site_data/UC riverside expert/riverside.rds")

# read in UW madison expert data ----
uwmadison_expert <- read.csv("./data/raw_site_data/UWmadison expert/Paper-and-pencil condition_Data Entry complete_UW-Madison.csv", 
                             header = TRUE, stringsAsFactor = FALSE)
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

# read in Azusa in-house data ----
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
# recode race from race.azusa
azusa$race <- case_when(azusa$race.azusa == "3" ~ NA_character_, # don't know "race" of hispanic participants
                        azusa$race.azusa == "5" ~ NA_character_, # don't know "races" of biracial participants
                        TRUE                    ~ azusa$race.azusa) # otherwise retain codes
# generate ethnicity: participants who ID'd as "hispanic" are hispanic
azusa$ethnicity <- ifelse(azusa$race.azusa == 3, 2, 1)
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

# read in Ithaca in-house data ----
ithaca <- read.csv("./data/raw_site_data/ithaca inhouse/Ithaca data.csv", 
                   header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
ithaca$expert <- 0
# add site identifier
ithaca$source <- "ithaca"
# add location, usually identical to source
ithaca$location <- "ithaca"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# ithaca <- readRDS("./data/raw_site_data/ithaca inhouse/ithaca.rds")

# read in wpi in-house data ----
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

# read in ufl in-house data ----
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

# read in illinois in-house data ----
illinois <- read.csv("./data/raw_site_data/universityillinois inhouse/ML4_Storage_Dataset.csv", 
                     header = TRUE, stringsAsFactor = FALSE)
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

# read in upenn inhouse data ----
upenn <- read.csv("./data/raw_site_data/upenn inhouse/Greenberg Data Recoded.csv", 
                  header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
upenn$expert <- 0
# add site identifier
upenn$source <- "upenn"
# add location, usually identical to source
upenn$location <- "upenn"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# upenn <- readRDS("./data/raw_site_data/upenn inhouse/upenn.rds")

# read in UWmadison inhouse data ----
# note: this was giving me encoding errors so I manually recoded to UTF-8,
# original .csv in /old
uwmadison_inhouse <- read.csv("./data/raw_site_data/UWmadison inhouse/Terror_Management.csv", 
                              header = TRUE, stringsAsFactor = FALSE, encoding = "UTF-8")
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
uwmadison_inhouse <- rename(uwmadison_inhouse, 
                            prous4  = Click.to.write.the.question.text.I.liked.the.author.of.the.first.essay, 
                            prous3  = Click.to.write.the.question.text.I.think.the.author.of.the.first.essay.was.intelligent,   
                            prous5  = Click.to.write.the.question.text.I.think.the.author.of.the.first.essay.was.knowledgeable, 
                            prous2  = Click.to.write.the.question.text.I.agree.with.the.arguments.of.the.first.essay,           
                            prous1  = Click.to.write.the.question.text.I.think.the.arguments.in.the.first.essay.were.valid,     
                            antius4 = Click.to.write.the.question.text.I.liked.the.author.of.the.second.essay,                  
                            antius3 = Click.to.write.the.question.text.I.think.the.author.of.the.second.essay.was.intelligent,  
                            antius5 = Click.to.write.the.question.text.I.think.the.author.of.the.second.essay.was.knowledgeable,
                            antius2 = Click.to.write.the.question.text.I.agree.with.the.arguments.of.the.second.essay,          
                            antius1 = Click.to.write.the.question.text.I.think.the.arguments.in.the.second.essay.were.valid
                            )
# no condition variable exists, so creating one based on whether they responded to the tv or ms prompt
uwmadison_inhouse$ms_condition <- NA
uwmadison_inhouse$ms_condition[nchar(uwmadison_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you.) > 1] <- "ms"
uwmadison_inhouse$ms_condition[nchar(uwmadison_inhouse$X.Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you.) > 1] <- "tv"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# uwmadison_inhouse <- readRDS("./data/raw_site_data/UWmadison inhouse/uwmadison_inhouse.rds")

# read in kansas inhouse data ----
kansas_inhouse <- read.csv("./data/raw_site_data/kansas inhouse/in house condition, Kansas, TMT replication.csv", 
                           header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
kansas_inhouse$expert <- 0
# add site identifier
kansas_inhouse$source <- "kansas_inhouse"
# add location, usually identical to source
kansas_inhouse$location <- "kansas"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# kansas_inhouse <- readRDS("./data/raw_site_data/kansas inhouse/kansas_inhouse.rds")

# read in Pace expert data ----
pace_expert <- read.csv("./data/raw_site_data/pace expert/mancini_gosnell_Pace_expert_TMT_replication_data_FINAL_2017.csv", 
                        header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
pace_expert$expert <- 1
# add site identifier
pace_expert$source <- "pace_expert"
# add location, usually identical to source
pace_expert$location <- "pace"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# pace_expert <- readRDS("./data/raw_site_data/pace expert/pace_expert.rds")

# read in wesleyan inhouse data ----
# double-check politicalID coding
wesleyan_inhouse <- read.csv("./data/raw_site_data/wesleyan inhouse/schmidtmsrep.csv", 
                             header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
wesleyan_inhouse$expert <- 0
# add site identifier
wesleyan_inhouse$source <- "wesleyan_inhouse"
# add location, usually identical to source
wesleyan_inhouse$location <- "wesleyan"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# wesleyan_inhouse <- readRDS("./data/raw_site_data/wesleyan inhouse/wesleyan_inhouse.rds")

# read in sou inhouse data ----
sou_inhouse <- read.csv("./data/raw_site_data/sou inhouse/data to send.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
sou_inhouse$expert <- 0
# add site identifier
sou_inhouse$source <- "sou_inhouse"
sou_inhouse$location <- "sou"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# sou_inhouse <- readRDS("./data/raw_site_data/sou inhouse/sou_inhouse.rds")

# read in Kansas expert data ----
kansas_expert <- read.csv("./data/raw_site_data/kansas expert/Swanson_KUdata.csv", 
                          header = TRUE, stringsAsFactor = FALSE)
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

# read in PLU inhouse data ----
plu <- read.csv("./data/raw_site_data/plu/Pacific Lutheran University Many Labs 4 Clean Data.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the inhouse condition
plu$expert <- 0
# add site identifier
plu$source <- "plu"
plu$location <- "plu"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# plu <- readRDS("./data/raw_site_data/plu/plu.rds")

# read in ashland expert data ----
ashland <- read.csv("./data/raw_site_data/ashland/ML 4 AU Data Chartier & Brady.csv", 
                    header = TRUE, stringsAsFactor = FALSE)
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

# read in vcu expert data ----
vcu <- read.csv("./data/raw_site_data/vcu/manylabsVCU.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
vcu$expert <- 1
# add site identifier
vcu$source <- "vcu"
vcu$location <- "vcu"
# Note: One R user was having issues with some .csv files.
# If that happens, you can uncomment the below line to simply read in the pre-processed .rds file.
# vcu <- readRDS("./data/raw_site_data/vcu/vcu.rds")

# read in BYU in-house data ----
byui <- read.csv("./data/raw_site_data/byui_expert/ML4 BYUI Wiggins.csv", header = TRUE, stringsAsFactor = FALSE)
# add column indicating it's from the expert condition
byui$expert <- 1
# add site identifier
byui$source <- "byui"
# add location, usually identical to source
byui$location <- "byui"

# read in Pace in-house data ----
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
pace_inhouse <- rename(pace_inhouse,
                       prous4 = How.much.do.you.like.Author.P.,                      
                       prous3 = How.intelligent.is.Author.P.,                        
                       prous5 = How.knowledgeable.about.America.is.Author.P.,        
                       prous2 = How.much.do.you.agree.with.Author.P.s.essay.,        
                       prous1 = How.valid..true.or.logical..are.Author.P.s.arguments.,
                       antius4 = How.much.do.you.like.Author.A.,                      
                       antius3 = How.intelligent.is.Author.A.,                        
                       antius5 = How.knowledgeable.about.America.is.Author.A.,        
                       antius2 = How.much.do.you.agree.with.Author.A.s.essay.,        
                       antius1 = How.valid..true.or.logical..are.Author.A.s.arguments.,
                       gender = What.is.your.gender.,                                
                       age = What.is.your.age.                                   
                       )
# no condition variable exists, so creating one based on whether they responded to the tv or ms prompt
pace_inhouse$ms_condition <- NA
pace_inhouse$ms_condition[nchar(pace_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you.) > 1] <- "ms"
pace_inhouse$ms_condition[nchar(pace_inhouse$Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you.) > 1] <- "tv"
# For users having trouble with .csv: can uncomment below line and read processed .rds
# pace_inhouse <- readRDS("./data/raw_site_data/pace_inhouse/TMT.rds")


# Merge individual datasets ----

# make list object. Each data frame is an entry in the list
allsitedata <- list(riverside, 
                    uwmadison_expert, 
                    occid, 
                    cnj, 
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
                    pace_inhouse)

# convert all dataframe columns to character for easier merging
for (i in 1:length(allsitedata)) {
  allsitedata[[i]] <- data.frame(lapply(allsitedata[[i]], as.character), stringsAsFactors=FALSE)
}

# merge data frames vertically
merged <- bind_rows(template, allsitedata)

# convert columns back to numeric/factor where needed
numeric.columns <- c("prous1", "prous2", "prous3", "prous4", "prous5",
                     "antius1", "antius2", "antius3", "antius4", "antius5",
                     "expert", "americanid")
merged <- mutate_at(merged, .vars = vars(all_of(numeric.columns)), .funs = as.numeric)

# convert in-house open text for race to numeric code
source("./sources/race_text_to_num.R") # load custom function
merged$race <- race_text_to_num(merged$race)

# some of these were treated as numeric, I believe they are factor
# TODO: check number of factor levels
# TODO: handle text-response data for race
factor.columns <- c("ms_condition", "msincomplete", 
                    "countryofbirth","ethnicity", "race")
merged <- mutate_at(merged, .vars = vars(all_of(factor.columns)), .funs = as.factor)

# We appear to have some entirely NA rows, except meta data. To address this I'm
# going to remove responses that are blank in all of these variables:
# 1) did not respond to any of the author rating items and 2) also did not have
# an experimental condition assignment. 
# I think this is a conservative approach that does not lose any real data, at trade-off
# of retaining some rows with little/no information.
merged <- subset(merged, (!is.na(merged$prous3) | !is.na(merged$prous4) | !is.na(merged$prous5) | 
                          !is.na(merged$antius3) | !is.na(merged$antius4) | !is.na(merged$antius5) | 
                          !is.na(merged$ms_condition)))

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

##
# We're going to apply some exclusions in later steps due to re-examining the
# pre-reg. I'm saving the full dataset here for reference.
##

# save .csv
write.csv(merged, "./data/processed_data/merged_full.csv",row.names=FALSE)
# save .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged, "./data/processed_data/merged_full.rds")

# Create deidentified dataset by dropping the following variables
merged_deidentified_full <- merged

# dropping open text responses used at some sites, possibly identifying
merged_deidentified_full$MS1 <- NULL 
merged_deidentified_full$MS2 <- NULL
merged_deidentified_full$control1 <- NULL
merged_deidentified_full$control2 <- NULL
merged_deidentified_full$race_6_TEXT <- NULL
merged_deidentified_full$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you. <- NULL
merged_deidentified_full$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and... <- NULL
merged_deidentified_full$Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you. <- NULL
merged_deidentified_full$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and... <- NULL
merged_deidentified_full$The.one.thing.I.fear.the.most.about.my.death.is. <- NULL
merged_deidentified_full$My.scariest.thoughts.about.my.death.are. <- NULL
merged_deidentified_full$race.elaborate <- NULL
merged_deidentified_full$politicalviews_other <- NULL
merged_deidentified_full$Did.anything.in.the.survey.strike.you.as.odd.or.unusual. <- NULL
merged_deidentified_full$Sometimes.in.psychology.studies..participants.believe.there.is.more.going.on.than.meets.the.eye.... <- NULL
merged_deidentified_full$controlresponse2 <- NULL
merged_deidentified_full$mortalityresponse1 <- NULL
merged_deidentified_full$mortalityresponse2 <- NULL
merged_deidentified_full$controlresponse1 <- NULL
merged_deidentified_full$Television1 <- NULL
merged_deidentified_full$Television2 <- NULL
merged_deidentified_full$SubtleOwnDeath1 <- NULL
merged_deidentified_full$SubtleOwnDeath2 <- NULL
merged_deidentified_full$major <- NULL
merged_deidentified_full$The.one.thing.I.fear.most.about.my.death.is. <- NULL
merged_deidentified_full$My.scariest.thoughts.about.death.are. <- NULL
merged_deidentified_full$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television.and.o... <- NULL
merged_deidentified_full$The.one.thing.I.fear.most.about.television.is. <- NULL
merged_deidentified_full$My.scariest.thoughts.about.television.are. <- NULL
merged_deidentified_full$Language <- NULL
merged_deidentified_full$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and.once.you.are.physically.dead. <- NULL
merged_deidentified_full$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and.once.you.have.physically.watched.television. <- NULL

# dropping Qualtrics-recorded location data, and similar
merged_deidentified_full$LocationLatitude <- NULL
merged_deidentified_full$LocationLongitude <- NULL
merged_deidentified_full$LocationAccuracy <- NULL
merged_deidentified_full$IP.Address <- NULL

# dropping what may be ID numbers of some kind
merged_deidentified_full$WBL_ID <- NULL 
merged_deidentified_full$Respondent.ID <- NULL
merged_deidentified_full$Collector.ID <- NULL

# dropping potentially triangulating data
merged_deidentified_full$age <- NULL
merged_deidentified_full$time <- NULL
merged_deidentified_full$gender <- NULL
merged_deidentified_full$ethnicity <- NULL
merged_deidentified_full$ethnicity..1...White.Caucasian..2...Middle.Eastern..3...Asian.Pacific.Islander..4...African.American.Black..5...Hispanic.Latino..6...Indigenous.Aboriginal..7...Would.Rather.Not.Say..8...Other <- NULL
merged_deidentified_full$politicalid <- NULL
merged_deidentified_full$politicalid.wpi <- NULL
merged_deidentified_full$politicalview..1.Republican..2...Democrat..3...Independent..4...Other..5...No.Preference. <- NULL
merged_deidentified_full$politicalparty..1...Republican..2...Democrat..3...Libertarian..4...Green..5...Constitution..6...Independent..7...I.don.t.identify.with.a.political.party..8...Other. <- NULL
merged_deidentified_full$countryofbirth..187...US. <- NULL
merged_deidentified_full$birthcountry <- NULL
merged_deidentified_full$raceombmulti <- NULL
merged_deidentified_full$In.what.country.were.you.born. <- NULL
merged_deidentified_full$race.azusa <- NULL
merged_deidentified_full$race.wpi <- NULL
merged_deidentified_full$raceomb <- NULL

# for good measure, let's drop all unique questions asked by sites (hard to police exactly what was asked at each site)
merged_deidentified_full <- select(merged_deidentified_full, 
                                   participantnum:location, 
                                   proauth_avg:pro_minus_anti)

# save deidentified .csv
write.csv(merged_deidentified_full, "./data/public/merged_deidentified_full.csv",row.names=FALSE)
# save deidentified .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged_deidentified_full, "./data/public/merged_deidentified_full.rds")

##################
## Correction: applying sample exclusions based on pre-reg: https://osf.io/4xx6w
## Exclude samples with N < 60 
## Exclude data collected before the pre-reg was registered (February 15, 2017).
## This applied to some In House sites that had their own deadlines.
##################

# read in the generated full merged dataset
merged <- readRDS("./data/processed_data/merged_full.rds")

### Under 60: 
# definitely: ashland, azusa, kansas_expert, sou_inhouse
# maybe: pace_inhouse hovers right around 60. Glancing over the 
#   raw data, there appear to be over 60 respondants, but that drops below 60 
#   after applying the lowest exclusion criteria. For now, I've left them included.

merged_over60 <- merged %>% filter(
  source == "byui"|
  source == "cnj"|
  source == "illinois"|
  source == "ithaca"|
  source == "kansas_inhouse"|
  source == "occid"|
  source == "pace_expert"|
  source == "plu"|
  source == "riverside"|
  source == "ufl"|
  source == "upenn"|
  source == "uwmadison_expert"|
  source == "uwmadison_inhouse"|
  source == "vcu"|
  source == "wesleyan_inhouse"|
  source == "wpi"|
  source == "pace_inhouse")

# Now, drop rows of data from before Feb 15, the date of the pre-reg, 
# as specified in the pre-reg.
# occurs in: azusa, ithaca, plu, sou_inhouse, ufl, wesleyan_inhouse
# azusa and sou_inhouse already filtered out due to < 60 N.

# Note: dates are not in a standard format across labs, so I'm heisitant
# to deal with them through POSIXlt

# ithaca
merged_subset <- merged_over60 %>% 
  filter(source != "ithaca" | (source == "ithaca" & as.numeric(participantnum) > 97 & !is.na(participantnum))) 
# 97 was last participant run at plu before feb 15, 2017

# plu
merged_subset <- merged_subset %>% 
  filter(source != "plu" | (source == "plu" & as.numeric(participantnum) > 187 & !is.na(participantnum))) 
# 187 was last participant run at plu before feb 15, 2017

# ufl
merged_subset <- merged_subset %>% 
  filter(source != "ufl" | (source == "ufl" & as.numeric(X._session_id) > 9453807 & !is.na(X._session_id))) 
# 9453807 was last session number before feb 15, 2017

# wesleyan_inhouse
merged_subset <- merged_subset %>% 
  filter(source != "wesleyan_inhouse" | (source == "wesleyan_inhouse" & as.numeric(participantnum) > 80 & !is.na(participantnum))) 
# 80 was last participant at wesleyan_inhouse before feb 15, 2017

#now need to deidentify

# save .csv
write.csv(merged_subset, "./data/processed_data/merged_subset.csv",row.names=FALSE)
# save .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged_subset, "./data/processed_data/merged_subset.rds")

# Create deidentified dataset by dropping the following variables
merged_deidentified_subset <- merged_subset

# dropping open text responses used at some sites, possibly identifying
merged_deidentified_subset$MS1 <- NULL 
merged_deidentified_subset$MS2 <- NULL
merged_deidentified_subset$control1 <- NULL
merged_deidentified_subset$control2 <- NULL
merged_deidentified_subset$race_6_TEXT <- NULL
merged_deidentified_subset$Please.briefly.describe.the.emotions.that.the.thought.of.your.own.death.arouses.in.you. <- NULL
merged_deidentified_subset$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and... <- NULL
merged_deidentified_subset$Please.briefly.describe.the.emotions.that.the.thought.of.watching.television.arouses.in.you. <- NULL
merged_deidentified_subset$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and... <- NULL
merged_deidentified_subset$The.one.thing.I.fear.the.most.about.my.death.is. <- NULL
merged_deidentified_subset$My.scariest.thoughts.about.my.death.are. <- NULL
merged_deidentified_subset$race.elaborate <- NULL
merged_deidentified_subset$politicalviews_other <- NULL
merged_deidentified_subset$Did.anything.in.the.survey.strike.you.as.odd.or.unusual. <- NULL
merged_deidentified_subset$Sometimes.in.psychology.studies..participants.believe.there.is.more.going.on.than.meets.the.eye.... <- NULL
merged_deidentified_subset$controlresponse2 <- NULL
merged_deidentified_subset$mortalityresponse1 <- NULL
merged_deidentified_subset$mortalityresponse2 <- NULL
merged_deidentified_subset$controlresponse1 <- NULL
merged_deidentified_subset$Television1 <- NULL
merged_deidentified_subset$Television2 <- NULL
merged_deidentified_subset$SubtleOwnDeath1 <- NULL
merged_deidentified_subset$SubtleOwnDeath2 <- NULL
merged_deidentified_subset$major <- NULL
merged_deidentified_subset$The.one.thing.I.fear.most.about.my.death.is. <- NULL
merged_deidentified_subset$My.scariest.thoughts.about.death.are. <- NULL
merged_deidentified_subset$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television.and.o... <- NULL
merged_deidentified_subset$The.one.thing.I.fear.most.about.television.is. <- NULL
merged_deidentified_subset$My.scariest.thoughts.about.television.are. <- NULL
merged_deidentified_subset$Language <- NULL
merged_deidentified_subset$Jot.down..as.specifically.as.you.can..what.you.think.will.happen.to.you.physically.as.you.die.and.once.you.are.physically.dead. <- NULL
merged_deidentified_subset$Jot.down..as.specifically.as.you.can..what.you.think.happens.to.you.as.you.watch.television..and.once.you.have.physically.watched.television. <- NULL

# dropping Qualtrics-recorded location data, and similar
merged_deidentified_subset$LocationLatitude <- NULL
merged_deidentified_subset$LocationLongitude <- NULL
merged_deidentified_subset$LocationAccuracy <- NULL
merged_deidentified_subset$IP.Address <- NULL

# dropping what may be ID numbers of some kind
merged_deidentified_subset$WBL_ID <- NULL 
merged_deidentified_subset$Respondent.ID <- NULL
merged_deidentified_subset$Collector.ID <- NULL

# dropping potentially triangulating data
merged_deidentified_subset$age <- NULL
merged_deidentified_subset$time <- NULL
merged_deidentified_subset$gender <- NULL
merged_deidentified_subset$ethnicity <- NULL
merged_deidentified_subset$ethnicity..1...White.Caucasian..2...Middle.Eastern..3...Asian.Pacific.Islander..4...African.American.Black..5...Hispanic.Latino..6...Indigenous.Aboriginal..7...Would.Rather.Not.Say..8...Other <- NULL
merged_deidentified_subset$politicalid <- NULL
merged_deidentified_subset$politicalid.wpi <- NULL
merged_deidentified_subset$politicalview..1.Republican..2...Democrat..3...Independent..4...Other..5...No.Preference. <- NULL
merged_deidentified_subset$politicalparty..1...Republican..2...Democrat..3...Libertarian..4...Green..5...Constitution..6...Independent..7...I.don.t.identify.with.a.political.party..8...Other. <- NULL
merged_deidentified_subset$countryofbirth..187...US. <- NULL
merged_deidentified_subset$birthcountry <- NULL
merged_deidentified_subset$raceombmulti <- NULL
merged_deidentified_subset$In.what.country.were.you.born. <- NULL
merged_deidentified_subset$race.azusa <- NULL
merged_deidentified_subset$race.wpi <- NULL
merged_deidentified_subset$raceomb <- NULL

# for good measure, let's drop all unique questions asked by sites (hard to police exactly what was asked at each site)
merged_deidentified_subset <- select(merged_deidentified_subset, participantnum:location, proauth_avg:pro_minus_anti)

# save deidentified .csv
write.csv(merged_deidentified_subset, "./data/public/merged_deidentified_subset.csv",row.names=FALSE)
# save deidentified .rds for r users, has some advantages so I recommend loading this file
saveRDS(merged_deidentified_subset, "./data/public/merged_deidentified_subset.rds")