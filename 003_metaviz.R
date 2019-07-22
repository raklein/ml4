# Trying some new forest plot visualizations

library('metaviz')
library('tidyverse')

# Read per-site results according to the three exclusions criteria levels
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# Tidy up the sitesource variable into a descriptive label
combinedresults1$sitesource_label[combinedresults1$sitesource == "ufl"] <- "University of Florida"
combinedresults1$sitesource_label[combinedresults1$sitesource == "occid"] <- "Occidental College"
combinedresults1$sitesource_label[combinedresults1$sitesource == "ashland"] <- "Ashland University"
combinedresults1$sitesource_label[combinedresults1$sitesource == "ithaca"] <- "Ithaca College"
combinedresults1$sitesource_label[combinedresults1$sitesource == "riverside"] <- "UC Riverside"
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
combinedresults2$sitesource_label[combinedresults2$sitesource == "riverside"] <- "UC Riverside"
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
combinedresults3$sitesource_label[combinedresults3$sitesource == "riverside"] <- "UC Riverside"
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

# metaviz requests sampling errors, not variances
combinedresults1$sqrt_vi <- sqrt(combinedresults1$vi)
combinedresults2$sqrt_vi <- sqrt(combinedresults2$vi)
combinedresults3$sqrt_vi <- sqrt(combinedresults2$vi)

# sort all from large to small effect size within lab condition for aesthetics
combinedresults1 <- arrange(combinedresults1, expert, yi)
combinedresults2 <- arrange(combinedresults2, expert, yi)
combinedresults3 <- arrange(combinedresults3, expert, yi)

# I'll use a temporary 'dat' object to avoid typos in making identical plots
# from different datasets

# Change this to 'combinedresults2' or 'combinedresults3' to generate plots
# for the other two exclusion sets
dat <- combinedresults1

# # Rain plot - tried it, prefer variant = 'thick'
# rain <- dat[, c("yi", "sqrt_vi")] %>% 
#   viz_forest(study_labels = dat[, "sitesource_label"], 
#              summary_label = c("Summary (In House labs)", "Summary (Author Advised labs)"),
#              text_size = 4,
#              group = dat[, "expert"],
#              col = c("Blues"), # can't have multiple colors for 'rain'
#              summary_col = c("Blues"), # can't have multiple colors for 'rain'
#              xlab = "Hedges' g", 
#              method = "DL", # random effects model
# #             annotate_CI = TRUE,
#              variant = "rain"
#   )
# 
# rain + theme(panel.border = element_blank())

# Thick plot
thick <- dat[, c("yi", "sqrt_vi")] %>% 
  viz_forest(
    study_labels = dat[, "sitesource_label"],
    summary_label = c("Summary (In House labs)", "Summary (Author Advised labs)"),
    text_size = 3,
    tick_col = "#000000",
    group = dat[, "expert"],
    col = c("#4475b4", "#d73027")[dat$expert + 1], # +1 because the first element in a column is 1 (not 0)
    summary_col = c("#4475b4", "#d73027"),
    xlab = "Hedges' g",
    method = "DL", # random effects model
    variant = "thick",
  )

# old colors ("#0072B2", "#D55E00")

# viz_forest conveniently outputs a ggplot object, so I'll add some additional
# customzation using ggplot2 arguments

thick <- thick + 
  theme(panel.border = element_blank()) + 
  theme(axis.text.y = element_text(face = c("plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold"), color = c("#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000"))) +
  theme(axis.text.x = element_text(face = c("bold"), color = "#000000")) +
  theme(axis.title.x = element_text(hjust = 0.407, face = "plain")) +
  theme(axis.ticks = element_blank())

# thick

# I'm having trouble editing the geom objects given I'm not generating
# the gg code from scratch. Using the gginnards package to change the order
# of layers so the vertical dashed X axis vline is moved behind all other layers
library("gginnards")
summary(thick)
str(thick$layers[[5]])
thick <- move_layers(thick, "GeomVline", position = "bottom")

# Another goal is to make the CIs prettier. I think the below is the layer,
# but I'm stuck on how to improve.
str(thick$layers[[2]])

# Now messing with fonts. This step may not share well, but it's pretty
# unnecessary so feel free to comment out if you're reusing this code.
library("extrafont")

# font_import() # do this only once, can take ~5 minutes
# loadfonts() # do this only if you used font_import() in this session

# show available fonts
fonts()

thick <- thick +
  theme(text=element_text(family="Arial"))

# thick

# Save the plot, note filename if you're doing a different exclusion set
ggsave(filename = "./output/ml4_mainfigure_excl1.png", plot = thick, dpi=300, height=5, width=5, units="in", type = "cairo")

# Repeat for exclusion sets 2 and 3 by changing dat <- combinedresults2 and dat <- combinedresults3
