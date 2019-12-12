# Creating the main forest plots (Figure 1) with meta-analytic results

library('metaviz')
library('tidyverse')

# Read per-site results according to the three exclusions criteria levels
# These files are generated in 002_ml4analysis.R
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

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
