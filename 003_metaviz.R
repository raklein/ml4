# Creating the main forest plots (Figure 1) with meta-analytic results

library('metaviz')
library('tidyverse')
library("gginnards")
library("extrafont")
# font_import() # do this only once, can take ~5 minutes
# loadfonts() # do this only if you used font_import() in this session

# Read per-site results according to the three exclusions criteria levels
# These files are generated in 002_ml4analysis.R
combinedresults1 <- read.csv("./data/public/combinedresults1.csv")
combinedresults2 <- read.csv("./data/public/combinedresults2.csv")
combinedresults3 <- read.csv("./data/public/combinedresults3.csv")

# metaviz requests sampling errors, not variances
combinedresults1$sqrt_vi <- sqrt(combinedresults1$vi)
combinedresults2$sqrt_vi <- sqrt(combinedresults2$vi)
combinedresults3$sqrt_vi <- sqrt(combinedresults3$vi)

# sort all from large to small effect size within lab condition for aesthetics
combinedresults1 <- arrange(combinedresults1, expert, yi)
combinedresults2 <- arrange(combinedresults2, expert, yi)
combinedresults3 <- arrange(combinedresults3, expert, yi)

# Function for making & customizing thick plots
make_thick_plot <- function(data) {

  # Thick plot
  thick <- data[, c("yi", "sqrt_vi")] %>% 
    viz_forest(
      study_labels = data[, "sitesource_label"],
      summary_label = c("Summary (In House labs)", "Summary (Author Advised labs)"),
      text_size = 3,
      tick_col = "#000000",
      group = data[, "expert"],
      col = c("#4475b4", "#d73027")[data$expert + 1], # +1 because the first element in a column is 1 (not 0)
      summary_col = c("#4475b4", "#d73027"),
      xlab = "Hedges' g",
      method = "DL", # random effects model
      variant = "thick",
    )
  # Consider: confidence_level = 0.90
  # old colors ("#0072B2", "#D55E00")
  
  # viz_forest conveniently outputs a ggplot object, so I'll add some additional
  # customzation using ggplot2 arguments
  #this customization needs adjusted based on number of labs (e.g., whether you exclude < 60 or not)
  thick <- thick + 
    theme(panel.border = element_blank()) + 
    theme(axis.text.y = element_text(face = c("plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold"), color = c("#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000"))) +
    theme(axis.text.x = element_text(face = c("bold"), color = "#000000")) +
    theme(axis.title.x = element_text(hjust = 0.407, face = "plain")) +
    theme(axis.ticks = element_blank())
  
  # If using full dataset, replace the above theme(axis.text.y...) line to reflect the additional samples.
  # theme(axis.text.y = element_text(face = c("plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold"), color = c("#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#555555", "#000000"))) +
  
  # I'm having trouble editing the geom objects given I'm not generating
  # the gg code from scratch. Using the gginnards package to change the order
  # of layers so the vertical dashed X axis vline is moved behind all other layers
  
  # summary(thick)
  # str(thick$layers[[5]])
  thick <- move_layers(thick, "GeomVline", position = "bottom")
  
  # Another goal is to make the CIs prettier. I think the below is the layer,
  # but I'm stuck on how to improve.
  # str(thick$layers[[2]])
  
  # Now messing with fonts. This step may not share well, but it's pretty
  # unnecessary so feel free to comment out if you're reusing this code.
  
  thick <- thick +
    theme(text=element_text(family="Arial"))
  
  return(thick)
}

# Create thick plots per exclusion rule
thick1 <- make_thick_plot(combinedresults1)
thick2 <- make_thick_plot(combinedresults2)
thick3 <- make_thick_plot(combinedresults3)

# Export thick plots to .png
ggsave(filename = "./output/ml4_mainfigure_excl1.png", plot = thick1, 
       dpi=300, height=5, width=5, units="in", type = "cairo")
ggsave(filename = "./output/ml4_mainfigure_excl2.png", plot = thick2, 
       dpi=300, height=5, width=5, units="in", type = "cairo")
ggsave(filename = "./output/ml4_mainfigure_excl3.png", plot = thick3, 
       dpi=300, height=5, width=5, units="in", type = "cairo")