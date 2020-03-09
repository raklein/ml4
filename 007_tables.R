# This script generates tables used in the manuscript

library(tidyverse)

# import summary results files
combinedresults1 <- read_csv("./data/public/combinedresults1.csv")
combinedresults2 <- read_csv("./data/public/combinedresults2.csv")
combinedresults3 <- read_csv("./data/public/combinedresults3.csv")

### Summary stats table for exclusion set 1
# generate tidied up tables
tmp <- combinedresults1 # temporarily assigning a dataframe

# select required columns
table_summary1 <- select(tmp, 
                         "Location" = sitesource_label, 
                         "Author Advised (AA) or In House (IH)" = expert, 
                         "N (TV)" = n_tv, "N (MS)" = n_ms, 
                         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
                         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
                         "Hedges' g" = yi, "p" = p.value)

# Round numeric columns to 2 digits
table_summary1[,3:10] <- sapply(table_summary1[,3:10], round, digits = 2)

# Change numeric to a label
table_summary1[2][table_summary1[2] == 1] <- "AA"
table_summary1[2][table_summary1[2] == 0] <- "IH"

# Sort alphabetically
table_summary1 <- arrange(table_summary1, `Location`)

tmp <- NULL # delete temp df to avoid mistakes

### Summary stats table for exclusion set 2
# generate tidied up tables
tmp <- combinedresults2 # temporarily assigning a dataframe

# select required columns
table_summary2 <- select(tmp, 
                         "Location" = sitesource_label, 
                         "Author Advised (AA) or In House (IH)" = expert, 
                         "N (TV)" = n_tv, "N (MS)" = n_ms, 
                         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
                         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
                         "Hedges' g" = yi, "p" = p.value)

# Round numeric columns to 2 digits
table_summary2[,3:10] <- sapply(table_summary2[,3:10], round, digits = 2)

# Change numeric to a label
table_summary2[2][table_summary2[2] == 1] <- "AA"
table_summary2[2][table_summary2[2] == 0] <- "IH"

# Sort alphabetically
table_summary2 <- arrange(table_summary2, `Location`)

tmp <- NULL # delete temp df to avoid mistakes

### Summary stats table for exclusion set 3
# generate tidied up tables
tmp <- combinedresults3 # temporarily assigning a dataframe

# select required columns
table_summary3 <- select(tmp, 
                         "Location" = sitesource_label, 
                         "Author Advised (AA) or In House (IH)" = expert, 
                         "N (TV)" = n_tv, "N (MS)" = n_ms, 
                         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
                         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
                         "Hedges' g" = yi, "p" = p.value)

# Round numeric columns to 2 digits
table_summary3[,3:10] <- sapply(table_summary3[,3:10], round, digits = 2)

# Change numeric to a label
table_summary3[2][table_summary3[2] == 1] <- "AA"
table_summary3[2][table_summary3[2] == 0] <- "IH"

# Sort alphabetically
table_summary3 <- arrange(table_summary3, `Location`)

tmp <- NULL # delete temp df to avoid mistakes

write.csv(table_summary1, "./output/table_summary1.csv", row.names = FALSE)
write.csv(table_summary2, "./output/table_summary2.csv", row.names = FALSE)
write.csv(table_summary3, "./output/table_summary3.csv", row.names = FALSE)
