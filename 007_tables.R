# This script generates tables used in the manuscript
# TODO: Consider adding CIs or SE of hedge's g; consider sorting by IH / AA too

library(tidyverse)

# import summary results files
combinedresults1 <- read_csv("./data/public/combinedresults1.csv")
combinedresults2 <- read_csv("./data/public/combinedresults2.csv")
combinedresults3 <- read_csv("./data/public/combinedresults3.csv")

### Summary stats table for exclusion set 1
# generate tidied up tables
table_summary1 <- combinedresults1 %>% 
  # Change author-advised numeric to a label
  mutate(expert = ifelse(expert == 1, "AA", "IH")) %>% 
  # select required columns
  select("Location" = sitesource_label, 
         "Author Advised (AA) or In House (IH)" = expert, 
         "N (TV)" = n_tv, "N (MS)" = n_ms, 
         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
         "Hedges' g" = yi, "p" = p.value) %>% 
  # Round numeric columns to 2 digits
  mutate_if(.predicate = is.numeric, .funs = round, digits = 2) %>% 
  # Sort alphabetically
  arrange(`Location`)

### Summary stats table for exclusion set 2
# generate tidied up tables
table_summary2 <- combinedresults2 %>% 
  # Change author-advised numeric to a label
  mutate(expert = ifelse(expert == 1, "AA", "IH")) %>% 
  # select required columns
  select("Location" = sitesource_label, 
         "Author Advised (AA) or In House (IH)" = expert, 
         "N (TV)" = n_tv, "N (MS)" = n_ms, 
         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
         "Hedges' g" = yi, "p" = p.value) %>% 
  # Round numeric columns to 2 digits
  mutate_if(.predicate = is.numeric, .funs = round, digits = 2) %>% 
  # Sort alphabetically
  arrange(`Location`)

### Summary stats table for exclusion set 3
# generate tidied up tables
table_summary3 <- combinedresults3 %>% 
  # Change author-advised numeric to a label
  mutate(expert = ifelse(expert == 1, "AA", "IH")) %>% 
  # select required columns
  select("Location" = sitesource_label, 
         "Author Advised (AA) or In House (IH)" = expert, 
         "N (TV)" = n_tv, "N (MS)" = n_ms, 
         "Mean (TV)" = mean_tv, "Mean (MS)" = mean_ms, 
         "SD (TV)" = sd_tv, "SD (MS)" = sd_ms,
         "Hedges' g" = yi, "p" = p.value) %>% 
  # Round numeric columns to 2 digits
  mutate_if(.predicate = is.numeric, .funs = round, digits = 2) %>% 
  # Sort alphabetically
  arrange(`Location`)

write.csv(table_summary1, "./output/table_summary1.csv", row.names = FALSE)
write.csv(table_summary2, "./output/table_summary2.csv", row.names = FALSE)
write.csv(table_summary3, "./output/table_summary3.csv", row.names = FALSE)
