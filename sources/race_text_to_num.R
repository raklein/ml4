# Function for converting 

# table(merged$race)

race_text_to_num <- function(x) {
  # keep dictionaries in lowercase b/c I'm using tolower()
  dict.white <- c("caucasian", "white", "white/caucasian", "white/caucasion",
                  "White/Caucasian/Welsh,German,Italian",
                  "Irish American")
  dict.afram <- c("african american")
  dict.asian <- c("asian-indian", "asian", "asian american", "chinese")
  
  case_when(tolower(x) %in% dict.white ~ "1",
            tolower(x) %in% dict.afram ~ "2",
            tolower(x) %in% dict.asian ~ "4",
            T                          ~ x)
}

# demo <- c(1, 2, 4, 1, 1, "white", "White", "caucasian", "white/caucasian", 
#           "African American", "Asian", "Chinese")
# race_text_to_num(demo)

# TODO: consider function for handling biracial responses e.g. "1,4" or "1 AND 3"
