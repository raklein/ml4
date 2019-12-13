# Package version control for reproducibility using the 'renv' package (https://github.com/rstudio/renv).
# See https://rstudio.github.io/renv/articles/renv.html for an overview

# Run this script if you're having issues with package versions.
# Basically, when I wrote the script I stored all package dependencies with
# the renv package, and the below command will restore exactly the packages and versions
# I used when I wrote it. 
# These are stored in a private library for this project,
# so it shouldn't disrupt your other projects or general R envinronment.

#install.packages("renv")
library(renv)
renv::restore()
