# This file is used from the command line.
# Alternatively, the README.Rmd can be "knit" manually. 
# The result should be the same if using the container.
# From command line (within Rstudio, if necessary) run
# R CMD BATCH 00_main.R 

rmarkdown::render("README.Rmd",quiet=FALSE)

