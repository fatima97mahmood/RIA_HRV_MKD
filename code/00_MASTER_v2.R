
# SETUP -------------------------------------------------------------------

# Install (if necessary) libraries
# library(renv) # install.packages("renv") if necessary
# renv::restore(confirm = FALSE)

# Load libraries
library(here) #easy file referencing, root is set at project root

# Setup - Define global variables -------------------------------------------------------------------

source(here("code","01_setup.R"))

# Download data  -----------------------------------------------------------

# This step only needs to be ran once! The results are stored in the input/data folder
# source(here("code","02_data_download_v2.R"))

# Prepare data ------------------------------------------------------------

source(here("code","03_dataprep_v2.R"))


# Descriptive analysis of data ---------------------------------------------

source(here("code","04_Descriptive.R"))

# Model -------------------------------------------------------------------

source(here("code","05_Methodology.R"))

