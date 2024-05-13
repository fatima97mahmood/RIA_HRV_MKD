# SETUP -------------------------------------------------------------------

# Load libraries
library(here) #easy file referencing, root is set at project root
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(fixest)
library(huxtable)
library(flextable)

# Merge USITC itpd_e and dgd ---------------------------------------------

x = readRDS(here("input","data","usitc_itpd_e_r02.rds"))
y = readRDS(here("input","data","usitc_dgd.rds"))

x = left_join(x,y,by=c("year", 
                       "exporter_dynamic_code" = "dynamic_code_o",
                       "importer_dynamic_code" = "dynamic_code_d")) 

rm(y)

