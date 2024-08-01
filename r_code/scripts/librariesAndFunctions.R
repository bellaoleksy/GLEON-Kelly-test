pacman::p_load("ggcorrplot",
               "ggthemes",
               "rstatix",
               "readxl",
               "patchwork",
               "tidyverse",
               "RSQLite",
               "yaml",
               "scales",
               "ggpubr",
               "ggrepel",
               "stringr",
               "lubridate",
               "googlesheets4",
               "huxtable",
               "viridis",
               "hrbrthemes",
               "colorspace",
               "renv",
               "broom",
               "glue",
               "ggridges",
               "skimr",
               "deSolve",
               "Metrics",
               "imputeTS",
               "emmeans",
               "ggpp",
               "colorblindr",
               "officer",
               "flextable")


# library(rnaturalearth)
# library(rnaturalearthdata)
# library(rgeos)




select <- dplyr::select
#make it so that MASS::select() doesnt constantly clash with dplyr::select
