# script for managing siting data



rm(list=ls())


####

source("scripts/LoadPackages.R")

RequiredPackages(
    c(
        "ggplot2",
        "r2stl",
        "reshape2",
        "plyr",
        "xtable",
        "corrgram",
        "maptools",
        "rgdal",
        "rgeos",
        "fields",
        "gstat", 
        "akima"
        )
    )

#################################
# source("scripts/tidy_data_maker.r")

# source("scripts/make_pollution_wrap_figures.r")


############################################################################################################
tidy_block <- read.csv("Data/raw/defra_pollution_estimates_combined.csv")
#save(tidy_block, file="Data/robj/tidy_block.rdata")


