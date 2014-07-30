# script for managing siting data


#### To dos:

# Correlation tables and corrplots of pollutants
# First differences 
# Principle components 

rm(list=ls())


####

source("scripts/LoadPackages.R")

RequiredPackages(
    c(
        "ggplot2",
        "r2stl",
        "reshape2",
        "plyr",
        "xtable"
        )
    )

#################################
# source("scripts/tidy_data_maker.r")

# source("scripts/make_pollution_wrap_figures.r")


############################################################################################################

load("data/robj/tidy_block.rdata")

# Correlations? 

# correlations overall (all years): 

cor_pearson.all_years <- cor(
    subset(tidy_block, select=c("pm10", "pm2.5", "no2", "nox", "co" ,"so2", "ozone", "benzene")),
    use="pairwise.complete.obs"
    )


print.xtable(
    xtable(
        cor_pearson.all_years,
        digits=3
        ),
    file="tables/cor_pearson_all_years.html", type="html"
    )

print.xtable(
    xtable(
        cor_spearman.all_years,
        digits=3
    ),
    file="tables/cor_spearman_all_years.html", type="html"
)

# now a corplot


# Correlation tables and corrplots of pollutants


