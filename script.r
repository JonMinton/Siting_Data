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
        "xtable",
        "corrgram"
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

cor_spearman.all_years <- cor(
    subset(tidy_block, select=c("pm10", "pm2.5", "no2", "nox", "co" ,"so2", "ozone", "benzene")),
    use="pairwise.complete.obs",
    method="spearman"
)

print.xtable(
    xtable(
        cor_spearman.all_years,
        digits=3
    ),
    file="tables/cor_spearman_all_years.html", type="html"
)

# now a corplot

png("figures/corrgram_pearson_all_years.png", width=500, height=500)
corrgram(
    cor_pearson.all_years,
    upper.panel=panel.conf,
    main="Correlations between pollutants (Pearson)",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)

dev.off()


png("figures/corrgram_spearman_all_years.png", width=500, height=500)
corrgram(
    cor_spearman.all_years,
    upper.panel=panel.conf,
    main="Correlations between pollutants (Spearman)",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)

dev.off()


################################

# Options 
#  1) Separately for each year



#########################################################
# 2) differences

