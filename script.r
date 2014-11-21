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

##############################################################################################
# 21/11/2014

# Loading Gavin's Three Step approach to estimating areal pollution

# Link to the file: 
# https://www.dropbox.com/s/3whmj4h5v6m853x/DATA_LINKAGE.RData?dl=0
datazones_shp <-  readShapePoly("G:/dropbox/Dropbox/Data/Shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")
datazones_ids <- sapply(slot(datazones_shp, "polygons"), function(x) slot(x, "ID"))# just check 
datazones_id_link <- data.frame(
    zone_id=datazones_ids,
    zone_name=datazones_shp[["zonecode"]]
    )


load("G:/dropbox/Dropbox/Data/Pollution/gavins_three_step_approach/DATA_LINKAGE.RData")

fn <- function(x){
    x <- data.frame(
        datazone=datazones_id_link$zone_name,
        value=x
        )
    return(x)
}
out_dataframe <- ldply(out_list, fn)



out2 <- recast(
    out_dataframe,
    year + datazone ~ pollutant,
    id.var=c("year", "datazone", "pollutant"),
    measure.var="value",
    fun=mean
               )

out2[,-c(1,2)] <- apply(
    out2[,-c(1,2)], 2, 
    function(x){
        out <- x
        out[is.nan(out)] <- NA
        out
    }
)

pollution_tidy <- out2
rm(out2)

write.csv(pollution_tidy, file="Data/generated/pollution_by_datazone.csv")

################################################################################
### Now to visualise
#####################################################################################################

require(reshape2)
require(plyr)
require(stringr)
require(ggplot2)
require(maptools)
require(grid)


datazones_shp@data$id <- rownames(datazones_shp@data)
id_name <- subset(datazones_shp@data, select=c("id", "zonecode"))

datazones_map <- fortify(datazones_shp)
datazones_map <- join(datazones_map, id_name)
datazones_map <- rename(datazones_map, replace=c("zonecode"="datazone"))


theme_clean <- function(base_size=12){
    theme_grey(base_size) %+replace%
        theme(
            axis.title=element_blank(),
            axis.text=element_blank(),
            panel.background=element_blank(),
            panel.grid=element_blank(),
            axis.ticks.length=unit(0, "cm"),
            axis.ticks.margin=unit(0, "cm"),
            panel.margin=unit(0, "lines"),
            plot.margin=unit(c(0,0,0,0), "lines"),
            complete=TRUE
        )
}

pollutants_joined <- join(datazones_map, pollution_tidy, by="datazone", type="full")
pollutants_joined <- arrange(pollutants_joined, year, group, order)


g1 <- ggplot(
    subset(
        pollutants_joined,
        year==2001
        )
)
g2 <- g1 + geom_polygon(aes(x=long, y=lat, fill=no2, group=id)) + coord_equal()
g3 <- g2 + theme_clean() + scale_fill_gradient(low="white", high="red")      
print(g3)


