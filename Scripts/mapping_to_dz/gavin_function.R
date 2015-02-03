rm(list=ls())
gc()

require(maptools)
require(rgeos)
require(rgdal)
require(geoR)
require(plyr)
require(reshape2)
require(ggplot2)

datazones_shp <- readShapePoly("data/shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")
# assign the correct projection
proj4string(datazones_shp) <- CRS("+init=epsg:27700")
datazones_ids <- sapply(slot(datazones_shp, "polygons"), function(x) slot(x, "ID"))# just check
# Centroids of DZ
datazones_centroids <- gCentroid(datazones_shp,byid=TRUE)
datazones_centroids <- SpatialPointsDataFrame(datazones_centroids,datazones_shp@data)
### Read in the pollution data
all_pollution_data <- read.table("data/raw/defra_pollution_estimates_combined.csv",sep=",",header=TRUE)
all_pollution_data <- subset(all_pollution_data, subset = x >=50000 & x <= 500000 & y >= 500000 & y <= 1300000)
# melt into a format that plyr can split more easily
all_pollution_data_long <- melt(
    all_pollution_data, # sample for now due to memory limit
    id.vars=c("X", "ukgridcode" ,"x", "y", "year"),
    variable.name="pollutant"
)
na_values <- is.na(all_pollution_data_long$value)
all_pollution_data_long <- all_pollution_data_long[!na_values,]
rm(all_pollution_data)
gc()
str(datazones_centroids)

# # load workplace containing the above data
# setwd("C:/Users/Guanpeng/Dropbox/data linkage/")
# load("DATA_LINKAGE")



#######################################################################################
gavin_linkage_fn <- function(
    pollution_data,
    dz_centroids,
    nearest_P
    #range_param=100,
    #num_samples=2000,
    #corr.para
){
    #@YearlyPollutionData is the Grid Pollution data year by year; A dataframe with coordinates
    #@pollution_name is the pollutants we want to calculate for Scottish Data zones
    ### Note before using this function, some data preparation is needed.
    #@ 1: the Scottish Data Zone; The names is DZ_centroid, otherwise we need change the names within the function
    #@ 2: yearly pollution data; the names is PD.data, otherwise we need change the names within the function
    #@ 3: make sure there is no missing data for each pollutant in the Pollutant grid point data
    # Finally, we can directly merge the calculated pollution data to the Scottish Data Zone
    #num_pollutant_obs <- nrow(pollution_data)
    num_dzs <- dim(dz_centroids)[1] # the number of data zones in Scotland
    out <- rep(NA, length=num_dzs)
    
    # nearest_n -- proportion of 1km by 1km grid pollutant data, for example 0.1%. 0.001*100,000 = 100
    # nearest_P <- nearest_P
    ######## Step 3
    # The indices of srounding pollution data points
    #ind <- list()
    # weights through a Gaussian kernel as we did in Step 2
    #Gweight <- list()
    coords_temp <- as.matrix(pollution_data[,c("x", "y")]/1000)
    # the range parameter obtained in Step 2. We also assume it to be the
    # the threshold of distance beyond which there is no spatial correlations
    # although it is not entirely true as the spatial correlation is about exp{-1} = 0.37
    for (i in 1:num_dzs) {
        # First calculate the distances between each dz and all pollution data
        dist_temp <- spDistsN1(
            pts=coords_temp,
            pt=dz_centroids@coords[i,]/1000
        )
        # find the distance threshold for user defined nearest_P (the proportion of grid pollutant used for 
        # calculating values for datazones)
        
        threshold_temp <- as.numeric(quantile(dist_temp,probs=nearest_P))
        # Change distance to geographical weights using a Gaussian kernel
        ind_temp <- which(
            dist_temp <= threshold_temp,
            arr.ind=TRUE
        )
        # Gaussian kernel
        w_temp <- exp(-(dist_temp[ind_temp]/threshold_temp)^2)
        out[i] <- sum(as.numeric(pollution_data[ind_temp,"value"]*w_temp))/sum(w_temp)
        if(i %% 1000 == 0) print(i)
    }
    # This takes about 1.7 minutes
    # finish the calculation of the k-th pollutant
    return(out)
}

#debug(gavin_linkage_fn)
# input: dataframe
# output: list
# Now it changes to

out_list <- dlply(
    all_pollution_data_long, # the input
    .(year, pollutant), # what to split by
    gavin_linkage_fn, # the function to apply
    dz_centroids=datazones_centroids, # additional unspecified arguments to function
    nearest_P=0.001, # for 0.1% of pollution data, which is 100
    # (by default the split contents of dlply is passed to the function as the first argument)
    .progress="text" # adds a progress bar
)



datazones_id_link <- data.frame(
    zone_id=datazones_ids,
    zone_name=datazones_shp[["zonecode"]]
)


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


fn <-function(x){
    this_year <- x$year[1]
    
    g1 <- ggplot(data=x)
    g2 <- g1 + geom_path(aes(x=long, y=lat, group=id), colour="grey") + coord_equal()
    g3 <- g2 + theme_clean() + scale_fill_gradient(low="white", high="red") 
    
 
    try(g_pm10 <- g3 + geom_polygon(aes(x=long, y=lat, fill=pm10, group=id)) + coord_equal())
    try(
        ggsave(g_pm10,
        filename=paste0("Figures/pollution_by_dz/pm10_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    try(g_pm2.5 <- g3 + geom_polygon(aes(x=long, y=lat, fill=pm2.5, group=id)) + coord_equal())
    try(        
        ggsave(g_pm2.5,
        filename=paste0("Figures/pollution_by_dz/pm2_5_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    try(g_no2 <- g3 + geom_polygon(aes(x=long, y=lat, fill=no2, group=id)) + coord_equal())
    try(    
        ggsave(g_no2,
        filename=paste0("Figures/pollution_by_dz/no2_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))

    try(g_nox <- g3 + geom_polygon(aes(x=long, y=lat, fill=nox, group=id)) + coord_equal())
    try(
        ggsave(g_nox,
        filename=paste0("Figures/pollution_by_dz/nox_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    try(g_co <- g3 + geom_polygon(aes(x=long, y=lat, fill=co, group=id)) + coord_equal())
    try(        
        ggsave(g_co,
        filename=paste0("Figures/pollution_by_dz/co_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    try(g_ozone <- g3 + geom_polygon(aes(x=long, y=lat, fill=ozone, group=id)) + coord_equal())
    try(
        ggsave(g_ozone,
        filename=paste0("Figures/pollution_by_dz/ozone_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    try(g_benzene <- g3 + geom_polygon(aes(x=long, y=lat, fill=benzene, group=id)) + coord_equal())
    try(        
        ggsave(g_benzene,
        filename=paste0("Figures/pollution_by_dz/benzene_", this_year, ".tiff"),
        width=8, height=10, units="cm"
    ))
    
    NULL    
}

d_ply(pollutants_joined, .(year), fn, .progress="text")



# # try PM2.5 in 2012 
# resPM25 <- gavin_linkage_fn(subset(all_pollution_data_long,year==2012 & pollutant=='pm2.5'),dz_centroids=datazones_centroids,nearest_P=0.001)
# resPM25_1 <- gavin_linkage_fn(subset(all_pollution_data_long,year==2012 & pollutant=='pm2.5'),dz_centroids=datazones_centroids,nearest_P=0.0005)
# resPM25_2 <- gavin_linkage_fn(subset(all_pollution_data_long,year==2012 & pollutant=='pm2.5'),dz_centroids=datazones_centroids,nearest_P=0.00025)
# resPM25_3 <- gavin_linkage_fn(subset(all_pollution_data_long,year==2012 & pollutant=='pm2.5'),dz_centroids=datazones_centroids,nearest_P=0.0001)
# 
# 
# # visualise
# plotdata <- data.frame(datazones_centroids@coords,resPM25,resPM25_1,resPM25_2,resPM25_3)
# names(plotdata) <- c("x","y","nearest_0.1%_pollutiondata","nearest_0.05%_pollutiondata","nearest_0.025%_pollutiondata",
#                      "nearest_0.01%_pollutiondata")
# 
# # melt into a format that plyr can split more easily
# plot_long <- melt(
#     plotdata,
#     id.vars=c("x", "y"),
#     variable.name="res"
# )
# ggplot(
#     plot_long,
#     aes(x=x, y=y, colour=value)
# ) + geom_point(size=0.5) + facet_wrap(~ res)
# 
# 
# ggsave(file="pm2.5_wrap.png")
                  