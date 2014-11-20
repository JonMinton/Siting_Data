require(maptools)
require(rgeos)
require(rgdal)
require(geoR)
require(plyr)
require(reshape2)

datazones_shp <-  readShapePoly("G:/dropbox/Dropbox/Data/Shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")
# assign the correct projection
proj4string(datazones_shp) <-  CRS("+init=epsg:27700")
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

str(datazones_centroids)



#######################################################################################
gavin_linkage_fn <- function(
    pollution_data,
    dz_centroids,
    range_param=100,
    num_samples=5000
    ){

    #@YearlyPollutionData is the Grid Pollution data year by year; A dataframe with coordinates
    #@pollution_name is the pollutants we want to calculate for Scottish Data zones
    ### Note before using this function, some data preparation is needed.
    #@ 1: the Scottish Data Zone; The names is DZ_centroid, otherwise we need change the names within the function
    #@ 2: yearly pollution data; the names is PD.data, otherwise we need change the names within the function
    #@ 3: make sure there is no missing data for each pollutant in the Pollutant grid point data
    #  Finally, we can directly merge the calculated pollution data to the Scottish Data Zone
    
    num_pollutant_obs <- nrow(pollution_data)
    num_dzs <- nrow(dz_centroids) # the number of data zones in Scotland
    out <- rep(NA, length=num_dzs)
    # Temporally keep the range parameters through the simulations
    range_temp <- rep(0,range_param)
    for (i in 1:range_param) {
        # draw samples
        temp_pollution_data <- pollution_data[
            sample(
                1:nrow(pollution_data),
                min(num_samples, num_pollutant_obs),
                rep=FALSE
            ),
            ]
        
        # Note the distance measured using KM
        # keep the THIRD and FOURTH columns are coordinates of the yearly pollution data
        dd <- variog(
            coords=temp_pollution_data[,c("x","y")]/1000,
            data=temp_pollution_data[,"value"], 
            messages=FALSE
        )
        
        dd_fit <- variofit(
            dd,
            cov.model="gaussian",
            messages=FALSE
        )
        
        range_temp[i] <- dd_fit$cov.pars[2]
        if (i %% 10 ==0) print(i)
    }
    range <- as.numeric(mean(range_temp))
    # assume a 0.7 correlation threshold; can change but need to aware that smaller threshold implies longer computing time
    threshold_dist <- range*sqrt(-log(0.7))
    
    
    ######## Step 3
    
    # The indices of srounding pollution data points
    #ind <- list()
    # weights through a Gaussian kernel as we did in Step 2
    #Gweight <- list()
    coord_temp <- as.matrix(pollution_data[,c("x", "y")]/1000)
    
    # the range parameter obtained in Step 2. We also assume it to be the
    # the threshold of distance beyond which there is no spatial correlations
    # although it is not entirely true as the spatial correlation is about exp{-1} = 0.37
    
    for (i in 1:num_dzs) {
        # First calculate the distances between each dz and all pollution data
        dist_temp <- spDistsN1(
            pts=coords.temp,
            pt=DZ_centroid@coords[i,]/1000
        )
        # Change distance to geographical weights using parameters calibrated in Step 2
        ind_temp <- which(
            dist_temp <= threshold_dist,
            arr.ind=TRUE
        )
        # Gaussian kernel
        w_temp <- exp(-(dist_temp[ind_temp]/range)^2)
        
        out[i] <- sum(as.numeric(pollution_data[ind_temp]*w_temp))/sum(w_temp)
        if(i %% 1000 == 0) print(i)
    }
        
        # This takes about 1.7 minutes
        # finish the calculation of the k-th pollutant
        
    return(out)
}

debug(gavin_linkage_fn)

# input: dataframe
# output: list
out_list <- dlply(
    all_pollution_data_long, # the input
    .(year, pollutant), # what to split by
    gavin_linkage_fn, # the function to apply
    dz_centroids=datazones_centroids # additional unspecified arguments to function
    # (by default the split contents of dlply is passed to the function as the first argument)
    )



# year 2012
PD.2012 <- subset(PD,year==2
                  # year 2012 and pollutant in/around Scotland
                  PD.2012.Scotland <- subset(PD.2012,x >=50000 & x <= 500000 & y >= 500000 & y <= 1300000)
                  # plot(PD.2012.Scotland$x,PD.2012.Scotland$y)
                  PD.data <- PD.2012.Scotland
                  
                  
                  #rm(list=setdiff(ls(),c('PD.2012.Scotland','PD.data','DZ_centroid','DZ','ID_names','PD','PD.2012')))
                  cnames <- names(PD.data)
                  
                  ### Using the function - Linkage below
                  res1 <- Linkage(YearlyPollutionData=PD.data,pollution_name=c("pm10","pm2.5","nox"))
                  
                  
                  
                  ######## Step 2
                  
                  