###### Data linkage of Scotish data zones and pollution data
setwd("U:/sorting models/data linkage/Scotland_dz_2001")
library(maptools)
library(rgeos)
library(rgdal)
library(geoR)

### Read in the Scotish data zone shapefile
DZ <- readShapePoly("scotland_dz_2001.shp")
# assign the correct projection
BNG <- CRS("+init=epsg:27700")
proj4string(DZ) <- BNG
ID_names <- sapply(slot(DZ, "polygons"), function(x) slot(x, "ID"))# just check 
# Centroids of DZ
DZ_centroid <- gCentroid(DZ,byid=TRUE)
DZ_centroid <- SpatialPointsDataFrame(DZ_centroid,DZ@data)

### Read in the pollution data
PD <- read.table("defra_pollution_estimates_combined.csv",sep=",",header=TRUE)
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
Linkage <- function(YearlyPollutionData,pollution_name){
start.time <- proc.time()
#@YearlyPollutionData is the Grid Pollution data year by year; A dataframe with coordinates
#@pollution_name is the pollutants we want to calculate for Scottish Data zones
### Note before using this function, some data preparation is needed.
#@ 1: the Scottish Data Zone; The names is DZ_centroid, otherwise we need change the names within the function
#@ 2: yearly pollution data; the names is PD.data, otherwise we need change the names within the function
#@ 3: make sure there is no missing data for each pollutant in the Pollutant grid point data
#  Finally, we can directly merge the calculated pollution data to the Scottish Data Zone
PD.data <- YearlyPollutionData
N <- dim(DZ_centroid)[1] # the number of data zones in Scotland
Pnames <- pollution_name
K <- length(Pnames)
DATA <- data.frame(matrix(0,nrow=N,ncol=K))
names(DATA) <- Pnames

for (k in 1:K) {
varName <- Pnames[k]
# PM10 - cannot be done for all the samples (110836 in 2012)
# Therefore we do this via plenty of simulations to find range parameters for each pollutant at each year
# It thus capture the spatial correlations of pollutants quite flexibly
# the number of simulation is set to 100 and the sample drawn is set to 5000

# Temporally keep the range parameters through the simulations
Range.temp <- rep(0,100)
for (i in 1:100) {
	# draw samples
	ind.1 <- sample(1:dim(PD.data)[1],5000,rep=FALSE)
	temp.PD.data <- PD.data[ind.1,]
	# Note the distance measured using KM
	# keep the THIRD and FOURTH columns are coordinates of the yearly pollution data
	dd <- variog(coords=temp.PD.data[,3:4]/1000,data=temp.PD.data[varName],messages=FALSE)
	dd.fit <- variofit(dd,cov.model="gaussian",messages=FALSE)
	Range.temp[i] <- dd.fit$cov.pars[2]
	if (i %% 10 ==0) print(i)
}
rm(i)
#for the k-th pollutant
Range.k <- as.numeric(mean(Range.temp))
# assume a 0.7 correlation threshold; can change but need to aware that smaller threshold implies longer computing time
threshold.dist <- Range.k*sqrt(-log(0.7))


######## Step 3

# The indices of srounding pollution data points
#ind <- list()
# weights through a Gaussian kernel as we did in Step 2
#Gweight <- list()
coords.temp <- as.matrix(PD.data[,3:4]/1000)

# the range parameter obtained in Step 2. We also assume it to be the
# the threshold of distance beyond which there is no spatial correlations
# although it is not entirely true as the spatial correlation is about exp{-1} = 0.37

for (i in 1:N) {
	# First calculate the distances between each dz and all pollution data
	dist.temp <- spDistsN1(pts=coords.temp,pt=DZ_centroid@coords[i,]/1000)
	# Change distance to geographical weights using parameters calibrated in Step 2
	ind.temp <- which(dist.temp <= threshold.dist,arr.ind=TRUE)
	# Gaussian kernel
	W.temp <- exp(-(dist.temp[ind.temp]/Range.k)^2)

	DATA[i,k] <- sum(as.numeric(PD.data[ind.temp,varName]*W.temp))/sum(W.temp)
	if(i %% 1000 == 0) print(i)
}

# This takes about 1.7 minutes
# finish the calculation of the k-th pollutant
}
# finish the function
time <- proc.time()-start.time
### results just keep the interpolated pollutant attributes
results <- list()
results$time <- time
results$DATA <- DATA
return(results)

}











