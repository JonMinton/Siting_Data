

# Code to work with shapefiles

# 2/10/2014

# The aim of this is to assign a point to an areal unit. 

# tasks: 
# 1) load shapefile
# 2) extract coordinates of vertices of shapefile
# 3) find unique coordinate configurations
# 4) 

# spatial_data <- readShapePoly(
#     "data/shapefiles/unzipped/England_oa_2011.shp"
#     )

spatial_data <- readShapePoly(
    "E:/gis_data/shapefiles/scotland_2001_datazones/scotland_dz_2001.shp"
)


# This works on campus!



# Looking at the gcontains function from rgeos, because of the following
# http://stackoverflow.com/questions/21971447/check-if-point-is-in-spatial-object-which-consists-of-multiple-polygons-holes


# I want a data frame containing the unique eastings and westings coordinates from the combined point dataframe

head(tidy_block)

tidy_block$X <- NULL

xy <- paste(tidy_block$x, tidy_block$y, sep="_")
grd <- subset(tidy_block, select=c("ukgridcode", "x" ,"y"))
grd <- data.frame(grd, xy=xy)
grd <- grd[unique(grd$xy),]

# save grd only 

write.csv(
    grd, 
    "data/point_codes.csv"
    )

grdpts <- subset(grd, select=c("x", "y"))
grdpts <- SpatialPoints(grdpts, proj4string=CRS(proj4string(spatial_data)))


## Interrogating the spatial_data object

slotNames(spatial_data)

tmp <- spatial_data@polygons[[1]]

gContains(tmp, grdpts)


# Will also look at using 

fastshp::read.shp(
    "data/shapefiles/unzipped/england_oa_2011.shp"
    )

# from http://www.rforge.net/doc/packages/fastshp/read.shp.html


# tried: cannot install

#Now look at readshapepoints rather than readshapepoly

#http://gis.stackexchange.com/questions/19064/how-to-open-a-shapefile-in-r

spatial_data <- readShapePoints(
    "data/shapefiles/unzipped/england_oa_2011.shp"
    )

# Does not work, as not points

# Another instruction for installing fashshp
#http://www.rforge.net/fastshp/git.html

install.packages("fashshp",,"http://rforge.net/", type="source")

install.packages("fashshp",,"http://rforge.net/")

install.packages("fashshp",,"http://rforge.net/", type="source")

"data/shapefiles/unzipped/england_oa_2011.shp"


#####################


# Now trying rgdal::readOGR

spatial_data <- readOGR(
    "data/shapefiles/unzipped",
    "England_oa_2011"
    )



spatial_data <- readOGR(
    "data/shapefiles/zipped/",
    "England_oa_2011"
)

# error: layer not found


####################


# now looking at shapefiles package

#install.packages("shapefiles")

require(shapefiles)

spatial_data <- read.shapefile(
    "data/shapefiles/unzipped/"
    )
