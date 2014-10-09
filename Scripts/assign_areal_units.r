

# Code to work with shapefiles

# 2/10/2014

# The aim of this is to assign a point to an areal unit. 

# tasks: 
# 1) load shapefile
# 2) extract coordinates of vertices of shapefile
# 3) find unique coordinate configurations
# 4) 

spatial_data <- readShapePoly(
    "data/shapefiles/unzipped/England_oa_2011.shp"
    )

# change memory limit 
# memory.limit(4095)

# the shapefile is too large to load into the workspace



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
