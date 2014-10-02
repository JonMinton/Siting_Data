

# Code to work with shapefiles

# 2/10/2014

# The aim of this is to assign a point to an areal unit. 

# tasks: 
# 1) load shapefile
# 2) extract coordinates of vertices of shapefile
# 3) find unique coordinate configurations
# 4) 

spatial_data <- readShapePoly(
    "data/shapefiles/unzipped/england_oa_2011.shp"
    )

# change memory limit 
# memory.limit(4095)

# the shapefile is too large to load into the workspace

spatial_data <- readShapePoly(
    "data/shapefiles/unzipped/england_oa_2011.shp"
)
