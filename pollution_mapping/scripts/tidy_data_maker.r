## The following code takes the link identifiers for each of the 1x1km modelled pollution data from 
# defra and combines it into a single dataset 

# For an unknown reason a small subset of the datasets could not be read directly from the defra website
# and had to be loaded into excel and then resaved as csvs in order to work. They have been housed 
# at the location data/fixed/ within the current directory structure. 
# They are handled separately.

# Because of the large size of the combined dataset github cannot be used. Instead a copy of the file is 
# stored on dropbox. The url for this file is indicated below. 

# Pollution data:
links_table <- read.csv("data/raw/pollution_data_links_1x1grid_uk.csv", stringsAsFactors=F)
names(links_table) <- tolower(names(links_table))


wrangler <- function(link.this){
    print(link.this)
    
    
    data.this <- read.csv(
        link.this, stringsAsFactors=F,
        na.strings="MISSING", 
        skipNul=TRUE # trying this argument as per http://stat.ethz.ch/R-manual/R-patched/library/utils/html/read.table.html
    )
    
    
    year.this <- as.numeric(data.this[1,1])
    variable.this <- names(data.this)[1]
    names.this <- as.character(data.this[5,])
    names.this[4] <- variable.this
    names(data.this) <- names.this
    data.this <- data.this[-c(1:5),]
    data.this$year <- year.this
    data.this$ukgridcode <- as.numeric(data.this$ukgridcode)
    data.this$x <- as.numeric(data.this$x)
    data.this$y <- as.numeric(data.this$y)
    data.this[,4] <- as.numeric(data.this[,4])
    
    # Remove all NAs from the value column
    
    output <- melt(data.this, id=c("ukgridcode", "x", "y", "year"))
    
    output <- subset(output, subset=!is.na(value))        
    
    return(output)
}

# For some reason mapcom8hr2006 is causing problems. Will remove it for now and see if the code below will now run


block <- llply(
    subset(links_table, 
           subset=link!="http://uk-air.defra.gov.uk/datastore/pcm/mapcom8hr2006.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2007.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2008.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2009.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2010.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2011.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2012.csv"
    )$link,
    try(wrangler),
    .progress="text"
)

# Now to do the same with the local files

local_files <- paste(
    "data/fixed",
    c(
        "mapcom8hr2006.csv",
        "mapbz2007.csv",
        "mapbz2008.csv",
        "mapbz2009.csv",
        "mapbz2010.csv",
        "mapbz2011.csv",
        "mapbz2012.csv"
    ),
    sep="/"   
)

block_local <- llply(
    local_files,
    try(wrangler),
    .progress="text"
)

block <- c(
    block,
    block_local
)

save(block, file="data/robj/block.rdata")



flat_block <- ldply(
    block
)

save(flat_block, file="data/robj/flat_block.rdata")
#####
load("data/robj/flat_block.rdata")

tidy_block  <- dcast(flat_block, formula = ukgridcode + x + y + year ~ variable, mean)
tidy_block[sapply(tidy_block, is.nan)] <- NA

save(tidy_block, file="data/robj/tidy_block.rdata")
write.csv(tidy_block, file="data/raw/defra_pollution_estimates_combined.csv")

#Dropbox link to above data:
#https://www.dropbox.com/s/jyq4l2mm7bgbtjo/defra_pollution_estimates_combined.csv
