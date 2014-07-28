# script for managing siting data

# first step: download and make stls of pollution data


rm(list=ls())


####

source("scripts/LoadPackages.R")

RequiredPackages(
    c(
        "ggplot2",
        "r2stl",
        "reshape2",
        "plyr"
        )
    )


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
           )$link,
    try(wrangler),
    .progress="text"
    )



# Plot this?

ggplot(
    data.this_melted,
    aes(x=x, y=y, colour=value)
    ) + geom_point()




r2stl(as.numeric(colnames(z.expanded)), as.numeric(rownames(z.expanded)), as.numeric(z.expanded), filename="test.stl", z.expand=T)


