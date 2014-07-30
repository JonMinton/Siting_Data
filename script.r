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
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2009.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2010.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2011.csv"
           & link!="http://uk-air.defra.gov.uk/datastore/pcm/mapbz2012.csv"
           )$link,
    try(wrangler),
    .progress="text"
    )

save(block, file="data/robj/block.rdata")

flat_block <- ldply(
    block
    )

#####
load("data/robj/flat_block.rdata")

tidy_block  <- dcast(flat_block, formula = ukgridcode + x + y + year ~ variable, mean)
tidy_block[sapply(tidy_block, is.nan)] <- NA

save(tidy_block, file="data/robj/tidy_block.rdata")
write.csv(tidy_block, file="data/raw/tidy_block.csv")
# Plot this?

##################################


ggplot(
    tidy_block,
    aes(x=x, y=y, colour=pm10)
    ) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/pm10_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=pm2.5)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/pm2_5_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=no2)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/no2_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=nox)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/nox_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=co)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/co_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=so2)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/so2_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=ozone)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/ozone_wrap.png")

ggplot(
    tidy_block,
    aes(x=x, y=y, colour=benzene)
) + geom_point() + facet_wrap( ~ year) 
ggsave(file="figures/benzene_wrap.png")



