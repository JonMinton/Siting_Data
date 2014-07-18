# script for managing siting data

# first step: download and make stls of pollution data


rm(list=ls())


####

source("scripts/LoadPackages.R")

RequiredPackages(
    c(
        "ggplot2",
        "r2stl",
        "reshape2"
        # INSTALLED PACKAGES HERE        
        )
    )


# Pollution data:
links_table <- read.csv("data/raw/pollution_data_links_1x1grid_uk.csv", stringsAsFactors=F)
names(links_table) <- tolower(names(links_table))

# first link:
link.this <- links_table$link[1]

data.this <- read.csv(
    link.this, stringsAsFactors=F,
    na.strings="MISSING"
    )

year.this <- as.numeric(data.this[1,1])
measure.this <- data.this[2,1]
variable.this <- data.this[3,1]
names.this <- as.character(data.this[5,])
names.this[4] <- sub(year.this, "", names.this[4])
names(data.this) <- names.this
data.this <- data.this[-c(1:5),]
data.this$year <- year.this
data.this$ukgridcode <- as.numeric(data.this$ukgridcode)
data.this$x <- as.numeric(data.this$x)
data.this$y <- as.numeric(data.this$y)
data.this[,4] <- as.numeric(data.this[,4])

head(data.this)

data.this$pm10[is.na(data.this$pm10)] <- 0

z.expanded <- recast(data.this, x ~ y, measure.var="pm10")
r2stl(as.numeric(colnames(z.expanded)), as.numeric(rownames(z.expanded)), as.numeric(z.expanded), filename="test.stl", z.expand=T)


