# Attempt at kriging continuous surface using 
# fields::Krig

# 17/10/14

#using pm25 for 2011 

data_ss <- subset(
    tidy_block,
    select=c("x", "y", "pm2.5"),
    subset=year==2011
    )

# remove any missing data

data_ss <- data_ss[!(is.na(data_ss$pm2.5)),]


# X <- as.matrix(data_ss[,1:2])
# y <- data_ss$pm2.5
# fit <- Krig(X, y)
# 
# 
# #
# fit <- Krig(ozone$x, ozone$y, theta=20)  

# Using example from:
# http://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf

coordinates(data_ss) <- ~ x + y
# Changes class of object from dataframe

fit <- krige(pm2.5 ~ 1, data_ss)
# PROBLEM: I need to create an object saying which new points should be predicted, created in the correct 
# object format. 
# http://www.bias-project.org.uk/ASDARcourse/unit4_slides.pdf


# Something else to look at:
# http://rgm.ogalab.net/RGM/R_rdfile?f=kriging/man/kriging.Rd&d=R_CC
