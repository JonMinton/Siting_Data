# # Attempt at kriging continuous surface using 
# # fields::Krig
# 
# # 17/10/14
# 
# #using pm25 for 2011 
# 
# data_ss <- subset(
#     tidy_block,
#     select=c("x", "y", "pm2.5"),
#     subset=year==2011
#     )
# 
# # remove any missing data
# 
# data_ss <- data_ss[!(is.na(data_ss$pm2.5)),]
# 
# 
# # X <- as.matrix(data_ss[,1:2])
# # y <- data_ss$pm2.5
# # fit <- Krig(X, y)
# # 
# # 
# # #
# # fit <- Krig(ozone$x, ozone$y, theta=20)  
# 
# # Using example from:
# # http://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf
# 
# coordinates(data_ss) <- ~ x + y
# # Changes class of object from dataframe
# 
# fit <- krige(pm2.5 ~ 1, data_ss)
# # PROBLEM: I need to create an object saying which new points should be predicted, created in the correct 
# # object format. 
# # http://www.bias-project.org.uk/ASDARcourse/unit4_slides.pdf
# 
# 
# # Something else to look at:
# # http://rgm.ogalab.net/RGM/R_rdfile?f=kriging/man/kriging.Rd&d=R_CC


##########################################################################################
# Another approach: akima:
# http://cran.r-project.org/web/packages/akima/akima.pdf

# I'm going to try this out instead. The formulae look much simpler


data_ss <- subset(
    tidy_block,
    select=c("x", "y", "pm2.5"),
    subset=year==2011
)

# remove any missing data

data_ss <- data_ss[!(is.na(data_ss$pm2.5)),]

z_matrix <- acast(
    data_ss,
    x ~ y
    )

pm2_5 <- list(
    x=rownames(z_matrix),
    y=colnames(z_matrix),
    z=z_matrix
    )

pm2_5_lin <- interp(data_ss$x, data_ss$y, data_ss$pm2.5,
                    xo=seq(min(data_ss$x), max(data_ss$x), length = length(data_ss$x) * 10),
                    yo=seq(min(data_ss$y), max(data_ss$y), length = length(data_ss$y) * 10)
)

# This produces a memory allocation error. However it should be easy enough to split into smaller chunks

data_ss_1 <- data_ss[1:as.integer(dim(data_ss)[1]/1000),]

pm2_5_lin <- interp(data_ss_1$x, data_ss_1$y, data_ss_1$pm2.5,
                    xo=seq(min(data_ss_1$x), max(data_ss_1$x), length = length(data_ss_1$x) * 10),
                    yo=seq(min(data_ss_1$y), max(data_ss_1$y), length = length(data_ss_1$y) * 10)
)

qplot(x=x, y=y, z=pm2.5, colour=pm2.5, data=data_ss, geom="tile")

cuts <- (1:as.integer(dim(data_ss)[1] / 250)) * 250

fn <- function(
    this_lim,
    data,
    increment=250,
    interpolation_scale=10
    ){
    
    this_subset <- data[(this_lim - increment + 1):this_lim,]
    
    this_interpolation <- try(interp(
        this_subset$x, this_subset$y, this_subset$pm2.5,
        xo = seq(min(this_subset$x), max(this_subset$x), length= length(this_subset$x) * interpolation_scale),
        yo = seq(min(this_subset$y), max(this_subset$y), length= length(this_subset$y) * interpolation_scale),
         )
    )
     
    if(class(this_interpolation)!="try-error"){
        interpolation_matrix <- as.matrix(this_interpolation$z)  
        dimnames(interpolation_matrix) <- list(
            row_names=this_interpolation$x,
            col_names=this_interpolation$y
        )
        
        write.csv(
            interpolation_matrix,
            file=paste0(
                "interpolation_matrices/",
                "m_",this_lim - increment + 1, "_", this_lim, ".csv"
            )
        )            
    }
    
}

l_ply(
    cuts,
    fn,
    data=data_ss
    )

# #install.packages("akima")
# require(akima)
# # first example:
# ## Not run:
# require(rgl)
# data(akima)
# # data
# rgl.spheres(akima$x,akima$z , akima$y,0.5,color="red")
# rgl.bbox()
# # bivariate linear interpolation
# # interp:
# akima.li <- interp(akima$x, akima$y, akima$z,
#                    xo=seq(min(akima$x), max(akima$x), length = 100),
#                    yo=seq(min(akima$y), max(akima$y), length = 100))
# # interp surface:
# rgl.surface(akima.li$x,akima.li$y,akima.li$z,color="green",alpha=c(0.5))
# # interpp:
# akima.p <- interpp(akima$x, akima$y, akima$z,
#                    runif(200,min(akima$x),max(akima$x)),
#                    runif(200,min(akima$y),max(akima$y)))
# # interpp points:
# rgl.points(akima.p$x,akima.p$z , akima.p$y,size=4,color="yellow")
# # bivariate spline interpolation
# # data
# rgl.spheres(akima$x,akima$z , akima$y,0.5,color="red")
# rgl.bbox()
# # bivariate cubic spline interpolation
# # interp:
# akima.si <- interp(akima$x, akima$y, akima$z,
#                    xo=seq(min(akima$x), max(akima$x), length = 100),
#                    yo=seq(min(akima$y), max(akima$y), length = 100),
#                    linear = FALSE, extrap = TRUE)
# 
# # interp surface:
# rgl.surface(akima.si$x,akima.si$y,akima.si$z,color="green",alpha=c(0.5))
# # interpp:
# akima.sp <- interpp(akima$x, akima$y, akima$z,
#                     runif(200,min(akima$x),max(akima$x)),
#                     runif(200,min(akima$y),max(akima$y)),
#                     linear = FALSE, extrap = TRUE)
# # interpp points:
# rgl.points(akima.sp$x,akima.sp$z , akima.sp$y,size=4,color="yellow")
# ## End(Not run)
# 
