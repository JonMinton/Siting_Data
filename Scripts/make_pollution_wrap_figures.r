
##################################
### The following produces the figures for each of the pollutants tiled by year


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

