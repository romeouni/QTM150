library(ggplot2)
library(ggthemes)

#Do cars with big engines use more fule than cars with small engines?

?mpg
highway_size <- ggplot(mpg, aes(displ, hwy)) + geom_point()
city_size <- ggplot(mpg, aes(displ, cty)) + geom_point()
highway_size <- highway_size + facet_wrap(~cyl)
highway_size
highway_size <- ggplot(mpg, aes(displ, hwy)) + geom_point()
highway_size
install.pack