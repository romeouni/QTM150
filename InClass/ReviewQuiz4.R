library(ggplot2)
library(ggthemes)
mpg
?mpg


ggplot(mpg, aes(displ, hwy, color = factor(year))) + geom_point() + geom_smooth(method = "lm")

ggplot(mpg, aes(cty, hwy)) + geom_point() + facet_wrap(~class) + geom_jitter()

                                          