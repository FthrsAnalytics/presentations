library(ggplot2)
library(dplyr)

dataset %>%
filter(Points >0) %>%
ggplot(aes(x = Price, y = Points)) +
geom_point(aes(color = Position), position = "jitter") +
geom_smooth(method = lm, se = FALSE) +
geom_hline(yintercept = c(100, 200)) +
annotate("rect", xmin = 10, xmax = 12.5, ymin = 200, ymax = 275, alpha = .3)
