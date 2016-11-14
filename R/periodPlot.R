library(ggplot2); theme_set(theme_bw())
library(dplyr)
source("lhs_y.R")
source("period.R")
load("../data/period.rda")

y <- lhs_y(range = c(0.1, 1), n.state = 4, n.sim = 1000, seed = 101)
tvec <- seq(0, 50, 0.1)

period <- lapply(LHSres, function(x){
    getPeriod(x[,2])
})

period <- unlist(period)

min <- min(period)

df <- LHSres %>%
    bind_rows

df$period <- rep(period, each = length(tvec))
df$sim <- rep(1:1000, each = length(tvec))

# path plot
ggplot(subset(df, time > 30 & period < 1.01 * min), aes(N1, N2)) +
    geom_path(aes(group = sim, col = period))

# hist
ggplot(subset(df, time == 0)) +
    geom_histogram(aes(period), bins = 50)

gbackground <- ggplot(subset(df, time > 30 & period < 1.03 * min), aes(N1, N2)) +
    geom_path(aes(group = sim, col = period, alpha = -period)) +
    scale_x_continuous(limits =c(0.13, 0.4), expand = c(0,0)) +
    scale_y_continuous(limits =c(0.13, 0.4), expand = c(0,0)) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        legend.position = "none",
        panel.margin = grid::unit(0, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank()
    ) +
    stat_function(fun = function(x)1/2-x, lty = 2, col = "tomato2")

ggsave("background.png", gbackground, width = 8, height = 8, dpi = 600)
