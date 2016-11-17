library(ggplot2); theme_set(theme_bw())
library(tidyr)
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
g.path <- ggplot(subset(df, time > 30 & period < 1.01 * min), aes(N1, N2)) +
    geom_path(aes(group = sim, col = period))

df.ini <- df %>%
    filter(time == 0) %>%
    select(-c(sim, time)) 

g.period <- df.ini %>%
    ggplot() +
        geom_point(aes((N1 * N2 * N3 * N4)/(N1 + N2 + N3 + N4)^4, period), size = 0.8) + 
        scale_x_log10(name = expression(N[1]~N[2]~N[3]~N[4]/(N[1]+N[2]+N[3]+N[4])^4), breaks = c(0.0005,0.001, 0.002, 0.004)) +
        theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = "in"),
            axis.title.y = element_text(margin=margin(0,15,0,0)),
            axis.title.x = element_text(margin=margin(10,0,0,0))
        )

ggsave("period.png", g.period, width = 8, height = 8, dpi = 600)

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
