library(ggplot2); theme_set(theme_bw(base_family = "serif"))
library(gridExtra)
load("../data/epsilon.rda")

sL <- do.call("rbind", resList)

names(sL) <- c("time", "N1", "N2", "N3", "N4", "epsilon")
sL$sim <- rep(1:9, each = 1001)

g.base <- ggplot(sL) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), units = "in")
    )

x.remove <- theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

y.remove <- theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

brk1 <- c(0.03, 0.06, 0.09)
brk2 <- c(0.35, 0.45, 0.55)
expand0 <- c(0.02, 0)

x.N1 <- scale_x_continuous(name = expression(N[1]),
    breaks = brk1,
    expand = expand0)
x.N2 <- scale_x_continuous(name = expression(N[2]),
    breaks = brk2,
    expand = expand0)
y.N3 <- scale_y_continuous(name = expression(N[3]),
    breaks = brk1,
    expand = expand0)
y.N4 <- scale_y_continuous(name = expression(N[4]),
    breaks = brk2,
    expand = expand0)

g13 <- g.base +
    geom_path(aes(N1, N3, group = sim, col = epsilon)) +
    x.N1 +
    y.N3 +
    theme(axis.title.y = element_text(margin=margin(0,8,0,0)))

g14 <- g.base +
    geom_path(aes(N1, N4, group = sim, col = epsilon)) +
    x.N1 +
    y.N4 +
    x.remove + 
    stat_function(fun = function(x)1/2-x, lty = 2,
        col = "tomato2") +
    theme(axis.title.y = element_text(margin=margin(0,8,0,0)))
    
g23 <- g.base +
    geom_path(aes(N2, N3, group = sim, col = epsilon)) +
    x.N2 +
    y.N3 +
    y.remove +
    stat_function(fun = function(x)1/2-x, lty = 2,
        col = "tomato2",
        xlim = c(0.3948, 0.4904))

g24 <- g.base +
    geom_path(aes(N2, N4, group = sim, col = epsilon)) +
    x.N2 +
    y.N4 +
    x.remove +
    y.remove +
    theme(legend.key.size = unit(c(0.15), unit = "in"),
        legend.justification = c(1,1),
        legend.position = c(1,1)) +
    scale_color_continuous(name = expression(~epsilon))

g <- arrangeGrob(g14, g24, g13, g23, nrow = 2,
    widths = c(1.2, 1),
    heights = c(1, 1.2)
)

ggsave(file = "epsilon.png", g, width = 8, height = 8, dpi = 1200)

## make gif?

setwd("../misc/gif")

sub <- subset(sL, sim == 3)
tvec <- sub$time
lim <- 481
tvec <- tvec[seq(1,lim, by = 4)]

fn <- sprintf("epsilon_%03d", 1:length(tvec))

for(i in 1:length(tvec)){
    cat(i)
    j <- tvec[i]
    
    tmpdf <- subset(sub, time == j)
    g14tmp <- g14 + 
        geom_point(data = tmpdf, aes(N1, N4))
    g24tmp <- g24 +
        geom_point(data = tmpdf, aes(N2, N4))
    g13tmp <- g13 +
        geom_point(data = tmpdf, aes(N1, N3))
    g23tmp <- g23 +
        geom_point(data = tmpdf, aes(N2, N3))
    gtmp <- arrangeGrob(g14tmp, g24tmp, g13tmp, g23tmp, nrow = 2,
                     widths = c(1.2, 1),
                     heights = c(1, 1.2)
    )
    ggsave(file = paste0(fn[i], ".png"), gtmp, width = 6, height = 6, dpi = 300)
}
