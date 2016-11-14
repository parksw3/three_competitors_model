library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tidyr)

l <- load("../data/bifurcation.rda")

df <- resList %>%
    bind_rows %>%
    filter(time > 180) %>%
    select(-time) %>%
    gather(key, value, -alpha) %>%
    mutate(value = round(value, 5)) %>%
    unique

g <- ggplot(df, aes(alpha, value, col = key)) +
    geom_point(size = 0.1, alpha = 0.5) + 
    scale_y_continuous(name = "") +
    scale_x_continuous(name = expression(alpha), expand = c(0,0)) + 
    scale_colour_discrete(
        name = "State",
        labels = list(bquote(N[1]),
                      bquote(N[2]),
                      bquote(N[3]))
    ) + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave("bifurcation.png", g, width = 8, height = 3, dpi = 600)

g.blank <- g +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        legend.position = "none",
        panel.margin = grid::unit(0, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank()
    ) + facet_wrap(~key, nrow = 3)

ggsave("bifurcation_blank.png", g.blank, width = 6, height = 6, dpi = 600)