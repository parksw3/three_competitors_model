library(ggplot2); theme_set(theme_bw())
library(deSolve)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
source("../R/model.R")
source("../R/functions.R")
scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)

pars1 <- list(
    matvals = c(0.3, 1, 1.7)
)

yini1 <- list(0.1, 0.8, 0.2, 0.4)
model1 <- base.model(pars1)
tvec <- seq(0, 50, 0.01)

r <- ode(unlist(yini1), tvec, model1, pars1)

df <- r %>%
    as.data.frame %>%
    gather(key, value, -time)

g1.labs <- list(bquote(N[1]), bquote(N[2]), bquote(N[3]), bquote(N[4]))

g1 <- df %>%
    ggplot(aes(time, value, col = key)) +
        geom_line(lwd = 1) +
        scale_x_continuous(expand = c(0, 0), name = "Time") +
        scale_y_continuous(expand = c(0, 0), name = NULL, limits = c(0, 0.8)) +
        scale_colour_discrete(
            name = NULL,
            labels = g1.labs) +  
        theme(plot.margin = unit(c(0.3, 0, 0.1, 0.2), units = "in"))

g1 <- g1 + annotation_custom(grobTree(
    textGrob(expression(
        paste(alpha==0.3, ", ", beta==1, ", ", gamma==1.7)
    ))
), xmin = 37, ymin = 0.73)

ggsave("dynamics.png", g1, width = 8, height = 6, dpi = 600)

df2 <- df %>%
    mutate(group = ifelse(key %in%  c("1", "3"), "A", "B")) %>%
    mutate(key = ifelse(key %in% c("1", "2"), "x", "y")) %>%
    spread(key, value)

phase1 <- df2 %>%
    filter(group == "A") %>%
    ggplot(aes(x, y)) +
        theme(axis.title.y = element_text(angle = 0),
              plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = "in")) +
        coord_fixed(ratio = 1)

phase2 <- phase1 %+% (df2 %>% filter(group == "B"))

c <- df2[1:2,3:4] %>%
    (function(x) apply(x, 1, prod)/(sum(x)^2))

phase1.y <- phase1 + geom_path(col = "#1B9E77", lwd = 1) + 
    scale_x_continuous(limit = c(0, 0.3), name = expression(N[1])) +
    scale_y_continuous(limit = c(0, 0.3), name = expression(N[3])) +
    stat_function(fun = function(x)c[1]/x, alpha = 0.5, lty = 2) +
    annotate("text", x = 0.3, y = 0.3, label = "c = 0.0089", hjust = 0.95) +
    geom_point(aes(x = yini1[[1]], y = yini1[[3]]), size = 1.5) +
    annotate("text", x = yini1[[1]], y = yini1[[3]], label = "Starting point", hjust = -0.1, vjust = -1)
    

y.brk <- c(0.1, 0.4, 0.7, 1)

phase2.y <- phase2 + geom_path(col = "#D95F02", lwd = 1) + 
    scale_x_continuous(limit = c(0.1, 1), name = expression(N[2]), breaks = y.brk) +
    scale_y_continuous(limit = c(0.1, 1), name = expression(N[4]), breaks = y.brk) +
    stat_function(fun = function(x)c[2]/x, alpha = 0.5, lty = 2) +
    annotate("text", x = 1, y = 1, label = "c = 0.1422", hjust = 0.95) +
    geom_point(aes(x = yini1[[2]], y = yini1[[4]]), size = 1.5) +
    annotate("text", x = yini1[[2]], y = yini1[[4]], label = "Starting point", vjust = -1.2)

png(file = "phase.png", height = 6*400, width = 12*400, res = 600)
print(grid.arrange(phase1.y, phase2.y, nrow=1))
dev.off()
