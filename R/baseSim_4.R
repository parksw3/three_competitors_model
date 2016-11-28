scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)

yini_base <- c(0.1, 0.8, 0.2, 0.4)
model <- base.model(pars)

r <- ode(yini_base, tvec, model, pars)

base.df <- r %>%
    as.data.frame %>%
    gather(key, value, -time)

g.base.labs <- list(bquote(N[1]), bquote(N[2]), bquote(N[3]), bquote(N[4]))

g.base <- ggplot(base.df, aes(time, value, col = key)) +
        geom_line(lwd = 1) +
        scale_x_continuous(expand = c(0, 0), name = "Time") +
        scale_y_continuous(expand = c(0, 0), name = NULL, limits = c(0, 0.8)) +
        scale_colour_discrete(
            name = NULL,
            labels = g.base.labs) +  
        theme(plot.margin = unit(c(0.3, 0, 0.1, 0.2), units = "in"))

base.df2 <- base.df %>%
    mutate(group = ifelse(key %in%  c("1", "3"), "A", "B")) %>%
    mutate(key = ifelse(key %in% c("1", "2"), "x", "y")) %>%
    spread(key, value)

hyperbola1 <- base.df2 %>%
    filter(group == "A") %>%
    ggplot(aes(x, y)) +
        theme(axis.title.y = element_text(angle = 0),
              plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = "in")) +
        coord_fixed(ratio = 1)

hyperbola2 <- hyperbola1 %+% (base.df2 %>% filter(group == "B"))

c_vals <- base.df2[1:2,3:4] %>% {apply(., 1, prod)/(sum(.)^2)}

hyperbola1.y <- hyperbola1 + geom_path(col = "#1B9E77", lwd = 1) + 
    scale_x_continuous(limit = c(0, 0.3), name = expression(N[1])) +
    scale_y_continuous(limit = c(0, 0.3), name = expression(N[3])) +
    stat_function(fun = function(x)c_vals[1]/x, alpha = 0.5, lty = 2) +
    geom_point(aes(x = yini_base[[1]], y = yini_base[[3]]), size = 1.5) +
    annotate("text", x = yini_base[[1]], y = yini_base[[3]], label = "Starting point", hjust = -0.1, vjust = -1)
    

y.brk <- c(0.1, 0.4, 0.7, 1)

hyperbola2.y <- hyperbola2 + geom_path(col = "#D95F02", lwd = 1) + 
    scale_x_continuous(limit = c(0.1, 1), name = expression(N[2]), breaks = y.brk) +
    scale_y_continuous(limit = c(0.1, 1), name = expression(N[4]), breaks = y.brk) +
    stat_function(fun = function(x)c_vals[2]/x, alpha = 0.5, lty = 2) +
    geom_point(aes(x = yini_base[[2]], y = yini_base[[4]]), size = 1.5) +
    annotate("text", x = yini_base[[2]], y = yini_base[[4]], label = "Starting point", vjust = -1.2)

g.hyperbola <- arrangeGrob(hyperbola1.y, hyperbola2.y, nrow=1)
