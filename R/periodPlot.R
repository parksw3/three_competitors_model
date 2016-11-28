y <- lhs_y(range = c(0.1, 1), n.state = 4, n.sim = 1000, seed = 101)

period <- lapply(LHSres, function(x){
    getPeriod(x[,2])
}) %>% unlist

min.period <- min(period)

period.df <- LHSres %>%
    bind_rows(.id = "sim")

period.df$period <- rep(period, each = length(tvec))

period.df.ini <- period.df %>%
    filter(time == 0) %>%
    select(-c(sim, time)) 

g.period <- ggplot(period.df.ini) +
        geom_point(aes((N1 * N2 * N3 * N4)/(N1 + N2 + N3 + N4)^4, period), size = 0.8) + 
        scale_x_log10(name = expression(N[1]~N[2]~N[3]~N[4]/(N[1]+N[2]+N[3]+N[4])^4), breaks = c(0.0005,0.001, 0.002, 0.004)) +
        theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = "in"),
            axis.title.y = element_text(margin=margin(0,15,0,0)),
            axis.title.x = element_text(margin=margin(10,0,0,0))
        )