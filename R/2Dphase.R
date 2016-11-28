beta <- c(0.25, 0.5, 0.75)

ylist <- list(
    y1 = c(0.1, 0.95, 0.2),
    y2 = c(0.9, 0.7, 0.2)
)

phaseList <- vector("list", length = length(beta))
pars_base <- pars_tc

for(i in 1:length(beta)){
    pars_base$matvals[2] <- beta[i]
    model_tmp <- base.model(pars_base)
    res_tmp <- lapply(ylist, function(y){
        r <- ode(y, tvec_long, model_tmp, pars_base) %>%
            as_data_frame %>%
            add_column(rep(as.character(beta[i]), length(tvec_long))) %>%
            setNames(c("time", "N1", "N2", "N3", "beta"))
    })
    
    phaseList[[i]] <- res_tmp %>% bind_rows(.id = "y")
}

phase.df <- phaseList %>% bind_rows

g.phase <- ggplot(phase.df) +
    geom_path(aes(N1, N2, group = y)) +
    facet_wrap(~beta,
        nrow = 3) +
    scale_x_continuous(name = expression(N[1])) + 
    scale_y_continuous(name = expression(N[2])) + 
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.margin = grid::unit(0, "lines"),
        axis.title.y = element_text(angle = 0)) + 
    geom_text(data = as.data.frame(beta),
        aes(x = 1, y = 0.95, label = paste("beta: ", beta)),
        hjust = 1
    )
