library(deSolve)
library(ggplot2); theme_set(theme_bw())
source("model.R")
source("circulant.R")

alpha <- c(0.25, 0.5, 0.75)
beta <- 1.5

ylist <- list(
    y1 = c(0.1, 0.95, 0.2),
    y2 = c(0.9, 0.7, 0.2)
)

tvec <- seq(0, 200, 0.1)

resList <- vector("list", length = length(alpha))

for(i in 1:length(alpha)){
    pars <- list(
        matvals = c(beta, alpha[i])
    )
    model <- base.model(pars)
    res <- lapply(ylist, function(y){
        r <- as.data.frame(ode(y, tvec, model, pars))
        r[,"alpha"] <- as.character(alpha[i])
        r <- setNames(r, c("time", "N1", "N2", "N3", "alpha"))
    })
    
    all <- do.call("rbind", res)
    all$id <- rep(names(res), sapply(res, nrow))
    
    resList[[i]] <- all
}

df <- do.call("rbind", resList)

g.phase <- ggplot(df) +
    geom_path(aes(N1, N2, group = id)) +
    facet_wrap(~alpha,
        nrow = 3) +
    scale_x_continuous(name = expression(N[1])) + 
    scale_y_continuous(name = expression(N[2])) + 
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.margin = grid::unit(0, "lines"),
        axis.title.y = element_text(angle = 0)
    ) + geom_text(
        data = as.data.frame(alpha),
        aes(x = 1, y = 0.95,
            label = paste("alpha: ", alpha)
        ),
        hjust = 1
    )

ggsave("phase.png", g.phase, width = 4, height = 6, dpi = 600)
