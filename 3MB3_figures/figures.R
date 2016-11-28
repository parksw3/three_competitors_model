library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(dplyr)
library(tibble) ## for add_column
dir <- "../R/"
basefile_names <- list("lhs_y.R", "parameters.R", "circulant.R", "model.R", "period.R")
sapply(basefile_names, function(x) source(paste0(dir, x), .GlobalEnv))

## parameter space
source(paste0(dir, "parameterPlot.R"))
ggsave("parameters.png", g.parameter, width = 6, height = 6, dpi = 600)

## 2D phase plot (hopf birufcation)
source(paste0(dir, "2Dphase.R"))
ggsave("phase.png", g.phase, width = 4, height = 6, dpi = 600)

## period plot
load("../data/period.rda")
source(paste0(dir, "periodPlot.R"))
ggsave("period.png", g.period, width = 8, height = 8, dpi = 600)