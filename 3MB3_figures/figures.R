library(ggplot2); theme_set(theme_bw())
library(deSolve)
library(tidyr)
library(dplyr)
library(tibble) ## for add_column
library(grid)
library(gridExtra)
library(rgl)
dir <- "../R/"
basefile_names <- list("lhs_y.R", "parameters.R", "circulant.R", "model.R", "period.R")
sapply(basefile_names, function(x) source(paste0(dir, x), .GlobalEnv))

## parameter space
source(paste0(dir, "parameterPlot.R"))
ggsave("parameters.png", g.parameter, width = 6, height = 6, dpi = 600)

## 2D phase plot (hopf birufcation)
source(paste0(dir, "2Dphase.R"))
ggsave("phase.png", g.phase, width = 4, height = 8, dpi = 600)

## sample simulation
source(paste0(dir, "baseSim_4.R"))
ggsave("dynamics.png", g.base, width = 8, height = 6, dpi = 600)
ggsave("hyperbola.png", g.hyperbola, width = 8, height = 4, dpi = 600)

## period plot
load("../data/period.rda")
source(paste0(dir, "periodPlot.R"))
ggsave("period.png", g.period, width = 8, height = 8, dpi = 600)

## 3d figures
rgl.df <- phase.df %>% filter(beta == 0.5)
source(paste0(dir, "rgl.R"))
snapshot3d(file.path("3D_1.png"))

rgl.df <- phase.df %>% filter(beta == 0.75)
source(paste0(dir, "rgl.R"))
snapshot3d(file.path("3D_2.png"))
