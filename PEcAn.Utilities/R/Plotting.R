library(PEcAn.assim.sequential)
library(tidyverse)
library(sp)
library(sf)
library(rgdal)
load(file.choose()) ## choose your SDA output

post.analysis.multisite.ggplot(settings=Viz.output[[1]],
                               t=t,
                               obs.times=names(Viz.output[[2]]) %>%
                                 as.POSIXct(),
                               obs.mean=Viz.output[[2]],
                               obs.cov=Viz.output[[3]],
                               FORECAST=FORECAST,
                               ANALYSIS=ANALYSIS,
                               plot.title = "test",
                               facetg = T)
