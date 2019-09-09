library(sp)
library(tidyverse)
library(sf)
library(raster)
library(RColorBrewer)
#> Linking to GEOS 3.6.1, GDAL 2.2.4, proj.4 4.9.3
library(spData)
us_states2163 <- st_union(st_transform(us_states, 2163))
setwd("/fs/data3/hamzed/Projects/SDAInspection")
voronoipolygons <- function(layer) {
  require(deldir)
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                         y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                      function(x) slot(x, 'ID'))))
}

load('sda.output.Rdata')

settings<-Viz.output[[1]]
var.names <- sapply(settings[[1]]$state.data.assimilation$state.variable, '[[', "variable.name")
all.years <- lubridate::year(names(Viz.output[[2]]))
site.ids <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
#------------------------------------ OBS
site.loc<-settings %>% 
  purrr::map('run') %>%
  purrr::map('site') %>% 
  purrr::map_dfr(~ data.frame(Lat=.x$lat %>% as.numeric() , 
                              Lon=.x$lon %>% as.numeric(), 
                              Id=.x$id %>% as.numeric()))


site.loc.sp <- site.loc 
coordinates(site.loc.sp) <- c("Lon", "Lat")
proj4string(site.loc.sp) <- CRS("+proj=longlat +datum=WGS84")  ## for example

# Making Voronoi
vp <-voronoipolygons(site.loc.sp)
vp@data <- vp@data %>%
  dplyr::left_join(site.loc,
                   by=c("x"="Lon", "y"="Lat"))

# Clip
v <-st_as_sf(vp)
st_crs(v) <- 4326
#plot(v)




#-- preparing data
Data_binding <-enkf.params %>%
  map(function(YD){
    var.names %>%
      map_dfr(function(vari){
     
        cbind(
          (YD[[1]])[grepl(vari, rownames(YD[[3]]))],
          (YD[[3]])[grepl(vari, rownames(YD[[3]]))],
          attr(YD[[3]], 'Site')[grepl(vari, rownames(YD[[3]]))]
        ) %>% 
          as.data.frame() %>%
          `colnames<-`(c('mu.f','mu.a',"Id")) %>%
          mutate(Var=vari,
                 Id=as.numeric(as.character(Id))) %>%
          gather(Stat, Value, -c(Id, Var))

      })
  })

names(Data_binding) <- all.years[seq_along(Data_binding)]



#---- plotting

var.plot <- var.names[2]

all.plots.print<-all.years%>%
  map(possibly(function(year){
    ss <- (crop(vp,as(us_states, "Spatial"))) 
    year <- as.character(year)
    #Preparing Obs
    Obs<-(Viz.output[[2]])[which(all.years == year)] %>%
      map_dfr(function(.x){
        
        data.frame(Value=unlist(.x), Id=as.numeric(names(.x)), Stat="Obs") %>%
          `row.names<-`(c())
      })
    
    band <- ANALYSIS %>%
      purrr::map(function(state.vars) {
        #finding the mean and Ci for all the state variables
        ops <- site.ids %>% unique() %>%
          map_df(function(site) {
            (state.vars)[, which(attr(FORECAST[[1]], 'Site')  %in% site)] %>%
              dplyr::select(var.plot) %>%
              as.data.frame %>%
              `colnames<-`(c(var.plot)) %>%
              mutate(Id = site %>% as.numeric())
          }) %>%
          tidyr::gather(Variable, Value,-c(Id)) %>%
          group_by(Id, Variable) %>%
          summarise(sd = sd(Value, na.rm = T) * 2) %>%
          dplyr::select(-Variable) %>%
          mutate(Stat="mu.aBand")
        
      })%>%
      setNames(all.years[seq_along(Data_binding)])
    
    
    ss@data<-ss@data %>%
      left_join(Data_binding[[year %>% as.character()]]%>%
                  mutate(Value = as.numeric(Value)) %>%
                  filter(Var==var.plot) %>% 
                  dplyr::select(-Var) %>%
                  bind_rows(Obs) %>%
                  bind_rows(band[[year %>% as.character]] %>%
                              dplyr::select(Id, Value=sd, Stat))
      )
    
    
    
    
    ss <-st_as_sf(ss)
    st_crs(ss) <- 4326
    read.layer <-st_transform(ss,
                              st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")) 
    
    read.layer$Value[read.layer$Value>500] <- NA
    read.layer$Stat[read.layer$Stat=="mu.f"] <- "FORECAST"
    read.layer$Stat[read.layer$Stat=="Obs"] <- paste0("OBSERVED - ", toupper(var.names[1]))
    read.layer$Stat[read.layer$Stat=="mu.a"] <- "ANALYSIS"
    read.layer$Stat[read.layer$Stat=="mu.aBand"] <- "ANALYSIS UNCERTAINTY"
    
    p<-ggplot()+
      geom_sf(data=read.layer, aes(fill=Value), lwd=.05, color="#5d5e60")+
      facet_wrap(~Stat, ncol=2)+
      scale_fill_distiller(palette = "RdYlGn", direction = 1,
                           name="", na.value = "white",
                           limits=c(-1,100))+
      theme_minimal(base_size = 15)+
      labs(title=paste(year,"-", var.plot))+
      theme(legend.position = "top",
            legend.key.width=unit(3,"cm"),
            legend.key.height=unit(1.01,"cm"),
            legend.text = element_text(size=15),
            plot.title = element_text(size=25),
            strip.text = element_text(size=15),
            panel.spacing.x = unit(5, "lines")
      )
    return(list(p, read.layer))
    
  },NULL))


all.plots.print[[1]]

#ggsave("SDA.png", width = 14, height = 10)

pdf(paste0("SDA_MAP_21_twoState_localization_",var.plot,".pdf"),width = 14, height = 10)
all.plots.print %>% map(~.x[[1]])
dev.off()

#--------------------------------------------------------- MAPView
library(mapview)
all.plots.print[[1]][[2]] %>%
  spread(Stat, Value, c(x, y)) %>%
  mapview(zcol=c('mu.a','mu.aBand','mu.f','Obs'),popup = popupTable(.,
                                          zcol = c("Id")), 
          label = all.plots.print[[1]][[2]]$Id)


site <- '1000026677' 
Viz.output[[2]]%>%
  map2(Viz.output[[3]], ~ c(.x[site]%>% unlist(),
                            .y[site] %>% unlist())
       )

