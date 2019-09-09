library(sf)
library(mapview)

library(spData)
us_states2163 <- st_union(st_transform(us_states, 2163))
coordinates(good.sites) <- ~ Lon + Lat
#a function to select N points on a raster, with
#inclusion probabilities defined by the raster values.
probsel <- function(probrast, N) {
  x <- getValues(probrast)
  #set NA cells in raster to zero
  x[is.na(x)] <- 0
  samp <- sample(nrow(probrast) * ncol(probrast),
                 size = N,
                 prob = x)
  samprast <- raster(probrast)
  samprast[samp] <- 1
  #set value of sampled squares to 1
  points <- rasterToPoints(
    samprast,
    fun = function(x) {
      x > 0
    }
  )
  points <- SpatialPoints(points)
  return(points)
}
r<-readRDS('RasteroutProduct_AGB_Weighted.rds')
pr1 <- projectRaster(r, crs="+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
plot(pr1)
aoi_boundary_HARV <- sf::st_read("/fs/data3/hamzed/Projects/ReadingGeojson/l2.json")
aoi_boundary_HARV<-as(aoi_boundary_HARV, "Spatial")
proj4string(aoi_boundary_HARV) <-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
Eco <- spTransform(aoi_boundary_HARV,CRS("+proj=longlat +datum=WGS84"))
#saveRDS(rr ,"ClipedOutProductRaster.rds")
rr <- raster::mask(pr1, as(us_states2163, "Spatial"))
rr[rr%in%(raster::values(rr) %>% boxplot(plot=F,range=2))$out]<-NA
rr2 <- rr
raster::values(rr2)<-raster::values(rr2) %>% scales::rescale()
#saveRDS(rr2, "rr2_AGB.rds")

samppoints<-probsel(projectRaster(rr2, crs="+proj=longlat +datum=WGS84"),500)


m1 <-mapview::mapview(rr2_AGB, alpha=1, na.color="transparent",
                      col.regions=rev(terrain.colors(1256)))+
  mapview(Eco, alpha.regions =0.05)

m1

m2 <-mapview::mapview(rr2, alpha=0.75, na.color="transparent",
                 col.regions=rev(terrain.colors(1256)))+mapview(samppoints)



m3 <-mapview::mapview(rr2, alpha=0.5, na.color="transparent",
                      col.regions=rev(terrain.colors(1256)))+ mapview(good.sites)

m4 <-mapview::mapview(rr2, alpha=0.55, na.color="transparent",
                      col.regions=rev(terrain.colors(1256)))

sync(m1, m2, m4)







