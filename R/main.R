#### IMPORT LIBRARIES ####

library(raster) # for raster manipiulation
library(rgdal) # for loading/writing geospatial data
library(sp) # for vector manipulation
library(rgeos) # for vector operations
library(leastcostpath) # for creating cost surfaces, adding vertical error, calculating least cost paths and probabilistic least cost path
library(tmap) # for producing probabilistic least cost path maps
library(ggplot2) # for producing boxplot graph

#### COORDINATE SYSTEM ####

osgb <- "+init=epsg:27700"

#### PROCESSING SRTM ELEVATION DATA ####

elev_osgb <- raster::raster("./Data/SRTM elevation/Elevation OSGB.tif")

slope = terrain(elev_osgb, opt='slope')
aspect = terrain(elev_osgb, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)

#### PROCESSING HIGH STREET ROMAN ROAD ####

road <- rgdal::readOGR("./Data/scheduled_monuments/High_Street_Roman_Road.shp")

raster::crs(road) <- raster::crs(osgb)

#### ORIGIN AND DESTINATIONS OF LEAST COST PATH ####

A <- sp::SpatialPoints(cbind(349015.607,524430.065))
B <- sp::SpatialPoints(cbind(343051.032,508617.526))

#### PROCESSING WATERBODIES ####

waterbodies <- rgdal::readOGR("./Data/waterbodies/WFD_Lake_Water_Bodies_Cycle_2.shp")

#### CREATE COST SURFACES ####

slope_cs <- leastcostpath::create_slope_cs(elev_osgb, cost_function = "tobler", neighbours = 16)

lcp <- leastcostpath::create_lcp(cost_surface = slope_cs, origin = A, destination = B, directional = FALSE)

writeOGR(obj = lcp[lcp$direction == "A to B",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_A_B_SRTM", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = lcp[lcp$direction == "B to A",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_B_A_SRTM", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#### INCORPORATE VERTICAL ERROR AND CREATE LCPS ####

lcps <- list()

n <- 1000

for (i in 1:n) {
  
  print(i)

lcps[[i]] <- leastcostpath::create_lcp(cost_surface = leastcostpath::create_slope_cs(dem = leastcostpath::add_dem_error(dem = elev_osgb, rmse = 9.73, type = "autocorrelated"), cost_function = "tobler", neighbours = 16), origin = A, destination = B, directional = FALSE, cost_distance = TRUE)

}

lcps <- do.call(rbind, lcps)

writeOGR(obj = lcps[lcps$direction == "A to B",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_A_B_SRTM_RMSE_inc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = lcps[lcps$direction == "B to A",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_B_A_SRTM_RMSE_inc", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#### PRODUCE LEAST COST PATH MAPS ####

lcp_A_B_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_shape(lcp[lcp$direction == "A to B",]) +
  tm_lines(col = "red", border.col = "red") + 
  tm_add_legend(type = "line", labels = "Least Cost Path", col = "red", border.col = "red") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_A_B_map, "./outputs/Plots/lcp_A_to_B.png")

lcp_B_A_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_shape(lcp[lcp$direction == "B to A",]) +
  tm_lines(col = "red", border.col = "red") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "line", labels = "Least Cost Path", col = "red", border.col = "red") + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_B_A_map, "./outputs/Plots/lcp_B_to_A.png")

#### CALCULATE PROBABILISTIC LEAST COST PATHS ####

lcp_A_B_density <- create_lcp_density(lcps[lcps$direction == "A to B",], raster = elev_osgb, rescale = FALSE, rasterize_as_points = FALSE)
lcp_B_A_density <- create_lcp_density(lcps[lcps$direction == "B to A",], raster = elev_osgb, rescale = FALSE, rasterize_as_points = FALSE)

lcp_A_B_density[lcp_A_B_density == 0] <- NA
lcp_B_A_density[lcp_B_A_density == 0] <- NA

lcp_A_B_density <- (lcp_A_B_density / n) 
lcp_B_A_density <- (lcp_B_A_density / n)

raster::writeRaster(x = lcp_A_B_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_A_B_density.tif")
raster::writeRaster(x = lcp_B_A_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_B_A_density.tif")

lcp_A_B_density <- raster::raster("./outputs/Probabilistic Least Cost Paths/lcp_A_B_density.tif")
lcp_B_A_density <- raster::raster("./outputs/Probabilistic Least Cost Paths/lcp_B_A_density.tif")

#### PRODUCE PROBABILISTIC LEAST COST PATH MAPS ####

lcp_A_B_density_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_A_B_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (quantiles)",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_A_B_density_map, "./outputs/Plots/lcp_A_to_B_probability_rmse.png", dpi = 300)

lcp_B_A_density_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_B_A_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (quantiles)",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_B_A_density_map, "./outputs/Plots/lcp_B_to_A_probability_rmse.png", dpi = 300)

#### PRODUCE CLOSE-UP OF PROBABILISTIC LEAST COST PATH MAPS ####

hill_close_up <- raster::crop(hill, as(extent(343290, 348230, 516030, 521280), "SpatialPolygons"))

lcp_A_B_density_close_up_map <- 
  tm_shape(hill_close_up) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_A_B_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (quantiles)",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_A_B_density_close_up_map, "./outputs/Plots/lcp_A_to_B_probability_rmse_close_up.png", dpi = 300)

lcp_B_A_density_close_up_map <- 
  tm_shape(hill_close_up) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_B_A_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (quantiles)",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_B_A_density_close_up_map, "./outputs/Plots/lcp_B_to_A_probability_rmse_close_up.png", dpi = 300)

###### CALCULATING MAXIMUM DISTANCE FROM LCPs TO KNOWN LOCATION OF HIGH STREET ROMAN ROAD #####

lcp_A_B_distance <- max(rgeos::gDistance(spgeom1 = lcp[lcp$direction == "A to B",], SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = crs(osgb)), byid = TRUE))

lcp_B_A_distance <- max(rgeos::gDistance(spgeom1 = lcp[lcp$direction == "B to A",], SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = crs(osgb)), byid = TRUE))

write.csv(lcp_A_B_distance, "./outputs/Statistical assessments/lcp_A_B_max_distance.csv")
write.csv(lcp_B_A_distance, "./outputs/Statistical assessments/lcp_B_A_max_distance.csv")

lcp_A_B_RMSE_distance <- list()
lcp_B_A_RMSE_distance <- list()

for(i in 1:(nrow(lcps) / 2)) {
  
  lcp_A_B_RMSE_distance[[i]] <- max(rgeos::gDistance(spgeom1 = lcps[lcps$direction == "A to B",][i,], sp::SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = raster::crs(osgb)), byid = TRUE))
  
  lcp_B_A_RMSE_distance[[i]] <- max(rgeos::gDistance(spgeom1 = lcps[lcps$direction == "B to A",][i,], sp::SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = raster::crs(osgb)), byid = TRUE))
}

lcp_A_B_RMSE_distance <- unlist(lcp_A_B_RMSE_distance)
lcp_B_A_RMSE_distance <- unlist(lcp_B_A_RMSE_distance)

write.csv(lcp_A_B_RMSE_distance, "./outputs/Statistical assessments/MC_1000_lcp_A_B_max_distance.csv")
write.csv(lcp_B_A_RMSE_distance, "./outputs/Statistical assessments/MC_1000_lcp_B_A_max_distance.csv")

#### STATISTICAL COMPARISON OF MAXIMUM DISTANCE DISTRIBUTIONS ####

welch_t_test <- t.test(lcp_A_B_RMSE_distance, lcp_B_A_RMSE_distance)

write.csv(welch_t_test$p.value, "./outputs/Statistical assessments/welch_t_test_p_value.csv")

#### BOXPLOT COMPARING MAXIMUM DISTANCE DISTRIBUTIONS ####

lcp_distances <- rbind(data.frame(id = "North to South", distance = lcp_A_B_RMSE_distance), data.frame(id = "South to North", distance = lcp_B_A_RMSE_distance))

lcp_distances$id <- factor(lcp_distances$id,levels = c("South to North", "North to South"))

lcp_box_plot <- ggplot(lcp_distances, aes(x=factor(id), y=distance) ) + 
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(id)), alpha=0.9,show.legend = FALSE) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE, aes(colour=factor(id))) + 
  coord_flip() + 
  theme_minimal(base_size = 24) + 
  labs(x = "Least Cost Path Direction", y = "Maximum Distance from Least Cost Paths\nto High Street Roman Road (metres)")

ggsave(filename = "./outputs/plots/lcp_box_plot.png", plot = lcp_box_plot, dpi = 300, width = 15, height = 7)
