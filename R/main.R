#### IMPORT LIBRARIES ####

library(raster) # for raster manipiulation
library(rgdal) # for loading/writing geospatial data
library(sp) # for vector manipulation
library(rgeos) # for vector operations
library(leastcostpath) # for creating cost surfaces, adding vertical error, calculating least cost paths and probabilistic least cost path
library(tmap) # for producing probabilistic least cost path maps
library(ggplot2) # for producing histograms graph
library(DMMF) # for digital elevation model sink fills

#### CONSTANTS ####

# Number of iterations to run when propogating vertical error on Least Cost Path results
n <- 1000

# Vertical error (Root Mean Square Error) of Digital Elevation Model
RMSE <- 4.0

# Seed set for reproducibility
set.seed(76418)

#### COORDINATE SYSTEM ####

osgb <- "+init=epsg:27700"

#### PRCOESSING SRTM ELEVATION DATA ####

elev_files <- list.files(path = "./Data/OS Terrain 50/",  pattern = "\\.asc$", full.names = TRUE)
elev_list <- lapply(elev_files, raster::raster)
elev_osgb <- do.call(raster::merge, elev_list)
elev_osgb <- raster::crop(elev_osgb, c(341940, 349640, 508130, 524940))
raster::crs(elev_osgb) <- raster::crs(osgb)

#### PROCESSING HIGH STREET ROMAN ROAD ####

road <- rgdal::readOGR("./Data/scheduled_monuments/High_Street_Roman_Road.shp")

# ensure coordinate reference system string is the same - no need to project
raster::crs(road) <- raster::crs(osgb)

#### ORIGIN AND DESTINATIONS OF LEAST COST PATH - BASED ON NORTHERN AND SOUTHERN ENDS OF HIGH STREET ROMAN ROAD####

# A is the northern point of the Roman road; B is the southern point of the Roman road
A <- sp::SpatialPoints(cbind(349015.607,524430.065))
B <- sp::SpatialPoints(cbind(343051.032,508617.526))

#### PROCESSING WATERBODIES ####

# waterbodies to add to plots
waterbodies <- rgdal::readOGR("./Data/waterbodies/WFD_Lake_Water_Bodies_Cycle_2.shp")

#### CREATE COST SURFACES ####

# cost surface using Tobler's Hiking Function and 16 adjacent cells
slope_cs <- leastcostpath::create_slope_cs(elev_osgb, cost_function = "tobler", neighbours = 16)

# Least Cost Path from A to B and B to A (due to directional = FALSE)
lcp <- leastcostpath::create_lcp(cost_surface = slope_cs, origin = A, destination = B, directional = FALSE)

#writeOGR(obj = lcp[lcp$direction == "A to B",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_A_B_OS_50", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(obj = lcp[lcp$direction == "B to A",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_B_A_OS_50", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#### INCORPORATE VERTICAL ERROR AND CREATE LCPS ####

# create empty list to be filled with Least Cost Paths
lcps <- list()

for (i in 1:n) {

  print(i)

  # Create Least Cost Path based on sink-fill-corrected DEM with a random error field representing the vertical error added. Add to lcps list

lcps[[i]] <- leastcostpath::create_lcp(cost_surface = leastcostpath::create_slope_cs(dem = DMMF::SinkFill(DEM = leastcostpath::add_dem_error(dem = elev_osgb, rmse = RMSE, type = "autocorrelated"))$DEM_nosink, cost_function = "tobler", neighbours = 16), origin = A, destination = B, directional = FALSE, cost_distance = TRUE)

}

# bind list of Least Cost Paths to one object
lcps <- do.call(rbind, lcps)

#writeOGR(obj = lcps[lcps$direction == "A to B",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_A_B_OS_50_RMSE_inc", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(obj = lcps[lcps$direction == "B to A",], dsn = "./Outputs/Least Cost Paths", layer = "LCP_B_A_OS_50_RMSE_inc", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#### PRODUCE LEAST COST PATH MAPS ####

lcp_A_B_map <- 
  tm_shape(elev_osgb) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, title = "Elevation (m)") + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_shape(lcp[lcp$direction == "A to B",]) +
  tm_lines(col = "red") + 
  tm_add_legend(type = "line", labels = "Least Cost Path", col = "red", border.col = "red") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1") +
  tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 2) + 
  tm_scale_bar(width = 0.01, position = c("right", "bottom"), breaks = c(0, 2, 4))

#tmap::save_tmap(lcp_A_B_map, "./outputs/Plots/lcp_A_to_B.png")

lcp_B_A_map <- 
  tm_shape(elev_osgb) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, title = "Elevation (m)") + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_shape(lcp[lcp$direction == "B to A",]) +
  tm_lines(col = "red") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "line", labels = "Least Cost Path", col = "red", border.col = "red") + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1") +
  tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 2) + 
  tm_scale_bar(width = 0.01, position = c("right", "bottom"), breaks = c(0, 2, 4))

#tmap::save_tmap(lcp_B_A_map, "./outputs/Plots/lcp_B_to_A.png")

#### CALCULATE PROBABILISTIC LEAST COST PATHS ####

# Create Least Cost Path density rasters using the Least Cost Paths incorporating vertical error
lcp_A_B_density <- leastcostpath::create_lcp_density(lcps[lcps$direction == "A to B",], raster = elev_osgb, rescale = FALSE, rasterize_as_points = FALSE)
lcp_B_A_density <- leastcostpath::create_lcp_density(lcps[lcps$direction == "B to A",], raster = elev_osgb, rescale = FALSE, rasterize_as_points = FALSE)

lcp_A_B_density[lcp_A_B_density == 0] <- NA
lcp_B_A_density[lcp_B_A_density == 0] <- NA

# Convert Least Cost Path density raster to a probability raster denoting the probability that an LCP crosses a cell
lcp_A_B_density <- (lcp_A_B_density / n)
lcp_B_A_density <- (lcp_B_A_density / n)

#raster::writeRaster(x = lcp_A_B_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_A_B_density.tif", overwrite=TRUE)
#raster::writeRaster(x = lcp_B_A_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_B_A_density.tif", overwrite=TRUE)

#### PRODUCE PROBABILISTIC LEAST COST PATH MAPS ####

lcp_A_B_density_map <- 
  tm_shape(elev_osgb) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, title = "Elevation (m)") + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_A_B_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1") +
  tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 2) + 
  tm_scale_bar(width = 0.01, position = c("right", "bottom"), breaks = c(0, 2, 4))

#tmap::save_tmap(lcp_A_B_density_map, "./outputs/Plots/lcp_A_to_B_probability_rmse.png", dpi = 300)

lcp_B_A_density_map <- 
  tm_shape(elev_osgb) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, title = "Elevation (m)") + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_B_A_density) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1") +
  tm_compass(type="arrow", position=c("right", "bottom"), show.labels = 2) + 
  tm_scale_bar(width = 0.01, position = c("right", "bottom"), breaks = c(0, 2, 4))

#tmap::save_tmap(lcp_B_A_density_map, "./outputs/Plots/lcp_B_to_A_probability_rmse.png", dpi = 300)

###### CALCULATING MAXIMUM DISTANCE FROM LCPs TO KNOWN LOCATION OF HIGH STREET ROMAN ROAD #####

lcp_A_B_distance <- max(rgeos::gDistance(spgeom1 = lcp[lcp$direction == "A to B",], SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = crs(osgb)), byid = TRUE))

lcp_B_A_distance <- max(rgeos::gDistance(spgeom1 = lcp[lcp$direction == "B to A",], SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = crs(osgb)), byid = TRUE))

#write.csv(lcp_A_B_distance, "./outputs/Statistical assessments/lcp_A_B_max_distance.csv")
#write.csv(lcp_B_A_distance, "./outputs/Statistical assessments/lcp_B_A_max_distance.csv")

lcp_A_B_RMSE_distance <- list()
lcp_B_A_RMSE_distance <- list()

for(i in 1:(nrow(lcps) / 2)) {
  
  lcp_A_B_RMSE_distance[[i]] <- max(rgeos::gDistance(spgeom1 = lcps[lcps$direction == "A to B",][i,], sp::SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = raster::crs(osgb)), byid = TRUE))
  
  lcp_B_A_RMSE_distance[[i]] <- max(rgeos::gDistance(spgeom1 = lcps[lcps$direction == "B to A",][i,], sp::SpatialPoints(road@polygons[[1]]@Polygons[[1]]@coords, proj4string = raster::crs(osgb)), byid = TRUE))

}

lcp_A_B_RMSE_distance <- unlist(lcp_A_B_RMSE_distance)
lcp_B_A_RMSE_distance <- unlist(lcp_B_A_RMSE_distance)

lcp_A_B_RMSE_distance <- data.frame(ID = "A", max_distance = lcp_A_B_RMSE_distance)
lcp_B_A_RMSE_distance <- data.frame(ID = "B", max_distance = lcp_B_A_RMSE_distance)

#write.csv(lcp_A_B_RMSE_distance, "./outputs/Statistical assessments/MC_1000_lcp_A_B_max_distance.csv")
#write.csv(lcp_B_A_RMSE_distance, "./outputs/Statistical assessments/MC_1000_lcp_B_A_max_distance.csv")

lcp_rmse_distances <- rbind(lcp_A_B_RMSE_distance, lcp_B_A_RMSE_distance)

lcp_distance_plot <- ggplot(data = lcp_rmse_distances, aes(x = max_distance)) + 
  geom_density(aes(fill= ID, colour = ID), size = 1, alpha = 0.5, show.legend = FALSE) + 
  facet_wrap(~ID) + 
  geom_vline(data = data.frame(ID = "A", lcp_A_B_distance), aes(xintercept = lcp_A_B_distance), colour = "black", linetype = "dashed", size = 1.2) + 
  geom_vline(data = data.frame(ID = "B", lcp_B_A_distance), aes(xintercept = lcp_B_A_distance), colour = "black", linetype = "dashed", size = 1.2) + 
  xlim(c(0, 1500)) + 
  theme_minimal(base_size = 24) + 
  labs(x = "Maximum straight-line distance from\nLCP to High Street Roman Road (metres)", y = "Density of Least Cost Paths") + 
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) +
  scale_colour_manual(values=c("#00BFC4", "#F8766D")) + 
  geom_text(data=data.frame(ID = "A", distance = lcp_A_B_distance), 
            aes(label= paste0(round(distance,2), "m"), x= 600, y=0.0045), colour="black", size = 8) + 
  geom_text(data=data.frame(ID = "B", distance = lcp_B_A_distance), 
            aes(label= paste0(round(distance,2), "m"), x= 450, y=0.0045), colour="black", size = 8) + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0))

#ggsave(filename = "./outputs/plots/lcp_distance_plot.png", plot = lcp_distance_plot, dpi = 300, width = 14, height = 7)

###### MONTE CARLO HYPOTHESIS TESTING #####

MC_HT_A_B <- round((sum(lcp_A_B_distance >= lcp_A_B_RMSE_distance$max_distance) / 1001), 3)

MC_HT_B_A <- round((sum(lcp_B_A_distance >= lcp_B_A_RMSE_distance$max_distance) / 1001), 3)

#write.csv(MC_HT_A_B, "./outputs/Statistical assessments/MC_HT_A_B.csv")
#write.csv(MC_HT_B_A, "./outputs/Statistical assessments/MC_HT_B_A.csv")

