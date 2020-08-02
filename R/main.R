#### IMPORT LIBRARIES ####

library(raster) # for raster manipiulation
library(rgdal) # for loading/writing geospatial data
library(sp) # for vector manipulation
library(rgeos) # for vector operations
library(leastcostpath) # for creating cost surfaces, adding vertical error, calculating least cost paths and probabilistic least cost path
library(tmap) # for producing probabilistic least cost path maps
library(ggplot2) # for producing histograms graph

#### COORDINATE SYSTEM ####

osgb <- "+init=epsg:27700"

#### PRCOESSING SRTM ELEVATION DATA ####

elev_osgb <- raster("./Data/SRTM elevation/Elevation OSGB.tif")

raster::crs(elev_osgb) <- raster::crs(osgb)

slope = terrain(elev_osgb, opt='slope')
aspect = terrain(elev_osgb, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)

#### PROCESSING HIGH STREET ROMAN ROAD ####

road <- rgdal::readOGR("./Data/scheduled_monuments/High_Street_Roman_Road.shp")

raster::crs(road) <- raster::crs(osgb)

#### ORIGIN AND DESTINATIONS OF LEAST COST PATH - BASED ON NORTHERN AND SOUTHERN ENDS OF HIGH STREET ROMAN ROAD####

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

set.seed(76418)

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

raster::writeRaster(x = lcp_A_B_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_A_B_density.tif", overwrite=TRUE)
raster::writeRaster(x = lcp_B_A_density, filename = "./outputs/Probabilistic Least Cost Paths/lcp_B_A_density.tif", overwrite=TRUE)

#### PRODUCE PROBABILISTIC LEAST COST PATH MAPS ####

lcp_A_B_density_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_A_B_density * 100) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (percentile)",
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
  tm_shape(lcp_B_A_density * 100) + 
  tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell (percentile)",
            palette = viridis::plasma(10),
            legend.hist = FALSE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_B_A_density_map, "./outputs/Plots/lcp_B_to_A_probability_rmse.png", dpi = 300)

#### PRODUCE 80th percentile PROBABILISTIC LEAST COST PATH MAPS ####

lcp_A_B_density_80th_percentile <- lcp_A_B_density >= quantile(lcp_A_B_density, 0.8)

lcp_A_B_density_80th_percentile_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_A_B_density_80th_percentile) + 
  tm_raster(n = 2, title = "Probability of Least\nCost Path crossing\ncell >= 80th percentile",
            palette = c("#d7191c", "#1a9641"),
            legend.hist = FALSE, legend.reverse = TRUE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_A_B_density_80th_percentile_map, "./outputs/Plots/lcp_A_to_B_probability_rmse_80th_percentile.png", dpi = 300)

lcp_B_A_density_80th_percentile <- lcp_B_A_density >= quantile(lcp_B_A_density, 0.8)

lcp_B_A_density_80th_percentile_map <- 
  tm_shape(hill) + 
  tm_raster(palette = gray.colors(10, start = 0, end = 1), legend.show = FALSE) + 
  tm_shape(waterbodies) + 
  tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + 
  tm_shape(lcp_B_A_density_80th_percentile) + 
  tm_raster(n = 2, title = "Probability of Least\nCost Path crossing\ncell >= 80th percentile",
            palette = c("#d7191c", "#1a9641"),
            legend.hist = FALSE, legend.reverse = TRUE) +
  tm_shape(road) +
  tm_polygons(col = "black", border.col = "black") + 
  tm_legend(show = TRUE, outside = TRUE, legend.position = c("right","bottom")) + 
  tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + 
  tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1")

tmap_save(lcp_B_A_density_80th_percentile_map, "./outputs/Plots/lcp_B_to_A_probability_rmse_80th_percentile.png", dpi = 300)

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

#### STATISTICAL ASSESSMENTS OF MAXIMUM DISTANCES - MONTE CARLO SIMULATION CONVERGENCE ####

lcp_A_B_RMSE_distance <- data.frame("max_distance" = lcp_A_B_RMSE_distance)
lcp_B_A_RMSE_distance <- data.frame("max_distance" = lcp_B_A_RMSE_distance)

lcp_A_B_RMSE_distance$id <- 1:n
lcp_B_A_RMSE_distance$id <- 1:n

for (i in 1:n) { 
  
  lcp_A_B_RMSE_distance$mean[i] <- mean(lcp_A_B_RMSE_distance$max_distance[1:i]) 
  lcp_B_A_RMSE_distance$mean[i] <- mean(lcp_B_A_RMSE_distance$max_distance[1:i])
  
  lcp_A_B_RMSE_distance$sd[i] <- sd(lcp_A_B_RMSE_distance$max_distance[1:i])
  lcp_B_A_RMSE_distance$sd[i] <- sd(lcp_B_A_RMSE_distance$max_distance[1:i])
  
}

lcp_A_B_RMSE_distance$error <- qnorm(0.975) * lcp_A_B_RMSE_distance$sd / sqrt(lcp_A_B_RMSE_distance$id)
lcp_B_A_RMSE_distance$error <- qnorm(0.975) * lcp_B_A_RMSE_distance$sd / sqrt(lcp_B_A_RMSE_distance$id)

lcp_A_B_RMSE_distance$ci_width <- lcp_A_B_RMSE_distance$error * 2
lcp_B_A_RMSE_distance$ci_width <- lcp_B_A_RMSE_distance$error * 2

lcp_A_B_RMSE_distance$below_95 <- lcp_A_B_RMSE_distance$mean - lcp_A_B_RMSE_distance$error
lcp_B_A_RMSE_distance$below_95 <- lcp_B_A_RMSE_distance$mean - lcp_B_A_RMSE_distance$error

lcp_A_B_RMSE_distance$above_95 <- lcp_A_B_RMSE_distance$mean + lcp_A_B_RMSE_distance$error
lcp_B_A_RMSE_distance$above_95 <- lcp_B_A_RMSE_distance$mean + lcp_B_A_RMSE_distance$error

lcp_A_B_RMSE_distance$convergence <- lcp_A_B_RMSE_distance$ci_width < min(res(elev_osgb))
lcp_B_A_RMSE_distance$convergence <- lcp_B_A_RMSE_distance$ci_width < min(res(elev_osgb))

lcp_A_B_RMSE_distance$plot <- "A"
lcp_B_A_RMSE_distance$plot <- "B"

lcp_convergence <- rbind(lcp_A_B_RMSE_distance, lcp_B_A_RMSE_distance)

write.csv(x = lcp_convergence, file = "./outputs/Statistical assessments/lcp_RMSE_convergence.csv")

LCP_RMSE_convergence <- ggplot(data=lcp_convergence) +
  facet_wrap(~plot) + 
  geom_ribbon(aes(x = id, ymin=below_95, ymax=above_95, fill = convergence), alpha=0.5, show.legend = FALSE) + 
  geom_line(aes(x=id, y=mean), colour = "black", lwd = 1, lty = 2) + 
  scale_fill_manual( values = c("#d7191c", "#1a9641")) + 
  labs(x = "Number of Iterations", y = "Mean-Maximum Distance from Least Cost\nPaths to High Street Roman road (metres)", fill = "Converged") +
  scale_x_continuous( breaks = seq(0, 1000, 100)) +
  ylim(600, 1000) + 
  theme_minimal(base_size = 24) + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0), axis.text.x = element_text(angle = 45))

ggsave(filename = "./outputs/plots/LCP_RMSE_convergence.png", plot = LCP_RMSE_convergence, dpi = 300, width = 14,height = 7)

#### STATISTICAL ASSESSMENTS OF MAXIMUM DISTANCES - Z SCORES ####

lcp_A_B_RMSE_distance_z <- base::scale(lcp_A_B_RMSE_distance$max_distance, center= TRUE, scale=TRUE)
lcp_B_A_RMSE_distance_z <- base::scale(lcp_B_A_RMSE_distance$max_distance, center= TRUE, scale=TRUE)

lcp_rmse_distances_z <- rbind(data.frame(id = "A", distance = lcp_A_B_RMSE_distance_z), data.frame(id = "B", distance = lcp_B_A_RMSE_distance_z))

write.csv(x = (lcp_A_B_distance - mean(lcp_A_B_RMSE_distance$max_distance)) / sd(lcp_A_B_RMSE_distance$max_distance), file = "./outputs/Statistical assessments/lcp_A_B_z_score.csv")
write.csv(x = (lcp_B_A_distance - mean(lcp_B_A_RMSE_distance$max_distance)) / sd(lcp_B_A_RMSE_distance$max_distance), file = "./outputs/Statistical assessments/lcp_B_A_z_score.csv")

write.csv(x = stats::pnorm((lcp_B_A_distance - mean(lcp_B_A_RMSE_distance$max_distance)) / sd(lcp_B_A_RMSE_distance$max_distance)), file = "./outputs/Statistical assessments/lcp_B_A_probability.csv")

lcp_z_score_plot <- ggplot(data = lcp_rmse_distances_z, aes(x = distance)) + 
  geom_histogram(aes(fill= id, colour = id), size = 1, alpha = 0.5, bins = 50, show.legend = FALSE) + 
  facet_wrap(~id) + 
  geom_vline(data = data.frame(id = "A", z_score = (lcp_A_B_distance - mean(lcp_A_B_RMSE_distance$max_distance)) / sd(lcp_A_B_RMSE_distance$max_distance)), aes(xintercept = z_score), colour = "black", linetype = "dashed", size = 1.2) + 
  geom_vline(data = data.frame(id = "B", z_score = (lcp_B_A_distance - mean(lcp_B_A_RMSE_distance$max_distance)) / sd(lcp_B_A_RMSE_distance$max_distance)), aes(xintercept = z_score), colour = "black", linetype = "dashed", size = 1.2) + 
  xlim(c(-5, 5)) + 
  theme_minimal(base_size = 24) + 
  labs(x = "Z score", y = "Number of Least Cost Paths") + 
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) +
  scale_colour_manual(values=c("#00BFC4", "#F8766D")) + 
  geom_text(data=rbind(data.frame(id = "A", z_score = (lcp_A_B_distance - mean(lcp_A_B_RMSE_distance$max_distance)) / sd(lcp_A_B_RMSE_distance$max_distance)), data.frame(id = "B", z_score = (lcp_B_A_distance - mean(lcp_B_A_RMSE_distance$max_distance)) / sd(lcp_B_A_RMSE_distance$max_distance))), 
            aes(label= paste0("Z = ", round(z_score,2)), x= -2., y=80), colour="black", size = 8) + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0))

ggsave(filename = "./outputs/plots/lcp_z_score_plot.png", plot = lcp_z_score_plot, dpi = 300, width = 14, height = 7)

