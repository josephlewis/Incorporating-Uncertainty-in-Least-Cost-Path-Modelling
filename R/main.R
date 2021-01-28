#### IMPORT LIBRARIES ####

library(raster)  # for raster manipiulation
library(rgdal)  # for loading/writing geospatial data
library(sp)  # for vector manipulation
library(rgeos)  # for vector operations
library(leastcostpath)  # for creating cost surfaces, adding vertical error, calculating least cost paths and probabilistic least cost path
library(tmap)  # for producing probabilistic least cost path maps
library(ggplot2)  # for producing histograms graph
library(DMMF)  # for digital elevation model sink fills

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

elev_files <- list.files(path = "./Data/OS Terrain 50/", pattern = "\\.asc$", full.names = TRUE, recursive = TRUE)
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
A <- sp::SpatialPoints(cbind(349015.607, 524430.065))
B <- sp::SpatialPoints(cbind(343051.032, 508617.526))

#### PROCESSING WATERBODIES ####

# waterbodies to add to plots
waterbodies <- rgdal::readOGR("./Data/waterbodies/WFD_Lake_Water_Bodies_Cycle_2.shp")

#### CREATE COST SURFACES ####

# cost surface using Llobera and Sluckin's symmetric cost function and 48 adjacent cells
slope_cs <- leastcostpath::create_slope_cs(elev_osgb, cost_function = "llobera-sluckin", neighbours = 48)

# Least Cost Path from A to B
lcp <- leastcostpath::create_lcp(cost_surface = slope_cs, origin = A, destination = B, directional = TRUE, cost_distance = TRUE)

writeOGR(obj = lcp[lcp$direction == 'A to B',], dsn = './Outputs/Least Cost Paths', layer = 'LCP_OS_50', driver = 'ESRI Shapefile',
overwrite_layer = TRUE)

#### CALCULATE SPATIAL AUTOCORRELATION IN DIGITAL ELEVATION MODEL ####

elev_osgb_spdf <- as(elev_osgb, "SpatialPixelsDataFrame")
vario = variogram(layer~1, elev_osgb_spdf)
fit = fit.variogram(vario, vgm("Sph"))
window <- round(fit[fit$model == "Sph",]$range / max(res(elev_osgb)))

#### INCORPORATE VERTICAL ERROR AND CREATE LCPS ####

# create empty list to be filled with Least Cost Paths
lcps <- list()

for (i in 1:n) {
    
    print(i)
    
    # Create Least Cost Path based on sink-fill-corrected DEM with a random error field representing the vertical error added. Add to lcps list
    
    lcps[[i]] <- leastcostpath::create_lcp(cost_surface = leastcostpath::create_slope_cs(dem = DMMF::SinkFill(DEM = add_dem_error(dem = elev_osgb, 
        rmse = RMSE, type = "autocorrelated", size = window))$DEM_nosink, cost_function = "llobera-sluckin", neighbours = 48), origin = A, destination = B, directional = TRUE, 
        cost_distance = TRUE)

}

# bind list of Least Cost Paths to one object
lcps <- do.call(rbind, lcps)

writeOGR(obj = lcps[lcps$direction == 'A to B',], dsn = './Outputs/Least Cost Paths', layer = 'LCPs_OS_50', driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#### PRODUCE LEAST COST PATH MAPS ####

lcp_A_B_map <- tm_shape(elev_osgb) + tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, title = "Elevation (m)") + 
    tm_shape(waterbodies) + tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + tm_shape(road) + tm_polygons(col = "black", border.col = "black") + 
    tm_shape(lcp[lcp$direction == "A to B", ]) + tm_lines(col = "red") + tm_add_legend(type = "line", labels = "Least Cost Path", col = "red", border.col = "red") + 
    tm_legend(show = TRUE, outside = TRUE, legend.position = c("right", "bottom")) + tm_add_legend(type = "fill", labels = "Roman Road", col = "black", 
    border.col = "black") + tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", border.col = "#9ECAE1") + tm_compass(type = "arrow", 
    position = c("right", "bottom"), show.labels = 2) + tm_scale_bar(position = c("right", "bottom"), breaks = c(0, 2, 4))

tmap::tmap_save(lcp_A_B_map, './outputs/Plots/lcp.png', width = 5, height = 6)

#### CALCULATE PROBABILISTIC LEAST COST PATHS ####

# Create Least Cost Path density rasters using the Least Cost Paths incorporating vertical error
lcp_A_B_density <- leastcostpath::create_lcp_density(lcps[lcps$direction == "A to B", ], raster = elev_osgb, rescale = FALSE, rasterize_as_points = FALSE)

lcp_A_B_density[lcp_A_B_density == 0] <- NA

# Convert Least Cost Path density raster to a probability raster denoting the probability that an LCP crosses a cell
lcp_A_B_density <- (lcp_A_B_density/n)

raster::writeRaster(x = lcp_A_B_density, filename = './outputs/Probabilistic Least Cost Paths/lcps_density.tif', overwrite=TRUE)

#### PRODUCE PROBABILISTIC LEAST COST PATH MAPS ####

lcp_A_B_density_map <- tm_shape(elev_osgb) + tm_raster(palette = gray.colors(10, start = 0, end = 1), n = 10, legend.show = TRUE, legend.reverse = TRUE, 
    title = "Elevation (m)") + tm_shape(waterbodies) + tm_polygons(col = "#9ECAE1", border.col = "#9ECAE1", legend.show = TRUE) + tm_shape(lcp_A_B_density) + 
    tm_raster(style = "quantile", n = 10, title = "Probability of Least\nCost Path crossing\ncell", palette = viridis::plasma(10), legend.hist = FALSE) + 
    tm_shape(road) + tm_polygons(col = "black", border.col = "black") + tm_legend(show = TRUE, outside = TRUE, legend.position = c("right", "bottom")) + 
    tm_add_legend(type = "fill", labels = "Roman Road", col = "black", border.col = "black") + tm_add_legend(type = "fill", labels = "Waterbodies", col = "#9ECAE1", 
    border.col = "#9ECAE1") + tm_compass(type = "arrow", position = c("right", "bottom"), show.labels = 2) + tm_scale_bar(position = c("right", 
    "bottom"), breaks = c(0, 2, 4))

tmap::tmap_save(lcp_A_B_density_map, './outputs/Plots/lcps.png', width = 5, height = 6)


