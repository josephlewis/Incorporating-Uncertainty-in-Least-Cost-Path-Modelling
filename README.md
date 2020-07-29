# Incorporating-Uncertainty-in-Least-Cost-Path-Modelling

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper "Probabilistic Modelling using Monte Carlo Simulation for Incorporating Uncertainty in Least Cost Path Results: a Roman Road Case Study" authored by Lewis, J.

Getting Started
---------------

1. Open project using Incorporating-Uncertainty-in-Least-Cost-Path-Modelling.Rproj to ensure relative paths work.
2. Run the main.R R script in the R folder to reproduce the analysis. **Note: The output may slightly differ due to the random process of Monte Carlo simulation.**
  + Lines 51-56 creates the cost surface and Least Cost Paths without incorporating vertical error. 
  + Lines 60-75 creates 2,000 realisations of the cost surface and Least Cost Paths (1,000 north-to-south and 1,000 south-to-north) that incorporate vertical error. 
  + Lines 79-19 produces Figure 5 (A and B) showing the Least Cost Paths based on DEM without incorporating vertical error from North to South and from South to North.
  + Lines 113-126 creates Least Cost Path density rasters and probabilistic Least Cost Path density rasters
  + Lines 130-162 produces Figure 6 (A and B) showing the Probabilistic Least Costs Path based on deterministic Least Cost Path Realisations from Equally Likely Realisations of the True Elevation Surface from North to South and from South to North.
  + Lines 166-200 produces Figure 7 (A and B) showing Close-up of the Probabilistic Least Costs Path in the northern section based on deterministic Least Cost Path Realisations from Equally Likely Realisation of the True Elevation Surface from North to South and from South to North.
  + Lines 204-226 calculates maximum distance between Least Cost Paths (with and without incorporating vertical error) and High Street Roman road.
  + Lines 229-236 calculates the Z-scores of the maximum distance and the probability of obtaining a maximum distance less than the maximum distance calculated from south-to-north without incorporating vertical error
  + Lines 238-251 produces Figure 8 (A and B) showing the Frequency distribution of maximum distance Z-scores for the 2,000 LCPs incorporating vertical error from north to south and south to north and the maximum distance for LCPs not incorporating vertical error.
  
Session Info
---------------

```
R Under development (unstable) (2020-04-27 r78316)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18362)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_3.3.0       tmap_3.1            leastcostpath_1.7.4 rgeos_0.5-3         rgdal_1.4-8        
[6] raster_3.1-5        sp_1.4-1           

loaded via a namespace (and not attached):
 [1] tidyselect_1.1.0   gdistance_1.3-1    purrr_0.3.4        pbapply_1.4-2      sf_0.9-4          
 [6] lattice_0.20-41    colorspace_1.4-1   vctrs_0.3.0        generics_0.0.2     htmltools_0.5.0   
[11] stars_0.4-3        viridisLite_0.3.0  base64enc_0.1-3    XML_3.99-0.3       rlang_0.4.6       
[16] e1071_1.7-3        pillar_1.4.4       glue_1.4.1         withr_2.2.0        DBI_1.1.0         
[21] RColorBrewer_1.1-2 lifecycle_0.2.0    munsell_0.5.0      gtable_0.3.0       htmlwidgets_1.5.1 
[26] leafsync_0.1.0     codetools_0.2-16   crosstalk_1.1.0.1  parallel_4.1.0     class_7.3-17      
[31] leafem_0.1.1       Rcpp_1.0.4.6       KernSmooth_2.23-17 scales_1.1.0       classInt_0.4-3    
[36] lwgeom_0.2-5       leaflet_2.0.3      abind_1.4-5        png_0.1-7          digest_0.6.25     
[41] dplyr_1.0.0        tmaptools_3.1      grid_4.1.0         tools_4.1.0        magrittr_1.5      
[46] tibble_3.0.1       dichromat_2.0-0    crayon_1.3.4       pkgconfig_2.0.3    ellipsis_0.3.1    
[51] Matrix_1.2-18      rstudioapi_0.11    R6_2.4.1           units_0.6-6        igraph_1.2.5      
[56] compiler_4.1.0  
```

License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
