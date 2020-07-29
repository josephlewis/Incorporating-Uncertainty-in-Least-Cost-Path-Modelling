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

License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
