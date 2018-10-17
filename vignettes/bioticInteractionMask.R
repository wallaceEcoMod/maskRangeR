---
title: "Masking with Forest Cover"
author: "Cory Merow"
date: "`r Sys.Date()`"
output:
  rmarkdown::html_vignette:
  fig_caption: yes
toc: true
toc_depth: 3
vignette: >
  %\VignetteIndexEntry{An overview of making rangeModelMetadata objects}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)
```

```{r,results='hide'}
library(maskedRangeR)
library(raster)
```

The spiny pocket mice (Heteromys spp. and Liomys spp.) are distributed from southern Texas to northern South America. All known species exhibit parapatry with their neighbors, which is likely caused by competition for resources and/or suitable habitat. This situation of congeneric species replacing each other across space is extremely common, and hence the methodology used here could make range estimates much more realistic. This research focuses on the distributions of two species in Ecuador: H. australis and H. teleus. H. australis ranges from Venezuela in the east to Ecuador in the south, but here we use high-quality data for the southern part of its range (Ecuador and southwestern Colombia; this situation of a regional model for part of the range represents a common situation, e.g., country-specific range estimates and modeling efforts that support them). H. teleus, found only in Ecuador, was recently discovered and remains very data-poor, with only 10 documented occurrences of varied spatial uncertainty. Sampling efforts for both species span many years, and neither species has ever been detected far within the range of its neighbor, with only a thin region of possible sympatry (or microparapatry) detectable: two sites of apparent sympatry exist near the estimated range boundary. As IUCN geographical estimates for data-poor species may be vast underestimates if convex hulls around known occurrences are used solely to delimit the extent of occurrence -- and conversely, estimates based on predictive models may vastly overestimate ranges if the distribution of the congeneric species is not taken into account --  we seek to improve estimates for both species using predictive models that are post-processed to consider both abiotic and biotic constraints. Because the congeners presumably have bidirectional effects on each other’s ranges, they should not be included in calibrating each other’s SDM (add citations for Soberon/Anderson/Hutchinson in vignette). 
Specifically, we estimate IUCN’s extent of occurrence (EOO) using convex hulls around both occurrence points (spatial) and around a thresholded SDM (environmental). For both EOO estimates, we then mask out the species’ predicted range using its SDM prediction thresholded by the minimum predicted suitability across all occurrences. Within this predicted range in the EOO, we then mask out biotically unsuitable areas (i.e., those more likely occupied by each species’ parapatric neighbor) to estimate the potential area of occupancy (IUCN’s AOO) in four ways: occupied grid cells, spatial, environmental, and a hybrid of both. The occupied grid cell AOO is recommended for most cases by the IUCN, and involves summing the area of all cells that overlap with species occurrences. The spatial AOO masks out areas predicted to be in the range of the congener using a simple spatial classifier trained with occurrence points of both species. The environmental AOO masks out areas predicted more suitable for the congener via raster algebra of the continuous SDM suitability predictions. The hybrid AOO masks out areas predicted to be in the range of the congener with the same spatial classifier as the spatial AOO, but adds continuous suitabilities predicted by the SDMs as additional predictor variables. The final calculation for AOO sums the area left after masking both with and without present forest cover. Finally, we report the differences in the area of EOO and AOO for both IUCN-recommended methodologies (EOO: convex hull of occurrences; AOO: occupied grid cells) and the masked range estimates. 
