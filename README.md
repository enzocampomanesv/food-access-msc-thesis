# **Food Accessibility Index for Ethiopia**

### This repository contains all the R scripts used in the development of the food accessibility index (FAI) for Ethiopia as part of my MSc thesis. It also contains all maps and plots created from the scripts.
---
### Each script has its own purpose and requirements and should be run in this sequence of steps to reproduce the results in the MSc thesis:

1. [Preprocessing of LSMS survey data](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_ESS_variable_preproc.R)
2. [Food access index construction using PCA](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_LSMS_PCA.R)
3. [Cereal price prediction](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_WFP_PricePrediction_v3.R)
4. [Population-weighted averaging](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_geoAveraging_1km.R) - run after spatially joining the geospatial input with the grids for averaging.
5. [Variable selection with RF](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_RF_varsel.R)
6. [GAM training and FAI prediction](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_GAM.R)
7. [Food access inequality quantification](https://github.com/enzocampomanesv/food-access-msc-thesis/blob/main/scripts/Food_access_index/ETH_Ineq.R)
---
### The following images are some of the major results from the MSc thesis:

- Map of calculated FAI at each LSMS geolocation
&nbsp;
![Food access index from PCA](./plots/PCA/FAI_LSMS_formal_v2.jpeg)
&nbsp;
- Map of extrapolated annual average FAI for Ethiopia 
![Annual average food access index](./plots/GAM/FAI_formal_annual_geo.jpeg)
&nbsp;
- Maps of monthly differences of the food access index from the annual average.
![Monthly FAI differences from annual average FAI](./plots/GAM/FAI_formal_monthly_geo_fixed.jpeg)
