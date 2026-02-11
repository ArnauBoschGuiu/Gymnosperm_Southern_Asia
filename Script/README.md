# Are gymnosperms of the eastern and southern Asia doomed to disappear? Projections based on species richness patterns and ecological niche modeling
Here you will find the scripts used to analyze gymnosperm diversity patterns and model past, present, and future species distributions across Asia under climate change scenarios.

**Link to project:** LINK

![alt tag](http://LINK.com)

## Repository contents

### Spatial phylogenetic diversity analysis

- **Phylo_spatial_cor_100_km.R.r_tmp**  
  R script for calculating and mapping biodiversity metrics at a 100-km spatial resolution, including:
  - Species richness (SR)  
  - Rarity-weighted richness (RWR)  
  - Phylogenetic diversity (PD)  
  - Relative phylogenetic diversity (RPD)  
  - Standardized effect size of phylogenetic diversity (SES.PD)  
  - Phylogenetic endemism (PE)

### Species distribution modeling (SDM)

Species distribution models (SDMs) were developed separately for species with different numbers of occurrence records to improve model performance.

- **1.GymnospermSDM_ModelOptimization_over20occ (Origin)**  
  Model tuning and optimization for gymnosperm species with more than 20 occurrence records.

- **2.GymnospermSDM_ModelOptimization_under20occ (Origin)**  
  Model tuning and optimization for gymnosperm species with fewer than 20 occurrence records.

- **3.GymnospermSDM_model_predict_over20occ (Origin)**  
  Projection of optimized SDMs for species with more than 20 occurrence records across past, present, and future climate scenarios.

- **4.GymnospermSDM_model_predict_under20occ (Origin)**  
  Projection of optimized SDMs for species with fewer than 20 occurrence records across past, present, and future climate scenarios.

### SDM analysis

- **5.richness_maps**  
  Script for generating spatial richness maps and visualizing diversity patterns derived from SDM outputs.

- **6.Reclassification_and_Areas_calculation**  
  Script for reclassifying model outputs (e.g., suitable/unsuitable) and calculating area statistics under different climate scenarios.

## Notes

These scripts were used in the analyses presented in the associated manuscript. Users may need to adapt file paths and input data formats to their own systems.


**Tech used:** R, RStudio


