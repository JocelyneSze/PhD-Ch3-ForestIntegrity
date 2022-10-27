# PhD-Forest Integrity
Code for [manuscript submitted to Current Biology: Sze et al. (2022) Indigenous lands in protected areas have high forest integrity across the tropics](https://www.sciencedirect.com/science/article/pii/S0960982222015408). An analysis of how forest landscape integrity index (FLII) is distributed across the tropics within protected areas, Indigenous lands, protected-Indigenous areas, and non-protected areas, changes in Anthromes from 1950-2010 within these matched areas, and the protective effect of each protection type on FLII based on regression modelling. 

01-CleanSpatialData provides code used to clean spatial data of covariates used in the matching processs, creation of the protection types raster (using data from World Database of Protected Areas and Indigenous Peoples' Land), and areas that were masked out of the analysis.

02-MatchProtectionTypes provides the R code that was used to run the matching process (for each tropical region, for each protection type), which was conducted using the University of Sheffield's High Performance Computing cluster, ShARC (Sheffield Advanced Research Computer).

03-PrepGAMMData provides code used to combine data from the matched areas with FLII and Anthromes data.

04-GAMMs provides code used to run the GAMMs for each tropical region.

05-Figures provides code used to make the figures and calculate summary statistics reported in the manuscript and supplementary materials.
