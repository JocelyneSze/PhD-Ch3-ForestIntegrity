## Clean and harmonise spatial datasets for matching
pacman::p_load(raster, sf, fasterize, tidyverse, data.table)
# raster v3.0.12; sf v0.9.6; fasterize v1.0.0; tidyverse v1.3.1; data.table v1.14.0

# use Dinerstein et al's Ecoregions2017 layer of Tropical Forests as raster template
tmp <- raster('../Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.tif')

#### CLEAN COVARIATES ####
# ---- slope ----
slo <- raster('../Data/Raw/Amatulli2018_topo/slope_1KMmn_GMTEDmd.tif')
slo2 <- crop(slo, tmp)
slo3 <- resample(slo2, tmp, method='bilinear')
writeRaster(slo3, '../Data/Raw/Amatulli2018_topo/slope_TropForResampled.tif')

# ---- elevation ----
ele <- raster('../Data/Raw/Amatulli2018_topo/elevation_1KMmn_GMTEDmn.tif')
ele2 <- crop(ele, tmp)
ele3 <- resample(ele2, tmp, method='bilinear')
writeRaster(ele3, '../Data/Raw/Amatulli2018_topo/elevation_TropForResampled.tif')

# ---- population density ----
pop <- raster('../Data/Raw/Lloyd2017_worldPop/ppp_2010_1km_Aggregated.tif')
pop2 <- resample(pop, tmp, method='bilinear')
writeRaster(pop2, '../Data/Raw/Lloyd2017_worldPop/ppp_2010_TropForResampled.tif')

# ---- travel time ----
# using the layer for travel time to areas with at least 5k to 110m population
tt5k <- raster('../Data/Raw/Nelson2019_travelTime/travel_time_to_cities_12.tif')
tt5k2 <- resample(tt5k, tmp, method='bilinear')
writeRaster(tt5k2, '../Data/Raw/Nelson2019_travelTime/travel_time_to_cities_12_TropForResampled.tif')

# ---- distance to roads ----
# calculate distance from each raster cell to nearest road
# used ArcMap Distance toolset to calculate distance to road
rd54009 <- raster('../Data/Raw/SEDAC_groads-v1-global-gdb/DisttoRoads_TropicalForests_epsg54009.tif')
rd2 <- projectRaster(rd54009, res=0.008333333, crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', method='ngb')
rd3 <- resample(rd2, tmp, method='ngb')
writeRaster(rd3, '../Data/Raw/SEDAC_groads-v1-global-gdb/DisttoRoads_TropicalForests_epsg4326_resampled.tif')

# ---- baseline forest area ----
# using Global Forest Watch Tree Cover 2010 data 
fa2000 <- raster('../Data/Raw/GFW/Forest2000Area_Thresh25.tif')
fa20002 <- resample(fa2000, tmp, method='bilinear')
fa20002[fa20002<0] <- 0
writeRaster(fa20002, '../Data/Raw/GFW/Forest2000Area_Thresh25_TropForResampled.tif')


# ---- country polygons ----
gadm <- st_read('../Data/Raw/Country Boundaries/gadm36_Countries.gpkg')

gadmtmp <- raster(gadm, res=0.008333333)
gadmR <- fasterize(gadm, gadmtmp, field="ID_0", fun="first")
gadmR2 <- resample(gadmR, tmp, method='ngb') 
writeRaster(gadmR2, '../Data/Raw/Country Boundaries/gadm36_Countries_complete_resampled.tif')

# ---- Protection Types ----
    # ---- obtain spatial intersect of PA and IL (PIA) ----
    # PAs already filtered to those established before 2011, intersecting with
    # tropical forest biomes, not UNESCO Man and Biosphere and polygons only.
    # first, split PAs that are governed by Indigenous peoples or not
    pa <- read_sf('../Data/Raw/WDPA/WDPA_TropicalForests_before2011_flat.gpkg') # 8756 features
    # NOT governed by IP
    pa_noIP <- pa[which(pa$GOV_TYPE != "Indigenous peoples"),] # 8289 features
    pa_noIP <- st_make_valid(pa_noIP)
    st_write(pa_noIP, '../Data/Raw/WDPA/WDPA_TropicalForests_before2011_flat_noIP.gpkg', append=FALSE)      
    # governed by IP
    pa_IP <- pa[which(pa$GOV_TYPE == "Indigenous peoples"),] # 467
    st_write(pa_IP, '../Data/Raw/IPLandPA/WDPA_TropicalForests_before2011_flat_withIP.gpkg') 
    
    # second, to get spatial intersect of PA and IL.
    ipl <- read_sf('../Data/Raw/Garnett2018_IPL_2017/Dinerstein-TEOW/IPL2017_TropicalForests17.shp')
    ipl <- st_make_valid(ipl)
    pa_IPL_intersect <- st_intersection(pa_noIP, ipl)
    pa_IPL_intersect2 <- aggregate(pa_IPL_intersect, list(pa_IPL_intersect$WDPAID), function(x) x[1]) 
    pa_IPL_intersect2 <- select(pa_IPL_intersect2, -Group.1) # 2241 obs
    write_sf(pa_IPL_intersect2, '../Data/Raw/IPLandPA/WDPA_before2011_noIP_IPL2017_intersect_dissolvebyWDPAID.gpkg')
    
    # third, join the layer where PAs are governed by IP with the layer where PA intersect IL
    # pa_IP doesn't have Name_ and ORIG_FID columns
    pa_IPL_intersect2 <- read_sf('../Data/Raw/IPLandPA/WDPA_before2011_noIP_IPL2017_intersect_dissolvebyWDPAID.gpkg')
    pa_IP <- read_sf('../Data/Raw/IPLandPA/WDPA_TropicalForests_before2011_flat_withIP.gpkg')
    pa_IP[,c('Name_', 'ORIG_FID')] <- NA
    # join the intersecting layer and PAs governed by IP layer
    pa_ipl_IP <- rbind(pa_IP,pa_IPL_intersect2) # 2708 features
    pa_ipl_IP <- st_buffer(pa_ipl_IP, 0)
    st_write(pa_ipl_IP, '../Data/Raw/IPLandPA/Type3_pre2011PAs_22May.gpkg') 
    write_sf(pa_ipl_IP, '../Data/Raw/IPLandPA/Type3_pre2011PAs_22May.shp')
    # convert to epsg4326
    pa_ipl_IP_4326 <- st_transform(pa_ipl_IP, 4326)
    st_write(pa_ipl_IP_4326, '../Data/Raw/IPLandPA/Type3_pre2011PAs_22May_epsg4326.gpkg')
    
    # ---- get PAs without PIAs ----
    #  used ArcMap Analysis>Overlay>Erase tool to clip out 
    #  Type3_pre2011PAs_22May from WDPA_TropicalForests_before2011_flat_noIP, 
    #  to get WDPA_pre2011_erasedType3.shp
    
    # ---- get ILs without PIAs ----
    # used ArcMap Analysis>Overlay>Erase tool to clip out 
    # Type3_pre2011PAs_22May from IPL2017_TropicalForests, 
    # to get IPL2017_pre2011_erasedType3.shp
    
    # ---- combine PAs, IAs and PIAs ----
    # combine the different ones into one. 
    type1 <- st_read('../Data/Raw/WDPA/WDPA_pre2011_erasedType3.shp') # 7498 features, 29 fields
    type2 <- st_read('../Data/Raw/Garnett2018_IPL_2017/Dinerstein-TEOW/IPL2017_pre2011_erasedType3.shp') # 9095 features, 3 variables
    type3 <- st_read('../Data/Raw/IPLandPA/Type3_pre2011PAs_22May.shp') # 2708 features, 30 fields
    
    allTypes <- st_as_sf(data.table::rbindlist(list(type1, type2, type3), fill = TRUE))
    allTypes4326 <- st_transform(allTypes, 4326)
    st_write(allTypes4326, '../Data/Raw/Type/WDPApre2011only_ILonly_PIA_EPSG4326.gpkg', append=FALSE)
    
    # clipped to tropical forest extent in ArcMap
    
    # ---- rasterise  ----
    # use library(terra) cos it's faster than raster
    tmp <- rast('../Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.tif')
    allTypes <- vect('../Data/Raw/Type/WDPApre2011only_ILonly_PIA_EPSG4326_TropFor.shp')
    allTypesR <- terra::rasterize(allTypes, tmp, field='Protection', background=0)
    writeRaster(allTypesR, '../Data/Raw/Type/ProtectionTypes_pre2011PAs.tif')
    # ---- get boundary mask ----
    # converted from polygon to line in QGIS
    tmp <- rast('../Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.tif')
    outline <- vect('../Data/Raw/Type/WDPApre2011only_ILonly_PIA_EPSG4326_TropFor_lines.shp')
    outlineR <- rasterize(outline, tmp, background=0, touches=TRUE)
    writeRaster(outlineR, '../Data/Raw/Type/WDPApre2011only_ILonly_PIA_EPSG4326_TropFor_boundary.tif')
    
    # ---- PAs established from 2011 onwards as a mask ----
    wdpa <- st_read('../Data/Raw/WDPA/WDPA_TropicalForests_notMarine.gpkg')
    st_crs(wdpa) <- st_crs('ESRI:54009')
    
    # identify those from 2011 onwards
    wdpa_post2011 <- wdpa %>% 
      filter(STATUS_YR >= 2011) # n=1777
    
    wdpa_4326 <- st_transform(wdpa_post2011, 4326)
    st_write(wdpa_4326, '../Data/Raw/WDPA/WDPA_TropicalForests_post2011_epsg4326.gpkg')
    
    # rasterise it
    tmp <- raster('../Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.tif')
    wdpa_post2011R <- fasterize(wdpa_4326, tmp, fun="first", background=0)
    writeRaster(wdpa_post2011R, '../Data/Raw/WDPA/WDPA_TropicalForests_post2011_epsg4326.tif')
    
    # ---- multi-use PAs ----
    g <- st_read('../Data/Raw/WDPA/WDPA_TropicalForests_before2011_flat.gpkg')
    pre2011_multiuse <- g %>% 
      mutate(IUCN_CAT = as.factor(IUCN_CAT)) %>% 
      filter(IUCN_CAT == "V" | IUCN_CAT == 'VI') #2218
    st_write(pre2011_multiuse, '../Data/Raw/WDPA/WDPA_TropicalForests_before2011_CatVandVI.gpkg', append=FALSE)
    
    pre2011_4326 <- st_transform(pre2011_multiuse, 4326)
    
    # rasterise it
    tmp <- raster('../Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.tif')
    pre2011_multiuseR <- fasterize(pre2011_4326, tmp, fun="first", background=0)
    writeRaster(pre2011_multiuseR, '../Data/Raw/WDPA/WDPA_TropicalForests_before2011_CatVandVI_epsg4326.tif')
    

#### SPATIAL MASK ####
# ---- PAs post 2011, boundary borders, tree plantations ----
    ## previously used raster. now using terra version 1.2.5
    library(terra)
    
    # use dr as mask so whichever are NA here will also be NA for others
    dr <- rast('../Data/Raw/GFW/DefRate10to18_Thresh25_TropForResampled.tif')
    dr[dr>0] <- 0
    
    #  PAs post 2011 which will need to be NAs 
    post2011 <- rast('../Data/Raw/WDPA/WDPA_TropicalForests_post2011_epsg4326.tif')
    dr2 <- mask(dr, post2011, maskvalue=1)
    
    # boundary outlines of different types. masked out to make sure cells are 100% in one or other
    outline <- rast('../Data/Raw/Type/WDPApre2011only_ILonly_PIA_EPSG4326_TropFor_boundary.tif') 
    dr2 <- mask(dr2, outline, maskvalue=1)
    
    # tree plantations
    treePlant <- rast('../Data/Raw/GFW/MosaicPlantations_binary.tif')
    dr2 <- mask(dr2, treePlant, maskvalue=1)
    writeRaster(dr2, '../Data/Raw/WDPA/WDPApost2011_boundary_treePlantation_mask.tif')
    
#### CONVERT RASTER TO CSV ####
pacman::p_load(terra, data.table, tidyverse)
# terra v1.2.5; data.table v1.14.0; tidyverse v1.3.1    
  
# ---- stack and convert to dataframe ----    
  ## standard variables
  vars <- c('../Data/Raw/GFW/DefRate10to18_Thresh25_TropForResampled.tif',
            '../Data/Raw/GFW/ForestedPixels2010_Thresh25_TropForResampled.tif',
            '../Data/Raw/GFW/ForestedPixels2018_Thresh25_TropForResampled.tif',
            '../Data/Raw/Amatulli2018_topo/slope_TropForResampled.tif',
            '../Data/Raw/Amatulli2018_topo/elevation_TropForResampled.tif',
            '../Data/Raw/Lloyd2017_worldPop/ppp_2010_TropForResampled.tif',
            '../Data/Raw/Nelson2019_travelTime/travelTimeto5kcity_TropForResampled.tif',
            '../Data/Raw/SEDAC_groads-v1-global-gdb/DisttoRoads_TropicalForests_epsg4326_resampled.tif',
            '../Data/Raw/GFW/Forest2000Area_Thresh25_TropForResampled.tif',
            '../Data/Country Boundaries/gadm36_Countries_complete_resampled.tif')
  # inc the changing variable
  vars <- c(vars, '../Data/Raw/Type/ProtectionTypes_pre2011PAs.tif')
  # only multiuse PAs: '../Data/Raw/Type/Type_pre2011MultiUsePAsOnly.tif' 
  
  # mask layer
  newmask <- rast('../Data/Raw/WDPA/WDPApost2011_boundary_treePlantation_mask.tif')
  
  # get rasters
  dataStack <- rast(vars)
  dataStack2 <- mask(dataStack, newmask)
  writeRaster(dataStack2, '../Data/Processed/ModInput/DataStack_pre2011.tif')
  
  # get csv files
  df <- list()
  for(i in 1:dim(dataStack2)[3]){
    ID <- strsplit(names(dataStack2[[i]]), "_")[[1]][1]
    cat(i, ID, sep=" ", '\n')
    df[[i]] <- as.data.frame(dataStack2[[i]],xy=TRUE, cells=FALSE, na.rm=TRUE)
    names(df[[i]])[3] <- ID
  }
  # combine to one
  full <- df %>% 
    reduce(left_join, by=c('x','y')) %>% 
    drop_na() 
  
  fwrite(full, '../Data/Processed/ModInput/AllVarsDataset_pre2011PAs.csv')
    
# ---- split to each type and region ----
# to run matching for non-protected+PA; non-protected+IL; non-protected+PIA for each region
  
  ## gadm ID_0 numbers by ipbes region 
  Africa <- c(8,25,31,38,39,41,43,46,47,54,57,63,65,69,71,72,74,83,84,87,97,96,118,126,127,128,134,135,138,142,143,144,152,153,155,162,163,185,186,189,199,201,203,204,210,211,214,218,221,227,230,234,239,253,255,256)
  Americas <- c(9,11,12,18,21,24,26,28,29,33,35,42,45,48,53,56,59,67,66,68,70,80,91,92,94,98,99,101,113,141,145,151,161,174,177,178,183,193,194,196,219,233,237,243,245,249,251)
  AsiaPacific <- c(1,6,15,19,20,27,36,40,49,55,77,81,93,102,105,106,107,108,114,116,119,121,123,125,132,136,137,140,146,149,154,156,157,159,160,164,166,168,170,171,172,173,175,179,180,184,197,200,205,209,213,217,224,225,228,229,231,232,238,241,247,250,252,254)
  
  # remember to change file path!
  split_region <- function(dataset){
    type <- unique(dataset$Type)[2]
    if(type != 1){
      dataset$Type[dataset$Type != 0] <- 1 
    }
    Af <- dataset %>% 
      filter(gadm36 %in% Africa)
    fwrite(Af, paste0('../Data/Processed/1MatchIt_Input/Type', type, '_Africa_pre2011PAs.csv')) 
    Am <- dataset %>% 
      filter(gadm36 %in% Americas)
    fwrite(Am, paste0('../Data/Processed/1MatchIt_Input/Type', type, '_Americas_pre2011PAs.csv'))
    AP <- dataset %>% 
      filter(gadm36 %in% AsiaPacific)
    fwrite(AP, paste0('../Data/Processed/1MatchIt_Input/Type', type, '_AsiaPacific_pre2011PAs.csv'))
  }
  
  full <- fread('../Data/Processed/ModInput/AllVarsDataset_pre2011PAs.csv')
  
  # control units
  dat0 <- full[which(full$Type==0),] 

  ### type 1 (PA)
  dat1 <- full[which(full$Type==1),] 
  type1 <- rbind(dat0, dat1) 
  split_region(type1)
  
  ### type 2 (IL)
  dat2 <- full[which(full$Type==2),] 
  type2 <- rbind(dat0, dat2) 
  split_region(type2)
  
  ### type 3 (PIA)
  dat3 <- full[which(full$Type==3),] 
  type3 <- rbind(dat0, dat3) 
  split_region(type3)
  