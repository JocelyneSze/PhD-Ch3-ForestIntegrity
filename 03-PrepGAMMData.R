## with matched areas, create csv for building GAMMs 
## including FLII as response variable
pacman::p_load(terra, tidyverse, data.table)
# terra v1.4.22; tidyverse v1.3.1; data.table v1.14.0

#### process FLII data ####
flii <- rast("../Data/Grantham2020-FLII/flii_earth-002.tiff")
plot(flii) # 0.002694946 res / 300m

# tropical forest study extent
dat <- rast('../Data/Processed/ModInput/DataStack_pre2011.tif')

# crop to study boundaries
flii2 <- crop(flii, dat) 
flii2 <- flii/1000 # original index is from 0 to 10; dividing by 1000 returns it to original index.
flii3 <- aggregate(flii2, fact=3)
flii_resampled <- resample(flii3, dat, method='near')
writeRaster(flii_resampled, "../Data/Grantham2020-FLII/flii_tropics_1km.tif")


#### join anthromes, flii data to matched data ####
## covariates
dat <- rast('../Data/Processed/ModInput/DataStack_pre2011.tif')
names(dat)
# extract slope, elevation, pop, travel time, dist to roads, forest 2010 area, countries
dat <- subset(dat,11:17) # 0.008333 / 1km

## Forest Landscape Integrity Index
flii <- rast("../Data/Grantham2020-FLII/flii_tropics_1km.tif")
names(flii_resampled) <- "FLII2019"

## anthromes
ant1950 <- rast("../Data/Ellis2021-Anthromes/anthromes_12K_full/anthromes1950AD.asc")
ant2010 <- rast("../Data/Ellis2021-Anthromes/anthromes_12K_full/anthromes2010AD.asc")
ant <- c(ant1950, ant2010)
ant_resampled <- resample(ant, dat, method='near')

## Intact forest landscapes
ifl <- rast("../Data/Potapov2017-IFL/IFL_2013/ifl_2013.tif")
names(ifl) <- "IFL2013"

## matched areas of protection type 
matchDat <- rast("../Data/Raw/Type/Type_ECJRC_pre2011PAs_MatchedData.tif")

## join together and mask to get only matched areas
newDat <- c(matchDat, flii_resampled, ant_resampled, ifl, dat)
newDat <- mask(newDat, matchDat)
freq(newDat$Type_pre2011)
# layer value   count
# [1,]     1     0 1600298
# [2,]     1     1  579188
# [3,]     1     2  526503
# [4,]     1     3  420334
writeRaster(newDat, '../Data/Analysis/MatchedDataStack_2021-02-02.tif')

## convert to csv
df <- list()
for(i in 1:dim(newDat)[3]){
  ID <- strsplit(names(newDat[[i]]), "_")[[1]][1]
  cat(i, ID, sep=" ", '\n')
  df[[i]] <- as.data.frame(newDat[[i]], xy=TRUE, cells=FALSE, na.rm=TRUE)
  names(df[[i]])[3] <- ID
}
# combine to one
full <- df %>% 
  reduce(left_join, by=c('x','y'))  # 3126323 obs
summary(full)
#  x                  y                 Type           FLII2019     
#  Min.   :-109.995   Min.   :-33.5368   Min.   :0.0000   Min.   : 0.0    
#  1st Qu.: -62.620   1st Qu.: -3.6451   1st Qu.:0.0000   1st Qu.: 7.8    
#  Median :  16.146   Median :  0.4049   Median :0.0000   Median : 9.6    
#  Mean   :   9.903   Mean   :  2.0273   Mean   :0.9254   Mean   : 8.3    
#  3rd Qu.:  82.563   3rd Qu.:  6.8382   3rd Qu.:2.0000   3rd Qu.:10.0    
#  Max.   : 178.996   Max.   : 28.0382   Max.   :3.0000   Max.   :10.0    
#  NA's   :657940  
# anthromes1950AD anthromes2010AD    IFL2013            slope        
# Min.   :11.00   Min.   :11.00   Min.   :1         Min.   : 0.0000  
# 1st Qu.:52.00   1st Qu.:43.00   1st Qu.:1         1st Qu.: 0.8389  
# Median :53.00   Median :52.00   Median :1         Median : 1.8397  
# Mean   :50.64   Mean   :48.02   Mean   :1         Mean   : 4.3328  
# 3rd Qu.:53.00   3rd Qu.:53.00   3rd Qu.:1         3rd Qu.: 5.7472  
# Max.   :63.00   Max.   :62.00   Max.   :1         Max.   :63.6703  
# NA's   :3240    NA's   :3240    NA's   :1999892                    
#  elevation            ppp           travelTimeto5kcity  DisttoRoads    
#  Min.   : -31.89   Min.   :    0.00   Min.   :    0.00   Min.   :     0  
#  1st Qu.: 130.79   1st Qu.:    0.37   1st Qu.:   79.36   1st Qu.:  3162  
#  Median : 321.94   Median :    2.66   Median :  278.19   Median :  9000  
#  Mean   : 478.91   Mean   :   25.71   Mean   :  572.44   Mean   : 33638  
#  3rd Qu.: 571.99   3rd Qu.:   14.68   3rd Qu.:  813.97   3rd Qu.: 31064  
#  Max.   :5312.60   Max.   :47182.78   Max.   :59908.22   Max.   :489004  
#  
#  Forest2010Area         gadm36      
#  Min.   :0.000006   Min.   : 12.00  
#  1st Qu.:0.679789   1st Qu.: 41.00  
#  Median :0.777869   Median : 71.00  
#  Mean   :0.670187   Mean   : 97.04  
#  3rd Qu.:0.786699   3rd Qu.:145.00  
#  Max.   :0.787781   Max.   :255.00  


full <- full %>% 
  replace_na(list(IFL2013=0)) %>%  # 1999892 NAs
  drop_na(FLII2019) # 657940 NAs. result in 2468383 obs. 
# anthromes still have 1953 NAs. but not going to be part of model so not gonna drop.

fwrite(full, '../Data/Analysis/MatchedDataStack_2022-02-02.csv')

