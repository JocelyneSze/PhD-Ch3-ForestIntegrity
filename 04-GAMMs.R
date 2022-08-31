## GAMM results, predictions and joining outputs 
pacman::p_load(dplyr, mgcv, data.table, mgcViz)
# dplyr v1.0.7; mgcv v1,8.36; mgcViz v0.1.9; data.table v1.14.0

## csv data with FLII, anthromes, IFL and other covars
df <- fread('../Data/Analysis/MatchedDataStack_2022-02-02.csv') 
# 2468383 obs 

## collect results in a list
resultsDF <- list()

## to run in a loop for each region
for(i in 1:3){
  # ---- get the data set up for each region first ----
  if(i == 1){ # for Africa
    # FLII 936054 obs
    Region = c(8,25,31,38,39,41,43,46,47,54,57,63,65,69,71,72,74,83,84,87,97,96,118,126,127,128,134,135,138,142,143,144,152,153,155,162,163,185,186,189,199,201,203,204,210,211,214,218,221,227,230,234,239,253,255,256)  
    reg = "Africa"
    dataset <- df %>% 
      filter(gadm36 %in% Region) %>% 
      mutate(Type = as.factor(Type),
             anthromes1950AD = as.factor(anthromes1950AD),
             anthromes2010AD = as.factor(anthromes2010AD),
             gadm36 = as.factor(gadm36),
             Region = reg,
             FLIIscaled = FLII2019/10) %>% # scale index to 0-1 to fit binom
      mutate(slopeTrans = slope^(1/3),
             elevTrans = elevation^(1/3), 
             pppTrans = ppp^(1/5), 
             travelTrans = travelTimeto5kcity^(1/3),
             distTrans = DisttoRoads^(1/3),
             Forest2010Area = Forest2010Area)
    # 186919 obs for multiuse PAs Africa 
    # to create new dataset for prediction first
    newX <- dataset %>%
      select(slope, elevation, ppp, travelTimeto5kcity, DisttoRoads, Forest2010Area) %>%
      summarise_all(median, na.rm = TRUE) %>% 
      transmute(slopeTrans = slope^(1/3),
                elevTrans = elevation^(1/3), 
                pppTrans = ppp^(1/5), 
                travelTrans = travelTimeto5kcity^(1/3),
                distTrans = DisttoRoads^(1/3),
                Forest2010Area = Forest2010Area) %>% 
      as.list()
  } else if(i == 2){ # for Americas
    # FLII 998126  obs
    Region = c(9,11,12,18,21,24,26,28,29,33,35,42,45,48,53,56,59,67,66,68,70,80,91,92,94,98,99,101,113,141,145,151,161,174,177,178,183,193,194,196,219,233,237,243,245,249,251)
    reg = "Americas"
    dataset <- df %>% 
      filter(gadm36 %in% Region) %>% 
      mutate(Type = as.factor(Type),
             anthromes1950AD = as.factor(anthromes1950AD),
             anthromes2010AD = as.factor(anthromes2010AD),
             gadm36 = as.factor(gadm36),
             Region = reg,
             FLIIscaled = FLII2019/10) %>% 
      mutate(slopeTrans = slope^(1/3),
             elevTrans = elevation^(1/3),
             pppTrans = ppp^(1/6),
             travelTrans = travelTimeto5kcity^(1/3),
             distTrans = DisttoRoads^(1/3),
             Forest2010Area = Forest2010Area)
    # 142909 obs for multiuse Americas
    newX <- dataset %>%
      select(slope, elevation, ppp, travelTimeto5kcity, DisttoRoads, Forest2010Area) %>%
      summarise_all(median, na.rm = TRUE) %>% # change mean or median
      transmute(slopeTrans = slope^(1/3),
                elevTrans = elevation^(1/3),
                pppTrans = ppp^(1/6),
                travelTrans = travelTimeto5kcity^(1/3),
                distTrans = DisttoRoads^(1/3),
                Forest2010Area = Forest2010Area) %>% 
      as.list()
  } else if(i == 3){ # for Asia
    # 534203 obs
    Region = c(1,6,15,19,20,27,36,40,49,55,77,81,93,102,105,106,107,108,114,116,119,121,123,125,132,136,137,140,146,149,154,156,157,159,160,164,166,168,170,171,172,173,175,179,180,184,197,200,205,209,213,217,224,225,228,229,231,232,238,241,247,250,252,254) 
    reg = "Asia" 
    dataset <- df %>% 
      filter(gadm36 %in% Region) %>% 
      mutate(Type = as.factor(Type),
             anthromes1950AD = as.factor(anthromes1950AD),
             anthromes2010AD = as.factor(anthromes2010AD),
             gadm36 = as.factor(gadm36),
             Region = reg,
             FLIIscaled = FLII2019/10) %>% 
      mutate(slopeTrans = slope^(1/3),
             elevTrans = elevation^(1/3), 
             pppTrans = ppp^(1/5),
             travelTrans = travelTimeto5kcity^(1/3),
             distTrans = DisttoRoads^(1/3),
             Forest2010Area = Forest2010Area)
    # 100220 obs for Asia
    newX <- dataset %>%
      select(slope, elevation, ppp, travelTimeto5kcity, DisttoRoads, Forest2010Area) %>%
      summarise_all(median, na.rm = TRUE) %>% 
      transmute(slopeTrans = slope^(1/3),
                elevTrans = elevation^(1/3), 
                pppTrans = ppp^(1/5),
                travelTrans = travelTimeto5kcity^(1/3),
                distTrans = DisttoRoads^(1/3),
                Forest2010Area = Forest2010Area) %>% 
      as.list()
  }
  
  cat(i, reg, "\n")
  cat("there are", nrow(dataset), "observations", "\n")
  
  # ---- run GAMMs ----
  ## formula for FLII GAMM 
  gamform_flii <- FLIIscaled ~ Type + 
    s(slopeTrans, bs='cr') + s(elevTrans, bs='cr') + s(pppTrans, bs='cr') + s(travelTrans, bs='cr') + s(distTrans, bs='cr') +
    s(Forest2010Area, bs='cr') + s(gadm36, Forest2010Area, bs='re') +
    s(gadm36, bs='re') + s(Type, gadm36, bs='re') 
  # note: for Africa, k=20 in Forest2010Area is better than without.
  # for Americas, no apparent problem with default k values
  # for Asia, no apparent problem with default k values. 
  
  ## fit GAMM
  gam_flii_quasi <- bam(gamform_flii, data=dataset, family=quasibinomial(), discrete=TRUE)
  
  ## check GAMM
  sum(residuals(gam_flii_quasi, type="pearson")^2)/df.residual(gam_flii_quasi)
  summary(gam_flii_quasi)
  
  ## look at plots using mgcViz: https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html
  p1 <- getViz(gam_flii_quasi)
  # to look at diagnostic plots
  check.gamViz(p1) 
  
  ## plot all numeric effects
  print(plot(p1, allTerms = TRUE, select=c(1:6)), pages = 1)
  
  # ---- get predictions ----
  cat('Predict for GAMM', '\n')
  # exclude country effects 
  zero_set <- c("s(Forest2010Area,gadm36)", "s(gadm36)", "s(Type,gadm36)")  
  
  # include unique country  levels
  newX$gadm36 <- sort(unique(dataset$gadm36))
  # include the key predictor
  newX$Type <- sort(unique(dataset$Type))
  # grid of all possible values
  newX <- do.call(what = expand.grid, args = newX)
  
  # predictions
  newY_flii_quasi_trans <- newX %>% 
    predict(gam_flii_quasi, newdata = ., type = 'response', se.fit = TRUE, exclude = zero_set) %>% # 
    cbind(newX)
  
  # the set of adjusted effects are
  resultsDF[[i]] <- newY_flii_quasi_trans[c("Type", "fit", "se.fit")] %>% unique
  resultsDF[[i]]$Region <- reg

}

## join the predicted output for the three regions
resultsDF <- data.table::rbindlist(resultsDF)
fwrite(resultsTB, "../Data/Analysis/GAMM_quasiBinomial_transVars_predResults.csv")
