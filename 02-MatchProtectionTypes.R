## Match for protection types for each region
## Done on HPC. 
## Script calls out a sample of the data to run matching for

shhh <- suppressPackageStartupMessages
shhh(library('MatchIt')) # to do actual matching
shhh(library('cem')) # this is for imbalance measures
shhh(library('cobalt')) # this is for balance tab
shhh(library('tableone')) # to create table one
shhh(library('dplyr'))
shhh(library('data.table'))
# MatchIt v4.3.3; cem v1.1.2; cobalt v4.2.2; tableone v0.13.0; dplyr v1.0.7; data.table v1.14.0

## read in dataset
args <- commandArgs(trailingOnly = TRUE)
cat('Dataset is', args[1], ', sample', args[2], sep=" ", "\n")
# args[1] is file path/name e.g. Input/1MatchIt_Input/Type1_Americas_pre2011.csv
# args[2] is sample number from 1 to 5
ID <- strsplit(basename(args[1]), "\\.")[[1]][1]
dat <- fread(args[1])

## take 100k sample of data 
if (grepl('pre2011PAs', ID)){
  if (grepl('Type1_Africa', ID)){
    fct <- 0.14 
  } else if (grepl('Type2_Africa', ID)) {
    fct <- 0.09 
  } else if (grepl('Type3_Africa', ID)) {
    fct <- 0.41 
  } else if (grepl('Type1_Americas', ID)) {
    fct <- 0.03
  } else if (grepl('Type2_Americas', ID)) {
    fct <- 0.04
  } else if (grepl('Type3_Americas', ID)) {
    fct <- 0.03
  } else if (grepl('Type1_AsiaPacific', ID)) {
    fct <- 0.25
  } else if (grepl('Type2_AsiaPacific', ID)) {
    fct <- 0.02
  } else if (grepl('Type3_AsiaPacific', ID)) {
    fct <- 0.14
  }
} else if (grepl('pre2011MultiUsePAs', ID)){
  if (grepl('Type1_Africa', ID)){
    fct <- 1 
  } else if (grepl('Type1_Americas', ID)) {
    fct <- 0.05
  } else if (grepl('Type1_AsiaPacific', ID)) {
    fct <- 0.9
  }
}


datSample <- dat[sample(nrow(dat),fct*nrow(dat)),]
datSample <- as.data.frame(datSample) %>% 
  mutate(Type = as.factor(Type),
         gadm36 = as.factor(gadm36))
if (!grepl('MultiUse', ID)){ # if it's not multi-use PAs, then all will need sampling
  fwrite(datSample, paste0('Output/Sample', args[2], '/', ID, '_dataSample.csv'))
} else {
  # for multiuse PAs, cos Type1_Africa and Type1_AsiaPacific doesnt need sampling
  if (grepl('Type1_Americas', ID)){
    fwrite(datSample, paste0('Output/Sample', args[2], '/', ID, '_dataSample.csv'))
  }
  if (grepl('Type1_AsiaPacific', ID)){
    fwrite(datSample, paste0('Output/Sample', args[2], '/', ID, '_dataSample.csv'))
  }
}


### do some pre-match checks for the sample
myVars = c("slope","elevation","ppp","travelTimeto5kcity","DisttoRoads","Forest2010Area", 'gadm36')
myVars2 = c("Type","slope","elevation","ppp","travelTimeto5kcity","DisttoRoads","Forest2010Area", 'gadm36')

cat('table one before matching', '\n')
print(CreateTableOne(vars = myVars2,
                     data = datSample,
                     strata = 'Type',
                     smd = TRUE))

cat('imbalance before matching', '\n')
preImb <- imbalance(group=datSample$Type, data=datSample[myVars])
print(preImb)

## do the matching
cat('doing strict PSM now for', ID, '\n')
myMatch <- matchit(Type~slope+elevation+ppp+travelTimeto5kcity+DisttoRoads+Forest2010Area, 
                   data=datSample, method='nearest', exact='gadm36', caliper=0.25)
matchedData <- match.data(myMatch)
fwrite(matchedData, paste0('Output/Sample', args[2], '/', ID, '_matchedData.csv'))

print(summary(myMatch, standardize=TRUE))

cat('imbalance after matching','\n')
postImb <- imbalance(group=matchedData$Type, data=matchedData[myVars]) 
print(postImb)

cat('balance tab after matching','\n')
balTab <- bal.tab(myMatch, data=datSample, stats=c("mean.diffs", "variance.ratios", "ks.statistics"), un=TRUE, s.d.denom="treated")
print(balTab)

cat('mean SMD before matching', '\n')
print(round(mean(abs(balTab$Balance$Diff.Un[-1])), digits=4))
cat('mean SMD after matching', '\n')
print(round(mean(abs(balTab$Balance$Diff.Adj[-1])), digits=4))

cat('table one after matching', '\n')
print(CreateTableOne(vars = myVars2,
                     data = matchedData,
                     strata = 'Type',
                     smd = TRUE))

## check number of rows
cat('number of rows and columns in this matched dataset', '\n')
print(dim(matchedData))

## input values into a summary dataframe
ct <- which(datSample$Type==0)

matchingSummary <- data.frame('ID' = ID,
                              'Ntotal' = dim(datSample)[1],
                              'Ncontrol' = length(ct),
                              'Ntreated' = dim(datSample)[1] - length(ct),
                              'L1' = round(preImb$L1$L1, digits=4),
                              'meanSMD' = round(mean(abs(balTab$Balance$Diff.Un[-1])), digits=4),
                              'matchedN' = dim(matchedData)[1],
                              'matchedL1' = round(postImb$L1$L1, digits=4),
                              'matchedSMD' = round(mean(abs(balTab$Balance$Diff.Adj[-1])), digits=4),
                              'sampleN' = args[2])

fwrite(matchingSummary, paste0('Output/Sample', args[2], '/', ID, '_matchSummary.csv'))
