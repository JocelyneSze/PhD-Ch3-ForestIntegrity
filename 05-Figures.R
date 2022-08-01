## Plot figures for manuscript
pacman::p_load(data.table, tidyverse, cowplot)
# data.table v1.14.0; tidyverse v1.3.1; cowplot v1.1.1

#### Fig 1: FLII map overview ####
# ---- prep data ---- 
library(terra) # v1.5.21

# get tif data to df, using coarser resolution data
type <- rast('../Data/Type_ECJRC_pre2011PAs_Masked_Resampled30x.tif')
flii <- rast("../Data/Grantham2020-FLII/flii_tropics_1km.tif")
flii[is.na(flii)] <- 0
flii2 <- aggregate(flii, fact=30, fun='mean', na.rm=TRUE)
datStack <- c(type, flii2)
writeRaster(datStack, "../Data/Map_FLII_Type_agg30x.tif")

# to use geom_raster, need evenly spaced tiles (inc NAs.)
datStack <- rast("../Data/Map_FLII_Type_agg30x.tif")
ant2010 <- rast("../Data/Ellis2021-Anthromes/anthromes_12K_full/anthromes2010AD.asc")
ant2 <- aggregate(ant2010, fact=3, fun='modal', na.rm=TRUE)
ant2 <- resample(ant2, datStack, method='near')
datStack <- c(datStack, ant2)
writeRaster(datStack, "../Data/Map_FLII_Type_Anthromes2010_agg30x.tif")

df <- list()
for(i in 1:dim(datStack)[3]){
  ID <- strsplit(names(datStack[[i]]), "_")[[1]][1]
  cat(i, ID, sep=" ", '\n')
  df[[i]] <- as.data.frame(datStack[[i]], xy=TRUE) # na.rm=TRUE
  names(df[[i]])[3] <- ID
}
# combine to one
full <- df %>% 
  reduce(full_join, by=c('x','y'))

# Make appropriate column headings
colnames(full) <- c("Longitude", "Latitude", "Type", "FLII", "Anthromes2010")
write_csv(full, "../Data/Map_FLII_Type_Anthromes2010_agg30x_incNAs.csv")

# ---- make plot ----
pacman::p_load(rnaturalearth, rnaturalearthdata) 
# rnaturalearth v0.1.0; rnaturalearthdata v0.1.0

# turn off s2 else coord_sf throws error
sf::sf_use_s2(FALSE)

## coastlines
coast <- ne_coastline(returnclass = 'sf')
## protection type and FLII data
dat <- read_csv("../Data/Map_FLII_Type_Anthromes2010_agg30x_incNAs.csv") %>% 
  mutate(Type = as.factor(Type),
         FLII = ifelse(is.na(Type), NA, FLII),
         Anthromes2010 = ifelse(is.na(Type), NA, Anthromes2010),
         Anthromes2010 = as.factor(Anthromes2010)) %>% 
  filter(Longitude >= -115) 

## add anthrome key
anthrome_key <- tibble::tibble(
  anthrome_class = factor(c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34,
                            41, 42, 43, 51, 52, 53, 54, 61, 62, 63, 70)),
  anthrome_className = factor(c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                                'Rainfed villages','Pastoral villages','Residential irrigated croplands',
                                'Residential rainfed croplands','Populated croplands','Remote croplands',
                                'Residential rangelands','Populated rangelands','Remote rangelands',
                                'Residential woodlands','Populated woodlands','Remote woodlands',
                                'Inhabited drylands','Wild woodlands', 'Wild drylands', 'Ice','NODATA'),
                              levels = c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                                         'Rainfed villages','Pastoral villages','Residential irrigated croplands',
                                         'Residential rainfed croplands','Populated croplands','Remote croplands',
                                         'Residential rangelands','Populated rangelands','Remote rangelands',
                                         'Residential woodlands','Populated woodlands','Remote woodlands',
                                         'Inhabited drylands','Wild woodlands', 'Wild drylands', 'Ice','NODATA')),
  anthrome_level = factor(c(rep('Dense settlements', 2),
                            rep('Villages', 4),
                            rep('Croplands', 4),
                            rep('Rangelands', 3),
                            rep('Cultured', 4),
                            rep('Wildlands', 3),
                            'NODATA'),
                          levels = c('Dense settlements','Villages','Croplands','Rangelands',
                                     'Cultured','Wildlands','NODATA')),
  anthrome_type = factor(c(rep('Intensive', 13),
                           rep('Cultured', 4),
                           rep('Wild', 3),
                           'NODATA'),
                         levels = c('Intensive', 'Cultured', 'Wild', 'NODATA')))
dat <- dat %>% 
  left_join(anthrome_key, by=c("Anthromes2010"="anthrome_class")) %>% 
  select(-c(Anthromes2010, anthrome_className, anthrome_type))

## colour-blind friendly palette for protection type
cbPalette <- c("#009E73", "#000000", "#E69F00", "#56B4E9")
# green, black, orange, blue
names(cbPalette) <- c("0", "1", "2", "3")

## colour-blind friendly palette for anthrome levels
anthPalette <- c("#662F00", "#CC9B7A","#F2DACD", "#CCFDFF", "#65EFFF", "#00A9CC") 
# dark brown, med brown, light brown, light blue, med blue, dark blue

## plot protection types
map_p1 <- ggplot() +
  geom_raster(data=dat, aes(x=Longitude, y=Latitude, fill=Type)) +
  geom_sf(data=coast, fill='transparent') +
  coord_sf(xlim=c(-115,180), ylim=c(-34,35)) +
  theme_bw() +
  scale_fill_manual(values=cbPalette,
                    name='Forest protection',
                    labels=c('Non-protected areas','Protected Areas only', 'Indigenous Lands only', 'Protected-Indigenous Areas'),
                    na.value="transparent") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        axis.text=element_text(size=9),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.margin=margin(t=-15,r=0,b=-15,l=0),
        legend.box.margin=margin(t=-15,r=0,b=-15,l=0),
        plot.margin=margin(t=-10,r=5,b=-15,l=1)) +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(nrow=2))

## plot FLII values
map_p2 <- ggplot() +
  geom_raster(data=dat, aes(x=Longitude, y=Latitude, fill=FLII)) +
  geom_sf(data=coast, fill='transparent') +
  coord_sf(xlim=c(-115,180), ylim=c(-34,35)) +
  theme_bw() +
  scale_fill_gradient(low="#edf8e9", high="#238b45", na.value="transparent",
                      breaks=c(0, 2.5, 5, 7.5, 10),
                      limits=c(0,10)) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        axis.text=element_text(size=9),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.margin=margin(t=-15,r=0,b=-15,l=0),
        legend.box.margin=margin(t=-15,r=0,b=-15,l=0),
        plot.margin=margin(t=-15,r=5,b=-15,l=1)) +
  xlab("") +
  ylab("")

## plot anthromes
map_p3 <- ggplot() +
  geom_raster(data=dat, aes(x=Longitude, y=Latitude, fill=anthrome_level)) +
  geom_sf(data=coast, fill='transparent') +
  coord_sf(xlim=c(-115,180), ylim=c(-34,35)) +
  theme_bw() +
  scale_fill_manual(values=anthPalette,
                    name='Anthrome levels',
                    labels=c("Dense settlements", "Villages", "Croplands", "Rangelands", "Cultured", "Wildlands"),
                    na.value="transparent",
                    na.translate=FALSE) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        axis.text=element_text(size=9),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.margin=margin(t=-15,r=0,b=-15,l=0),
        legend.box.margin=margin(t=-15,r=0,b=-15,l=0),
        plot.margin=margin(t=-10,r=5,b=-15,l=1)) +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(nrow=2))


## combine parts A and B and C
fig1 <- plot_grid(map_p1, map_p2, map_p3,
                  ncol=1, align="vh", 
                  labels=c("A", "B", "C"), label_size=10, label_x=0, label_y=0.98) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave('../Output/Figures/StudyArea_FLII_Types_2022-07-20.png', fig1,
       width = 18, height = 18, units ='cm') 


#### Fig 2: Naive FLII data ####
# naive FLII data
full2 <- fread("../Output/FLII_gadm_type_2022-02-27.csv")

# colour palettes
fliiPalette <- c("#662F00", "#D8AF97","#65EFFF")
names(fliiPalette) <- c("Low","Medium","High")


# look at average FLII by protection type
full2 %>% 
  drop_na() %>% 
  group_by(Type) %>% 
  summarise(meanFLII = mean(flii),
            n = n(),
            sd = sd(flii),
            se = sd(flii)/sqrt(n()),
            CI95lwr = meanFLII-(1.96*se),
            CI95upr = meanFLII+(1.96*se)) 
# and category
fliiCat <- full2 %>% 
  drop_na() %>% 
  mutate(Type = as.factor(Type),
         FLII_cat = cut(flii, breaks=c(0,6,9.5,10), include.lowest=TRUE, right=TRUE, labels=c("Low","Medium","High"), ordered_result=TRUE)) %>% 
  select(Type, FLII_cat) %>% 
  add_count(Type, name="totalN") %>% 
  add_count(Type, FLII_cat, name="FLIIN") %>% 
  distinct() %>% 
  mutate(totalN = totalN, # cos double counted est and act IFL rows
         FLIIprop = FLIIN/totalN,
         FLIIperc = round(FLIIprop*100, digits=2)) %>% 
  arrange(Type, FLII_cat)
# plot this
ggplot() +
  geom_col(data=fliiCat, aes(x=Type, y=FLIIperc, fill=FLII_cat), width=0.6) +
  scale_x_discrete(limits = c("0","1","2", "3"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA"),
                   name="Forest protection") +
  scale_fill_manual(values=fliiPalette, name="FLII category") +
  ylab("Percentage of area") +
  theme_bw() +
  theme(legend.position='bottom')


## average by region and type
# using just mean and SE
fliiTB <- full2 %>% 
  drop_na() %>% 
  group_by(Region, Type) %>% 
  summarise(meanFLII = mean(flii),
            n = n(),
            sd = sd(flii),
            se = sd(flii)/sqrt(n())) 

flii_p1 <- ggplot(fliiTB) +
  facet_wrap(~Region) +
  geom_hline(data=fliiTB[which(fliiTB$Type==0),], aes(yintercept=meanFLII), size=1, linetype=3) +
  geom_segment(data=fliiTB[which(fliiTB$Type!=0),], aes(x=as.numeric(Type)-0.2, xend=as.numeric(Type)+0.2, y=meanFLII, yend=meanFLII), size=1) +
  geom_segment(data=fliiTB[which(fliiTB$Type!=0),], aes(x=as.numeric(Type), xend=as.numeric(Type), y=meanFLII-se, yend=meanFLII+se), size=1) + 
  ## other specs
  ylab("Mean FLII value") +
  scale_x_discrete(limits = c("0","1","2"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="PA", "1"="IL", "2"="PIA")) +
  theme_bw() +
  theme(legend.position="none",
        axis.title.x = element_blank())

# using a violin plot
fliiTB <- full2 %>% 
  drop_na() %>%  # 14217825 obs after dropping NA
  select(Type, flii, Region) %>% 
  mutate(Type = as.factor(Type))

flii_p1 <- ggplot(fliiTB, aes(x=Type, y=flii)) +
  facet_wrap(~Region) +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=8, size=2) +
  stat_summary(fun=median, geom="point", shape=20, size=2) +
  ## other specs
  ylab("FLII score") +
  scale_x_discrete(limits = c("0","1","2", "3"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA")) +
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        axis.title.x=element_blank())

# avg by region and type for FLII categories
fliiTB2 <- full2 %>% 
  drop_na() %>% 
  mutate(Type = as.factor(Type),
         FLII_cat = cut(flii, breaks=c(0,6,9.5,10), include.lowest=TRUE, right=TRUE, labels=c("Low","Medium","High"), ordered_result=TRUE)) %>% 
  select(Region, Type, FLII_cat) %>% 
  add_count(Region, Type, name="totalN") %>% 
  add_count(Region, Type, FLII_cat, name="FLIIN") %>% 
  distinct() %>% 
  mutate(FLIIprop = FLIIN/totalN,
         FLIIperc = round(FLIIprop*100, digits=2)) %>% 
  arrange(Region, Type, FLII_cat)

flii_p2 <- ggplot() +
  facet_wrap(~Region) +
  geom_col(data=fliiTB2, aes(x=Type, y=FLIIperc, fill=FLII_cat), width=0.6, position=position_stack(reverse=TRUE)) +
  scale_x_discrete(limits = c("0","1","2", "3"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA"),
                   name="Forest protection") +
  scale_fill_manual(values=fliiPalette, name="FLII category") +
  ylab("Percentage of area") +
  theme_bw() +
  theme(legend.position='bottom',
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        axis.title.x=element_blank())

## combine parts A and B
fig2 <- plot_grid(flii_p1, flii_p2, ncol=1, align="v", labels="AUTO",
                  label_size=10) # drop rel_heights
ggsave("../Output/Figures/FLII_initialOverlay_2022-07-12.png", fig2,
       width=14, height=14, units='cm')

#### Fig 3: Anthromes ####
finalDF <- fread("../Output/GAMMedDataStack_quasibinomFLIIwAnthromes_2022-02-24.csv")
finalDF <- finalDF %>%   
  mutate(Type = as.factor(Type), 
         Region = as.factor(Region),
         anthromes1950AD = as.factor(anthromes1950AD))

# for 1950 
anthrome_key <- tibble::tibble(
  anthrome_class = factor(c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34,
                            41, 42, 43, 51, 52, 53, 54, 61, 62, 63, 70)),
  anthrome_className = factor(c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                                'Rainfed villages','Pastoral villages','Residential irrigated croplands',
                                'Residential rainfed croplands','Populated croplands','Remote croplands',
                                'Residential rangelands','Populated rangelands','Remote rangelands',
                                'Residential woodlands','Populated woodlands','Remote woodlands',
                                'Inhabited drylands','Wild woodlands', 'Wild drylands', 'Ice','NODATA'),
                              levels = c('Urban', 'Mixed settlements', 'Rice villages', 'Irrigated villages',
                                         'Rainfed villages','Pastoral villages','Residential irrigated croplands',
                                         'Residential rainfed croplands','Populated croplands','Remote croplands',
                                         'Residential rangelands','Populated rangelands','Remote rangelands',
                                         'Residential woodlands','Populated woodlands','Remote woodlands',
                                         'Inhabited drylands','Wild woodlands', 'Wild drylands', 'Ice','NODATA')),
  anthrome_level = factor(c(rep('Dense settlements', 2),
                            rep('Villages', 4),
                            rep('Croplands', 4),
                            rep('Rangelands', 3),
                            rep('Cultured', 4),
                            rep('Wildlands', 3),
                            'NODATA'),
                          levels = c('Dense settlements','Villages','Croplands','Rangelands',
                                     'Cultured','Wildlands','NODATA')),
  anthrome_type = factor(c(rep('Intensive', 13),
                           rep('Cultured', 4),
                           rep('Wild', 3),
                           'NODATA'),
                         levels = c('Intensive', 'Cultured', 'Wild', 'NODATA')))
# There's anthromes1950 and anthromes2010
finalDF <- finalDF %>% 
  left_join(anthrome_key, by=c("anthromes1950AD"="anthrome_class")) %>% 
  rename(antClass_1950 = anthrome_className,
         antLevel_1950 = anthrome_level,
         antType_1950 = anthrome_type) 

# for two years (https://stackoverflow.com/questions/46597278/how-to-plot-a-stacked-and-grouped-bar-chart-in-ggplot)
# need to turn the 1950 and 2010 data into long format
antDF <- finalDF %>% 
  filter(!is.na(antLevel_1950)) %>% 
  droplevels() %>% 
  pivot_longer(cols=c(antClass_2010:antType_1950),
               names_to=c("Var",".value"), names_sep="_") %>% 
  pivot_longer(cols=c(`2010`,`1950`),
               names_to="Year", values_to="category") %>% 
  mutate(category = factor(category, levels = c('Dense settlements','Villages','Croplands','Rangelands',
                                                'Cultured','Wildlands')))
y1950 <- filter(antDF, Year==1950, Var=="antLevel")
y2010 <- filter(antDF, Year==2010, Var=="antLevel")

barwidth=0.35
anth_palette <- c("#662F00", "#CC9B7A","#F2DACD", "#CCFDFF", "#65EFFF", "#00A9CC") 
#dark brown, med brown, light brown, light blue, med blue, dark blue from colorBlindness::Brown2Blue10Steps

# histogram of anthromes
ggplot(finalDF) +
  facet_grid(rows=vars(Type), cols=vars(Region)) +
  geom_bar(aes(y=antLevel_2010)) +
  theme_classic()

## stacked bar chart of anthrome levels
ggplot() +
  facet_wrap(~Region) +
  geom_bar(data=y1950, aes(x=Type, fill=category), width=barwidth) +
  geom_bar(data=y2010, aes(x=as.numeric(Type)+barwidth+0.02, fill=category), width=barwidth) +
  scale_x_discrete(limits = c("0","1","2", "3"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA"),
                   name="Forest protection") +
  scale_fill_manual(values=anth_palette, name="Anthrome levels") +
  ylab("Count") +
  theme_bw() +
  theme(legend.position='bottom')

# for just one year (2010AD)  
ggplot(finalDF) +
  facet_wrap(~Region) +
  geom_bar(aes(x=Type, fill=anthromes2010AD)) +
  scale_x_discrete(limits = c("0","1","2", "3"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA"),
                   name="Forest management") +
  scale_fill_manual(values=anth_palette, name="Anthrome levels") +
  ylab("Count") +
  theme_bw() +
  theme(legend.position='bottom')

### better to do in proportion
##  without distinguishing region 
## for 2010
anth_2010 <- finalDF %>% 
  select(c(antLevel_2010,Type)) %>% 
  add_count(Type, name="totalN") %>%
  add_count(Type, antLevel_2010, name="anthromeN") %>% 
  distinct() %>% 
  mutate(perc = round((anthromeN/totalN*100),2)) %>% 
  select(-c(totalN, anthromeN)) %>% 
  arrange(Type) %>% 
  mutate(Type = factor(Type, labels=c("None","PA","IL","PIA"))) %>% 
  pivot_wider(names_from=anthrome_level, values_from=perc) %>% 
  select(Type, `Dense settlements`, Villages, Croplands, Rangelands, Cultured, Wildlands)
# Type  `Dense settlements` Villages Croplands Rangelands Cultured Wildlands
# <fct>               <dbl>    <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
# 1 None                 1.36     7.1      13.8        6.23     60.4     11.1 
# 2 PA                   1.62     3.04      9.5        6.65     61.0     18.2 
# 3 IL                   1.68     5.38      7.72       4.97     72.4      7.86
# 4 PIA                  0.92     0.74      2.4        2.84     72.1     21.0 

anth_1950 <- finalDF %>% 
  select(c(antLevel_1950,Type)) %>% 
  add_count(Type, name="totalN") %>%
  add_count(Type, antLevel_1950, name="anthromeN") %>% 
  distinct() %>% 
  mutate(perc = round((anthromeN/totalN*100),2)) %>% 
  select(-c(totalN, anthromeN)) %>% 
  arrange(Type) %>% 
  mutate(Type = factor(Type, labels=c("None","PA","IL","PIA"))) %>% 
  pivot_wider(names_from=antLevel_1950, values_from=perc) %>% 
  select(Type, `Dense settlements`, Villages, Croplands, Rangelands, Cultured, Wildlands)
# Type  `Dense settlements` Villages Croplands Rangelands Cultured Wildlands
# <fct>               <dbl>    <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
# 1 None                 0.38     0.51      4.42       7.18     74.7     12.8 
# 2 PA                   0.36     0.18      1.81       6.94     70.7     20.0 
# 3 IL                   0.25     0.32      2.2        3.81     84.4      9.06
# 4 PIA                  0.09     0.01      0.56       1.93     75.5     21.9 

## split by region (for two years)
anth_region1950 <- y1950 %>% # alr filtered to anthrome levels
  select(c(category,Type, Region)) %>% 
  add_count(Region, Type, name="totalN") %>% 
  add_count(Region, Type, category, name="anthromeN") %>% 
  distinct() %>% 
  mutate(perc = round((anthromeN/totalN*100),2))
anth_region1950 %>% filter(Region=='Africa', Type=='0') %>% arrange(Type, category)

anth_region2010 <- y2010 %>% # alr filtered to anthrome levels
  select(c(category,Type, Region)) %>% 
  add_count(Region, Type, name="totalN") %>% 
  add_count(Region, Type, category, name="anthromeN") %>% 
  distinct() %>% 
  mutate(perc = round((anthromeN/totalN*100),2))
anth_region2010 %>% filter(Region=='Africa', Type=='0') %>% arrange(Type, category)

## plot figure by proportion
antProp <- ggplot() + 
  facet_wrap(~Region) +
  # geom_col(data=anth_region, aes(x=Type, y=anthromeN/totalN, fill=anthrome_level)) +
  geom_col(data=anth_region1950, aes(x=as.numeric(Type)-0.15, y=perc, fill=category), width=barwidth) +
  geom_col(data=anth_region2010, aes(x=as.numeric(Type)+barwidth-0.11, y=perc, fill=category), width=barwidth) +
  scale_x_discrete(limits = c("0","1","2", "3"), 
                   labels=c("0"="None", "1"="PA", "2"="IL", "3"="PIA"),
                   name="Forest protection") +
  ylab("Percentage of area") +
  scale_fill_manual(values=anth_palette, name="Anthrome levels") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave('../Output/Figures/Anthromes1950-2010-prop_2022-02-28.png', antProp,
       width = 14, height = 12, units ='cm')

#### Fig 4: GAMM output ####
## read in prediction results 
fliiDF_quasi <- read_csv("../Data/Analysis/GAMM_quasiBinomial_transVars_predResults.csv") %>% 
  mutate(Region = as.factor(Region), 
         Type = as.factor(Type),
         estFLIIori = fit*10,
         estFLIIlwr = (fit-se.fit)*10,
         estFLIIupr = (fit+se.fit)*10)

## calc percent difference between each protype and unprotected
## calculate SE for the % avoided following https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/percchg.pdf
# from: https://stats.stackexchange.com/questions/376639/what-is-the-standard-error-of-the-difference-in-means-scaled-as-percent-differen
fliiDF_percDiff <- read_csv("../Data/Analysis/GAMM_quasiBinomial_transVars_predResults.csv") %>% 
  group_by(Region) %>% 
  mutate(percentDiff = round(((fit - first(x=fit))/first(x=fit))*100, digits=2),
         percentSE = round((abs(fit/first(x=fit)) * sqrt((se.fit^2/fit^2) + (first(x=se.fit)^2/first(x=fit)^2)) * 100), digits=2))

# est effect of protection type on FLII 
flii_p1 <- ggplot(fliiDF_quasi) +
  facet_wrap(~Region) +
  ## GAMM predictions
  geom_hline(data=fliiDF_quasi[which(fliiDF_quasi$Type==0),], aes(yintercept=estFLIIori), size=0.75, colour='grey') + # removed aes colour=Type
  geom_rect(data=fliiDF_quasi[which(fliiDF_quasi$Type==0),], aes(ymin=estFLIIlwr, ymax=estFLIIupr), xmin=-Inf, xmax=Inf, alpha=0.3) +
  geom_segment(data=fliiDF_quasi[which(fliiDF_quasi$Type!=0),], aes(x=as.numeric(Type)-1.2, xend=as.numeric(Type)-0.8, y=estFLIIori, yend=estFLIIori), size=0.75) +
  geom_segment(data=fliiDF_quasi[which(fliiDF_quasi$Type!=0),], aes(x=as.numeric(Type)-1, xend=as.numeric(Type)-1, y=estFLIIlwr, yend=estFLIIupr), size=0.75) + 
  ## other specs
  ylab("Estimated FLII") +
  # xlab("Forest protection") +
  scale_x_discrete(limits = c("0","1","2"), # this specifies the position of x axis and what the axis label should be
                   labels=c("0"="PA", "1"="IL", "2"="PIA")) +
  theme_bw() +
  theme(legend.position="none",
        axis.title.x = element_blank()) 

ggsave('../Output/Figures/FLII_GAMM_multiusePAs_protectionEffect_2022-04-11.png', flii_p1,
       width = 14, height = 12, units ='cm') 

