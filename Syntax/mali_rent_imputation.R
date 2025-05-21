library(haven)
library(sf)
library(tidyverse)
library(gstat)
library(ggplot2)
library(caret)
library(Hmisc)
library(grid)
library(gridExtra)
library(cowplot)
# library(ggpubr)
library(patchwork)
library(ggthemes)
library(ggmap)
library(recipes)
library(CAST)

# set wd and read data
getwd()
rm(list = ls())
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Mali/Stata/Analytic")
# complete datasets
mali_baseline <- read_dta("FTF_ZOI_2019_Mali_Household_NonPublic.dta")
# C:/Users/59952/ICF/SMRFS - Documents/11. Nepal Midline/08. Data Analysis/02. Midline/02. Processed Data/02. Analytic data

mali_midline <- read_dta("FTF_P2-ZOI_2023_Mali_Survey_household_data_analytic.dta")


mlml_red <- read_dta("FTF_P2-ZOI_2023_Mali_Parallel-Survey_household_data_raw.dta")

# mlbl_coords <- sf::read_sf("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Mali/R/ML_Rental_Values/UFTF cluster GPS.xlsx")

mali_adm3_shp <- sf::read_sf("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Mali/R/ML_Rental_Values/mli_admb_1m_gov_20211220_shp/mli_admbnda_adm3_1m_gov_20211220.shp")

sf::plot_sf(mali_adm3_shp)

table(mali_baseline$hhea) 
# 3-136

# latitude
head(mlml_red$c07a)
# longitude
head(mlml_red$c07b)



head(mali_baseline$latitude)
head(mali_baseline$longitude)

# vtable::vtable(mali_baseline)

# find  variables
# baseline
# regions/corridors (a03d), 
# roof (v201), 
# floor (v202), 
# flush toilet (v208), 
# pit latrine (v208), 
# other toilet (v208), 
# water: pipe, well, surface, tank, bottle (211), 
# cook_inhouse (v220), 
# electricity (v222_a)
# dplyr::select(latitude, longitude, wat_pipe, roof, floor, acc_electricity, lat_flush, cook_inhouse)

# pcdhouse
# lat - ?
# long - ?


# put housing on log scale
hist(mali_baseline$pcd_house)
mali_baseline$log_house <- log(mali_baseline$pcd_house)

hist(mali_baseline$log_house)


# covariates
table(mali_baseline$v208)
mali_baseline <- mali_baseline %>%
  dplyr::mutate(reg_1 = ifelse(a03d == 1, 1, 0), # Bougouni
                reg_2 = ifelse(a03d == 2, 1, 0), # Koutiala 
                reg_3 = ifelse(a03d == 3, 1, 0)) %>% # Sikasso
  dplyr::mutate(roof = ifelse(v201 == 31|v201 == 32|v201 == 33|
                                v201 == 34|v201 == 35|v201 == 36, 1, 0)) %>% # improved roof
  dplyr::mutate(floor = ifelse(v202 == 31|v202 == 32|
                                 v202 == 33|v202 == 34|
                                 v202 == 35, 1, 0),
                floor = ifelse(is.na(v202), NA, floor)) %>% # improved floor
  dplyr::mutate(acc_electricity = ifelse(v222a == 1, 1, 0)) %>% # electricity
  dplyr::mutate(lat_flush = ifelse(v208 == 11|v208 == 12|v208 == 13|v208 == 14|v208 == 15, 1, 0),
                lat_flush = ifelse(is.na(v208), NA, lat_flush)) %>% # flushing toielt
  dplyr::mutate(lat_pit = ifelse(v208 == 21|v208 == 22|v208 == 23, 1, 0),
                lat_pit = ifelse(is.na(v208), NA, lat_pit)) %>% # pit latrine
  dplyr::mutate(lat_oth = ifelse(v208 == 31|v208 == 41|v208 == 61|v208 == 96, 1, 0),
                lat_oth = ifelse(is.na(v208), NA, lat_oth)) %>% # other latrine
  dplyr::mutate(wat_pipe = ifelse(v211 == 11|v211 == 12|v211 == 13|v211 == 14, 1, 0),
                wat_pipe = ifelse(is.na(v211), NA, wat_pipe)) %>% # water source
  dplyr::mutate(wat_well = ifelse(v211 == 21|v211 == 31|v211 == 32, 1, 0),
                wat_well = ifelse(is.na(v211), NA, wat_well)) %>% # water source
  dplyr::mutate(wat_surf = ifelse(v211 == 41|v211 == 42|v211 == 51|v211 == 81, 1, 0),
                wat_surf = ifelse(is.na(v211), NA, wat_surf)) %>% # water source
  dplyr::mutate(wat_tank = ifelse(v211 == 61|v211 == 71, 1, 0),
                wat_tank = ifelse(is.na(v211), NA, wat_tank)) %>% # water source
  dplyr::mutate(wat_bott = ifelse(v211 == 91, 1, 0),
                wat_bott = ifelse(is.na(v211), NA, wat_bott)) %>% # water source
  dplyr::mutate(cook_inhouse = ifelse(v220 == 1, 1, 0),
                cook_inhouse = ifelse(is.na(v220), NA, cook_inhouse)) # cooking location - inhouse


mali_baseline %>%
  dplyr::select(reg_1:cook_inhouse) %>%
  colMeans(., na.rm = T)

mali_baseline <- 
  mali_baseline %>%
  select(hhea, hhnum, reg_1:cook_inhouse, latitude, longitude, pcd_house, log_house)


# Midline
# vtable::vtable(mali_midline)
# vtable::vtable(mlml_red)


# filter only consented HHs

mlml_red <- mlml_red %>%
  dplyr::filter(ahresult ==1)

# data wrangling. creating covariates
table(mlml_red$v222a)
mlml_red <- 
  mlml_red %>%
  dplyr::mutate(reg_1 = ifelse(c05 == 2, 1, 0), # Bougouni
                reg_2 = ifelse(c05 == 4, 1, 0), # Koutiala 
                reg_3 = ifelse(c05 == 1, 1, 0), # Sikasso
                reg_4 = ifelse(c05 == 3, 1, 0), # Kadiolo
                reg_5 = ifelse(c05 == 5, 1, 0), # Yanfolila 
                reg_6 = ifelse(c05 == 6, 1, 0)) %>% # Yorosso
  dplyr::mutate(roof = ifelse(v201 == 31|v201 == 32|v201 == 33|
                                v201 == 34|v201 == 35|v201 == 36, 1, 0)) %>% # improved roof
  dplyr::mutate(floor = ifelse(v202 == 31|v202 == 32|
                                 v202 == 33|v202 == 34|
                                 v202 == 35, 1, 0),
                floor = ifelse(is.na(v202), NA, floor)) %>% # improved floor
  dplyr::mutate(acc_electricity = ifelse(v222a == 1, 1, 0)) %>% # electricity
  dplyr::mutate(lat_flush = ifelse(v208 == 11|v208 == 12|v208 == 13|v208 == 14|v208 == 15, 1, 0),
                lat_flush = ifelse(is.na(v208), NA, lat_flush)) %>% # flushing toielt
  dplyr::mutate(lat_pit = ifelse(v208 == 21|v208 == 22|v208 == 23, 1, 0),
                lat_pit = ifelse(is.na(v208), NA, lat_pit)) %>% # pit latrine
  dplyr::mutate(lat_oth = ifelse(v208 == 31|v208 == 41|v208 == 61|v208 == 96, 1, 0),
                lat_oth = ifelse(is.na(v208), NA, lat_oth)) %>% # other latrine
  dplyr::mutate(wat_pipe = ifelse(v211 == 11|v211 == 12|v211 == 13|v211 == 14, 1, 0),
                wat_pipe = ifelse(is.na(v211), NA, wat_pipe)) %>% # water source
  dplyr::mutate(wat_well = ifelse(v211 == 21|v211 == 31|v211 == 32, 1, 0),
                wat_well = ifelse(is.na(v211), NA, wat_well)) %>% # water source
  dplyr::mutate(wat_surf = ifelse(v211 == 41|v211 == 42|v211 == 51|v211 == 81, 1, 0),
                wat_surf = ifelse(is.na(v211), NA, wat_surf)) %>% # water source
  dplyr::mutate(wat_tank = ifelse(v211 == 61|v211 == 71, 1, 0),
                wat_tank = ifelse(is.na(v211), NA, wat_tank)) %>% # water source
  dplyr::mutate(wat_bott = ifelse(v211 == 91, 1, 0),
                wat_bott = ifelse(is.na(v211), NA, wat_bott)) %>% # water source
  dplyr::mutate(cook_inhouse = ifelse(v220 == 1, 1, 0),
                cook_inhouse = ifelse(is.na(v220), NA, cook_inhouse)) # cooking location - inhouse

table(mlml_red$v211)

mlml_red <- 
  mlml_red %>%
  dplyr::mutate(latitude = c07a,
                longitude = c07b) %>%
  select(hhea, hhnum, reg_1:cook_inhouse, latitude, longitude)

# 
mlml_red %>%
  dplyr::select(reg_1:cook_inhouse) %>%
  colMeans(., na.rm = T)

mlml_red %>%
  dplyr::filter()



library(ape)


mali_baseline <- mali_baseline %>%
  dplyr::filter(!is.na(log_house))

mali_baseline <- mali_baseline %>%
  dplyr::filter(log_house > -6)



mali_baseline %>% 
  filter(if_any(everything(), is.na))

# inv_dist = with(mali_baseline, 1/dist(cbind(latitude, longitude), diag = TRUE, upper = TRUE))
# inv_dist = as.matrix(inv_dist)
# ape::Moran.I(x = mali_baseline$log_house, weight = inv_dist, 
#              scaled = TRUE, 
#              na.rm = T)

table(is.na(mali_baseline$log_house))

library(spatstat)
library(mgcv)
gam_model  <- gam(log_house ~ s(longitude, latitude, bs = 'gp', k = 40, m = 2), data = mali_baseline)



summary(gam_model)
plot(gam_model)

gamm_spat = gam(
  log_house ~ wat_pipe + roof + floor + acc_electricity + lat_flush + cook_inhouse, # choose your own features here
  data = mali_baseline,
  correlation = corSpatial(form = ~ longitude + latitude, type = 'gaussian')
)
# plot(gamm_spat)
# summary(gamm_spat)

hist(gamm_spat$fitted.values)

library(Metrics)

rmse(mali_baseline$log_house, gamm_spat$fitted.values)

rm(gam_model, gamm_spat)



# quickly check coordinates
plot(mali_baseline$latitude, mali_baseline$longitude)

plot(mlml_red$latitude, mlml_red$longitude)



# TRAINING DATA

library(sp)
# str(mali_baseline)
# coordinates(mali_baseline) <- ~longitude+latitude
# proj4string(mali_baseline) <- "+proj=longlat +ellps=GRS80"                  
# dat <- spTransform(mali_baseline, CRS=CRS("+proj=merc +ellps=GRS80"))   
# ( dat.coords <- coordinates(dat) )     

# convert values to numeric and remove any NA values 

mali_baseline <- 
  mali_baseline %>%
  #   select(-hhea, -hhnum) %>%
  mutate_at(dplyr::vars(-latitude, -longitude, -hhea, -hhnum), 
            dplyr::funs(as.numeric(.))) %>%
  na.omit



# training dataset
trainDat <- mali_baseline[createDataPartition(mali_baseline$log_house, p = 1,list=FALSE),]

predictors <-
  mali_baseline %>%
  dplyr::select(-log_house, -latitude, -longitude, -hhea, -hhnum, -reg_1, -reg_2, -reg_3, -pcd_house) %>%
  names()

predictors2 <-
  mali_baseline %>%
  dplyr::select(-log_house, -hhea, -hhnum, -pcd_house, -contains("reg")) %>%
  names()

predictors3 <-
  mali_baseline %>%
  dplyr::select(latitude, longitude, wat_pipe, roof, floor, acc_electricity, lat_flush, cook_inhouse, -pcd_house) %>%
  names()

predictors4 <-
  mali_baseline %>%
  dplyr::select(latitude, longitude) %>%
  names()

trainDat$log_house <- as.numeric(trainDat$log_house)


# test collinearity
library(rms)
library(GGally)
library(corpcor)
# pairs(trainDat, main = "base R: plots showing correlation of variables")


cor2pcor(cov(trainDat[, predictors]))
cor2pcor(cov(trainDat[, predictors3]))


 # vif(cor2pcor(cov(trainDat[, predictors3])))




# Create a resampling methods
# standard cross validations
cv <- trainControl(
  method='cv', number=10,
  verboseIter=TRUE,
  index=createFolds(trainDat$log_house, 10))



### LOOCV Nearest Neighbor Distance Matching
# leave location out cv. in the sampling, leave out a cluster of each grouping
library(CAST)
library(doParallel)
library(terra)

# create a method that handles spatial correlation
#create folds for Leave Location Out Cross Validation:
set.seed(10)
indices <- CreateSpacetimeFolds(trainDat,spacevar = "hhea")

ctrl <- trainControl(method="cv",index = indices$index, indexOut = indices$indexOut)


cl <- makeCluster(3)
registerDoParallel(cl)

pts <- mali_baseline %>%
  dplyr::select(-hhea)
pts <- st_as_sf(pts,coords=c("longitude","latitude"))

st_crs(pts) <- 4326
# studyArea <- rast(system.file("extdata","predictors_2012-03-25.grd",package="CAST"))

studyArea <- st_union(mali_adm3_shp)
st_crs(studyArea) <- 4326

studyArea <- st_transform(studyArea, crs = st_crs(pts))

plot(studyArea)
plot(st_geometry(pts), add = TRUE, col = "red")

pts2 <- mlml_red %>%
  dplyr::select(-hhea)
pts2 <- st_as_sf(pts2,coords=c("longitude","latitude"))
st_crs(pts2) <- 4326
plot(st_geometry(pts2), add = TRUE, col = "blue")

# nndm_folds <- nndm(tpoints = pts, ppoints = pts2, 
#                    samplesize = 300, min_train = .5)
# plot(nndm_folds)
#use for cross-validation:
# ctrl2 <- trainControl(method="cv",
#                      index=nndm_folds$indx_train,
#                      indexOut=nndm_folds$indx_test,
#                      savePredictions='final')




# Create a hyperparameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1, 13, 2))
)


library(geosphere)


knn_origmodel <- train(trainDat[,predictors4], trainDat$log_house,
                       method="knn",
                       trControl=cv,
                       tuneLength=5, tuneGrid = hyper_grid)


rf_origmodel <- train(trainDat[,predictors4],trainDat$log_house,method="rf",
                      trControl=cv,
                      tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))

gam_origmodel <- train(trainDat[,predictors4],trainDat$log_house,
                       method="gam",
                       trControl=cv,
                       tuneLength=5)


rf_origmodel
knn_origmodel
gam_origmodel



knn_model2 <- train(trainDat[,predictors],trainDat$log_house,method="knn",
                    trControl=ctrl,
                    tuneLength=5, tuneGrid = hyper_grid)


rf_model2 <- train(trainDat[,predictors],trainDat$log_house,method="rf",
                   trControl=ctrl,
                   tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))

gam_model2 <- train(trainDat[,predictors],trainDat$log_house,
                    method="gam",
                    trControl=ctrl,
                    tuneLength=5)

knn_model2.2 <- train(trainDat[,predictors2],trainDat$log_house,method="knn",
                      trControl=ctrl,
                      tuneLength=5, tuneGrid = hyper_grid)


rf_model2.2 <- train(trainDat[,predictors2],trainDat$log_house,method="rf",
                     trControl=ctrl,
                     tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))

gam_model2.2 <- train(trainDat[,predictors2],trainDat$log_house,
                      method="gam",
                      trControl=ctrl,
                      tuneLength=5)


rf_model2
knn_model2
gam_model2

rf_model2.2
gam_model2.2
knn_model2.2




varImp(rf_model2.2)

dotplot(resamples(list(
  'KNN Model' = knn_model2,
  'RF Model' = rf_model2,
  'GAM Model' = gam_model2
)), 
metric=c('RMSE', 'MAE'))

# knn_model <- train(trainDat[,predictors3],trainDat$log_house,method="knn",
#                    trControl=trainControl(method="cv",index=indices$index,indexOut=indices$indexOut),
#                    tuneLength=5, tuneGrid = hyper_grid)
# 
# 
# rf_model <- train(trainDat[,predictors3],trainDat$log_house,method="rf",
#                   trControl=trainControl(method="cv",index=indices$index,indexOut=indices$indexOut),
#                   tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))
# 
# gam_model <- train(trainDat[,predictors3],trainDat$log_house,
#                    method="gam",
#                    trControl=trainControl(method="cv",
#                                           index=indices$index,
#                                           indexOut=indices$indexOut),
#                    tuneLength=5)
# 
# 



dotplot(resamples(list(
  'KNN Model' = knn_model,
  'RF Model' = rf_model,
  'GAM Model' = gam_model
)), 
metric=c('RMSE', 'MAE'))


p.lh <- predict(gam_model2.2)

sqrt(mean((mali_baseline$log_house - p.lh)^2))

p.rf <- predict(rf_model2.2)

mali_baseline$p.lh <- p.lh
mali_baseline$p.rf <- p.rf


plot(p.rf, p.lh)

testdat <- mlml_red[, predictors3]

# library(naniar)
library(simputation)

testdat %>%
  bind_shadow() %>%
  impute_lm(roof ~ latitude + longitude) %>% 
  as.data.frame()

testdat_imputed <- aregImpute(~latitude + longitude + wat_pipe + roof          
                              + floor + acc_electricity + lat_flush + cook_inhouse, n.impute = 1,
                              type = "pmm", testdat)


testdat2 <- impute.transcan(testdat_imputed, imputation=1, data=testdat, list.out=TRUE,
                            pr=FALSE, check=FALSE) %>% as.data.frame()

mlml_red$pred_rent_rf_log <- predict(rf_model2.2, newdata = mlml_red)



mlml_red$pred_rent_rf <- exp(mlml_red$pred_rent_rf_log)

hist(mlml_red$pred_rent_rf)
hist(mlml_red$pred_rent_rf_log)

write_dta(mlml_red, "mali_midline_rental_values.dta")


mlmldv <- ggplot() +
  geom_sf(data = mali_adm3_shp) +
  geom_point(data = mlml_red,
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Midline", subtitle = "Household Locations in Parallel Survey") +
  theme(
    #    legend.position = "none",
    title = element_text(size = 11) # ,
    #      axis.text.x = element_blank()
  )


# FTF ZOI variable to cut shapefile
mali_adm3_shp <- 
  mali_adm3_shp %>%
  dplyr::mutate(FTF_ZOI = case_when(
    # bougouni, 
    ADM3_FR == "Bougouni" ~ 1, ADM3_FR == "Débélin" ~ 1, ADM3_FR == "Domba" ~ 1,
    ADM3_FR == "Farédiélé" ~ 1, ADM3_FR == "Faragouaran" ~ 1, ADM3_FR == "Kolélé" ~ 1,
    ADM3_FR == "Koumantou" ~ 1, ADM3_FR == "Kouroulamini" ~ 1, ADM3_FR == "Sanso" ~ 1, ADM3_FR == "Zantiébougou" ~ 1,
    
    # kadiolo
    ADM3_FR == "Kadiolo" ~ 1, ADM3_FR == "Loulouni" ~ 1, ADM3_FR == "Zégoua" ~ 1,
    
    # koutiala
    ADM3_FR == "Kapala" ~ 1, ADM3_FR == "Kolonigué" ~ 1, ADM3_FR == "Koutiala" ~ 1,
    ADM3_FR == "Logouana" ~ 1, ADM3_FR == "N'goutjina" ~ 1, ADM3_FR == "Nafanga" ~ 1,
    ADM3_FR == "Sincina" ~ 1, ADM3_FR == "Sinkolo" ~ 1, ADM3_FR == "Yognogo" ~ 1,
    ADM3_FR == "Sorobasso" ~ 1, ADM3_FR == "Zangasso" ~ 1, ADM3_FR == "Zebala" ~ 1,
    
    # sikasso
    ADM3_FR == "Diomaténé" ~ 1, ADM3_FR == "Fama" ~ 1, ADM3_FR == "Farakala" ~ 1,
    ADM3_FR == "Finkolo Ganadougou" ~ 1, ADM3_FR == "Gongasso" ~ 1, ADM3_FR == "Kaboila" ~ 1,
    ADM3_FR == "Kafouziela" ~ 1, ADM3_FR == "Kapala" ~ 1, ADM3_FR == "Kléla" ~ 1,
    ADM3_FR == "Kouoro" ~ 1, ADM3_FR == "N'tjikouna" ~ 1, ADM3_FR == "Natien" ~ 1,
    ADM3_FR == "Niéna" ~ 1, ADM3_FR == "Pimperna" ~ 1, ADM3_FR == "Sikasso" ~ 1,
    ADM3_FR == "Zanférébougou" ~ 1, ADM3_FR == "Zangaradougou" ~ 1,
    # yanfolila
    ADM3_FR == "Wassoulou Balle" ~ 1,
    # yorosso
    ADM3_FR == "Karangana" ~ 1, ADM3_FR == "Yorosso" ~ 1,
    TRUE ~ 0))
# check results
mali_adm3_shp %>%
  dplyr::select(ADM3_FR, ADM2_FR, FTF_ZOI) %>%
  st_drop_geometry() %>%
  dplyr::filter(FTF_ZOI == 1) %>%
  dplyr::arrange(ADM2_FR, ADM3_FR) %>%
  as.data.frame()

mlbldv_cut <- 
  ggplot() +
  geom_sf(data = mali_adm3_shp %>% dplyr::filter(FTF_ZOI == 1)) +
  geom_point(data = mali_baseline,
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Baseline", subtitle = "Location of Enumeration Areas") +
  theme(
    title = element_text(size = 11)
  )

mlmldv_cut <- 
  ggplot() +
  geom_sf(data = mali_adm3_shp %>% dplyr::filter(FTF_ZOI == 1)) +
  geom_point(data = mlml_red %>% dplyr::filter(hhea != 69 & hhnum != 29),
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Midline", subtitle = "Location of Households in the Parallel Survey") +
  theme(
    title = element_text(size = 11)
  )

mlmldv <- 
  ggplot() +
  geom_sf(data = mali_adm3_shp) +
  geom_point(data = mlml_red %>% dplyr::filter(hhea != 69 & hhnum != 29),
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Midline", subtitle = "Location of Households in the Parallel Survey") +
  theme(
    title = element_text(size = 11)
  )

mlbldv <- 
  ggplot() +
  geom_sf(data = mali_adm3_shp) +
  geom_point(data = mali_baseline,
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Baseline", subtitle = "Location of Enumeration Areas") +
  theme(
    title = element_text(size = 11)
  )



combinedbl <- mlbldv + mlbldv_cut

combinedbl + plot_layout(guides = "keep", ncol = 2, nrow = 1)


combinedml <- mlmldv + mlmldv_cut

combinedml + plot_layout(guides = "keep", ncol = 2, nrow = 1)



