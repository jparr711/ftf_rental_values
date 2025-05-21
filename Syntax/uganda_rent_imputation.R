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
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/Stata/Analytic")
# complete datasets
uganda_baseline <- read_dta("FTF_P2-ZOI_Survey_Uganda_2019_household_data_analytic_REVISED_20231122.dta")
# C:/Users/59952/ICF/SMRFS - Documents/11. Nepal Midline/08. Data Analysis/02. Midline/02. Processed Data/02. Analytic data

uganda_midline <- read_dta("FTF_P2-ZOI_2022_Uganda_Survey_household_data_analytic.dta")


ugml_red <- read_dta("FTF_P2-ZOI_2022_Uganda_Parallel_Survey_household_data_raw.dta")

ugbl_coords <- sf::read_sf("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/R/UG_Rental_Values/UFTF cluster GPS.xlsx")

uganda_adm2_shp <- sf::read_sf("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/R/UG_Rental_Values/DistrictLevel/UG_Admin2_2020.shp")

uganda_adm2_shp <- sf::read_sf("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/R/UG_Rental_Values/DistrictLevel/UG_Admin2_2020.shp")

sf::plot_sf(uganda_adm2_shp)

table(uganda_baseline$hhea) 
# 3-136

# latitude
head(ugml_red$c07a)
# longitude
head(ugml_red$c07b)



# convert to sf obj

# ugbl_coords <- st_as_sf(ugbl_coords %>% dplyr::filter(!is.na(Cluster)), coords =c("Lat", "Long"))

uganda_baseline <- uganda_baseline %>% 
  dplyr::left_join(ugbl_coords, by = c("hhea"="Cluster"))

# uganda_baseline$Lat
# uganda_baseline$Long

# vtable::vtable(uganda_baseline)

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
uganda_baseline$log_house <- log(uganda_baseline$pcd_house)

# covariates
uganda_baseline <- uganda_baseline %>%
  dplyr::mutate(reg_1 = ifelse(a03d == 2, 1, 0), # East
                reg_2 = ifelse(a03d == 5, 1, 0), # Karamoja 
                reg_3 = ifelse(a03d == 3, 1, 0), # North 
                reg_4 = ifelse(a03d == 4, 1, 0)) %>% # Southwest
  dplyr::mutate(roof = ifelse(v201 == 31|v201 == 32|v201 == 33|
                              v201 == 34|v201 == 35|v201 == 36, 1, 0)) %>% # improved roof
  dplyr::mutate(floor = ifelse(v202 == 31|v202 == 32|
                               v202 == 33|v202 == 34|
                               v202 == 35|v202 == 36|v202 == 37, 1, 0),
                floor = ifelse(is.na(v202), NA, floor)) %>% # improved floor
  dplyr::mutate(acc_electricity = ifelse(v222a == 1, 1, 0)) %>% # electricity
  dplyr::mutate(lat_flush = ifelse(v208 == 11|v208 == 12|v208 == 13|v208 == 14|v208 == 15, 1, 0),
                lat_flush = ifelse(is.na(v208), NA, lat_flush)) %>% # flushing toielt
  dplyr::mutate(lat_pit = ifelse(v208 == 21|v208 == 22|v208 == 23, 1, 0),
                lat_pit = ifelse(is.na(v208), NA, lat_pit)) %>% # pit latrine
  dplyr::mutate(lat_oth = ifelse(v208 == 31|v208 == 41|v208 == 51|v208 == 61|v208 == 96, 1, 0),
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
  
  
uganda_baseline %>%
    dplyr::select(reg_1:cook_inhouse) %>%
    colMeans(., na.rm = T)
  

  
uganda_baseline <- 
  uganda_baseline %>%
    dplyr::mutate(latitude = Lat,
                  longitude = Long) %>%
    select(hhea, hhnum, reg_1:cook_inhouse, latitude, longitude, pcd_house, log_house)
  
  
# Midline
# vtable::vtable(uganda_midline)
# vtable::vtable(ugml_red)


# filter only consented HHs

ugml_red <- ugml_red %>%
  dplyr::filter(ahresult ==1)

# data wrangling. creating covariates
ugml_red <- 
  ugml_red %>%
  dplyr::mutate(reg_1 = ifelse(c06 == 1, 1, 0), # East
                reg_2 = ifelse(c06 == 2, 1, 0), # Karamoja  
                reg_3 = ifelse(c06 == 3, 1, 0), # North 
                reg_4 = ifelse(c06 == 4, 1, 0)) %>% # Southwest
  dplyr::mutate(roof = ifelse(v201 == 31|v201 == 32|v201 == 33|
                                v201 == 34|v201 == 35|v201 == 36, 1, 0)) %>% # improved roof - iron sheets, wood, asbestos, tiles, concrete, roofing shingles
  dplyr::mutate(floor = ifelse(v202 == 31|v202 == 32|
                                 v202 == 33|v202 == 34|
                                 v202 == 35|v202 == 36|v202 == 37, 1, 0),
                floor = ifelse(is.na(v202), NA, floor)) %>% # improved floor - parquet/polish, concrete, ceramic tiles, cement screed, carpet, stones, brick
  dplyr::mutate(acc_electricity = ifelse(v222a == 1, 1, 0)) %>% # electricity
  dplyr::mutate(lat_flush = ifelse(v208 == 11|v208 == 12|v208 == 13|v208 == 14|v208 == 15, 1, 0),
                lat_flush = ifelse(is.na(v208), NA, lat_flush)) %>% # flushing toielt
  dplyr::mutate(lat_pit = ifelse(v208 == 21|v208 == 22|v208 == 23, 1, 0),
                lat_pit = ifelse(is.na(v208), NA, lat_pit)) %>% # pit latrine
  dplyr::mutate(lat_oth = ifelse(v208 == 31|v208 == 41|v208 == 51|v208 == 61|v208 == 96, 1, 0),
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



ugml_red <- 
  ugml_red %>%
  dplyr::mutate(latitude = c07a,
                longitude = c07b) %>%
  select(hhea, hhnum, reg_1:cook_inhouse, latitude, longitude)

# 
ugml_red %>%
  dplyr::select(reg_1:cook_inhouse) %>%
  colMeans(., na.rm = T)

ugml_red %>%
  dplyr::filter()



library(ape)


uganda_baseline <- uganda_baseline %>%
  dplyr::filter(!is.na(log_house))

inv_dist = with(ugml_red, 1/dist(cbind(latitude, longitude), diag = TRUE, upper = TRUE))
inv_dist = as.matrix(inv_dist)
ape::Moran.I(x = ugml_red$roof, weight = inv_dist, scaled = TRUE, na.rm = T)


library(spatstat)
library(mgcv)
gam_model  <- gam(log_house ~ s(longitude, latitude, bs = 'gp', k = 100, m = 2), data = uganda_baseline)



summary(gam_model)
plot(gam_model)

gamm_spat = gam(
  log_house ~ wat_pipe + roof + floor + acc_electricity + lat_flush + cook_inhouse, # choose your own features here
  data = uganda_baseline,
  correlation = corSpatial(form = ~ longitude + latitude, type = 'gaussian')
)
plot(gamm_spat)
summary(gamm_spat)

hist(gamm_spat$fitted.values)

library(Metrics)

rmse(uganda_baseline$log_house, gamm_spat$fitted.values)

rm(gam_model, gamm_spat)



# quickly check coordinates
plot(uganda_baseline$latitude, uganda_baseline$longitude)

plot(ugml_red$latitude, ugml_red$longitude)



# TRAINING DATA

library(sp)
# str(uganda_baseline)
# coordinates(uganda_baseline) <- ~longitude+latitude
# proj4string(uganda_baseline) <- "+proj=longlat +ellps=GRS80"                  
# dat <- spTransform(uganda_baseline, CRS=CRS("+proj=merc +ellps=GRS80"))   
# ( dat.coords <- coordinates(dat) )     

# convert values to numeric and remove any NA values 

uganda_baseline <- 
  uganda_baseline %>%
  #   select(-hhea, -hhnum) %>%
  mutate_at(dplyr::vars(-latitude, -longitude, -hhea, -hhnum), 
            dplyr::funs(as.numeric(.))) %>%
  na.omit



# training dataset
trainDat <- uganda_baseline[createDataPartition(uganda_baseline$log_house, p = 1,list=FALSE),]

predictors <-
  uganda_baseline %>%
  dplyr::select(-log_house, -latitude, -longitude, -hhea, -hhnum, -reg_1, -reg_2, -reg_3, -reg_4, -pcd_house) %>%
  names()

predictors2 <-
  uganda_baseline %>%
  dplyr::select(-log_house, -hhea, -hhnum, -pcd_house, -contains("reg")) %>%
  names()

predictors3 <-
  uganda_baseline %>%
  dplyr::select(latitude, longitude, wat_pipe, roof, floor, acc_electricity, lat_flush, cook_inhouse, -pcd_house) %>%
  names()

predictors4 <-
  uganda_baseline %>%
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

pts <- uganda_baseline %>%
  dplyr::select(-hhea)
pts <- st_as_sf(pts,coords=c("longitude","latitude"))

st_crs(pts) <- 4326
# studyArea <- rast(system.file("extdata","predictors_2012-03-25.grd",package="CAST"))

studyArea <- st_union(uganda_adm2_shp)
st_crs(studyArea) <- 4326

studyArea <- st_transform(studyArea, crs = st_crs(pts))

plot(studyArea)
plot(st_geometry(pts), add = TRUE, col = "red")

pts2 <- ugml_red %>%
  dplyr::select(-hhea)
pts2 <- st_as_sf(pts2,coords=c("longitude","latitude"))
st_crs(pts2) <- 4326
plot(st_geometry(pts2), add = TRUE, col = "blue")

nndm_folds <- nndm(tpoints = pts, ppoints = pts2, 
                   samplesize = 300, min_train = .5)
plot(nndm_folds)
#use for cross-validation:
ctrl2 <- trainControl(method="cv",
                      index=nndm_folds$indx_train,
                      indexOut=nndm_folds$indx_test,
                      savePredictions='final')




# Create a hyperparameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1, 13, 2))
)


library(geosphere)


knn_origmodel <- train(trainDat[,predictors4], trainDat$log_house,
                       method="knn",
                       trControl=ctrl,
                       tuneLength=5, tuneGrid = hyper_grid)


rf_origmodel <- train(trainDat[,predictors4],trainDat$log_house,method="rf",
                      trControl=ctrl,
                      tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))

gam_origmodel <- train(trainDat[,predictors4],trainDat$log_house,
                       method="gam",
                       trControl=ctrl,
                       tuneLength=5)


knn_origmodel

rf_origmodel

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


knn_model2
rf_model2
gam_model2

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



knn_model2.2
rf_model2.2
.gam_model2.2

hist(rf_model2.2$pred$pred)
hist(gam_model2.2$pred$pred)

plot(gam_model2.2$pred$pred, rf_model2.2$pred$pred)

plot(knn_model2.2$pred$pred, rf_model2.2$pred$pred)

knn_model2.2
gam_model2.2

ugoutcome <- cbind("outcome" = gam_model2.2$finalModel$model$.outcome, 
      "latitude" = uganda_baseline$latitude, 
      "longitude" = uganda_baseline$longitude) %>% as.data.frame()

plot(ugoutcome$longitude, ugoutcome$latitude)
plot(ugoutcome)

abline(ugoutcome$longitude, ugoutcome$outcome)

uganda_adm2_shp <- st_as_sf(uganda_adm2_shp)



plot(gam_model2$finalModel$fitted.values, knn_model2$finalModel$fitted.values)
abline(gam_model2$finalModel$fitted.values, knn_model2$finalModel$fitted.values, add = T)
gam_model2[[11]]

varImp(rf_model2)

dotplot(resamples(list(
  'KNN Model' = knn_model2,
  'RF Model' = rf_model2,
  'GAM Model' = gam_model2
)), 
metric=c('RMSE', 'MAE'))

knn_model <- train(trainDat[,predictors3],trainDat$log_house,method="knn",
                   trControl=trainControl(method="cv",index=indices$index,indexOut=indices$indexOut),
                   tuneLength=5, tuneGrid = hyper_grid)


rf_model <- train(trainDat[,predictors3],trainDat$log_house,method="rf",
                  trControl=trainControl(method="cv",index=indices$index,indexOut=indices$indexOut),
                  tuneLength=5, tuneGrid = data.frame(mtry = seq(2, 10, by =2)))

gam_model <- train(trainDat[,predictors3],trainDat$log_house,
                   method="gam",
                   trControl=trainControl(method="cv",
                                          index=indices$index,
                                          indexOut=indices$indexOut),
                   tuneLength=5)





rf_model
knn_model
gam_model



varImp(rf_model)


dotplot(resamples(list(
  'KNN Model' = knn_model,
  'RF Model' = rf_model,
  'GAM Model' = gam_model
)), 
metric=c('RMSE', 'MAE'))


gam_newmodel <- gam(log_house ~ wat_pipe + roof + floor + acc_electricity + lat_flush + cook_inhouse, # choose your own features here
  data = uganda_baseline,
  correlation = corSpatial(form = ~ longitude + latitude, type = 'gaussian'))

summary(gam_newmodel)

gam_newmodel

p.lh <- predict(gam_newmodel)

sqrt(mean((uganda_baseline$log_house - p.lh)^2))

p.rf <- predict(rf_model)

uganda_baseline$p.lh <- p.lh
uganda_baseline$p.rf <- p.rf


plot(p.rf, p.lh)

testdat <- ugml_red[, predictors2]

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

ugml_red$pred_rent_rf_log <- predict(rf_model, newdata = testdat2)



ugml_red$pred_rent_rf <- exp(ugml_red$pred_rent_rf_log)


ugmldv <- ggplot() +
  geom_sf(data = uganda_adm2_shp) +
  geom_point(data = ugml_red,
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


ugbldv <- ggplot() +
  geom_sf(data = uganda_adm2_shp) +
  geom_point(data = uganda_baseline,
             aes(x = longitude, y = latitude), size = 2) +
  theme_bw() +
  scale_colour_gradient(low="black", high="black") +
  scale_fill_gradient(low="white", high="white") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Baseline", subtitle = "Location of Enumeration Areas") +
  theme(
    title = element_text(size = 11)
  )



combined <- ugbldv + ugmldv
combined + plot_layout(guides = "keep", ncol = 2, nrow = 1)

hist(ugml_red$pred_rent_rf)
hist(ugml_red$pred_rent_rf_log)

write_csv(ugml_red, "ugml_red.csv")

write_dta(ugml_red, "ugml_red.dta")

