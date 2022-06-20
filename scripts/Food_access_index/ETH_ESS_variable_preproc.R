# Living Standard Measurement Study (LSMS) Variable Preprocessing in R
# 
# Florencio Campomanes V
# 2022-02-16
#
# This script only requires base R packages.
#
# This script requires one user supplied dataset:
# Ethiopia 2018 LSMS dataset
# (Source: https://microdata.worldbank.org/index.php/catalog/3823/study-description)
#
# Notes:
# (a) All file paths and names should be changed as needed
#

setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia\\ETH_2018_ESS")

rm(list=ls(all=TRUE))
process_load = 1

################################################################################
########### DEFINE FUNCTIONS ##############

# calc summarized zScore of price per kg per crop
calc_zScore_crop <- function(price,crop_id,meanP, sdP){
  return ((price-meanP)/sdP)
}

# calc zScore given a df/matrix
calc_zScore <- function(x,df){
  meanX <- mean(df)
  sdX <- sd(df)
  return((x-meanX)/sdX)
}

########### END OF FUNCTIONS ##############
################################################################################

################################################################################
########### START OF DATA PREPARATION ##############

if(process_load == 1){
  
  #################################################################################
  ########### ASSETS RELATED TO ACCESS ##############
  
  hh_asset <- read.csv("sect11_hh_w4.csv")
  
  hh_asset <- hh_asset[hh_asset$asset_cd == "13. Bicycle" |
                         hh_asset$asset_cd == "14. Motor cycle" |
                         hh_asset$asset_cd == "15. Cart (Hand pushed)" |
                         hh_asset$asset_cd == "16. Cart (animal drawn)- for transporting people and goods" |
                         hh_asset$asset_cd == "22. Private car" |
                         hh_asset$asset_cd == "1. Kerosene stove"|
                         hh_asset$asset_cd == "2. Cylinder gasstove"|
                         hh_asset$asset_cd == "3. Electric stove"|
                         hh_asset$asset_cd == "19. Mitad-Electric"|
                         hh_asset$asset_cd == "20. Energy saving stove (lakech, mirt etc)"|
                         hh_asset$asset_cd == "21. Refrigerator"|
                         hh_asset$asset_cd == "26. Shelf for storing goods"|
                         hh_asset$asset_cd == "27. Biogas stove (pit)",]
  
  hh_asset$bike <- ifelse(hh_asset$asset_cd == "13. Bicycle" &
                            hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$motor <- ifelse(hh_asset$asset_cd == "14. Motor cycle" &
                             hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$cart_h <- ifelse(hh_asset$asset_cd == "15. Cart (Hand pushed)" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$cart_a <- ifelse(hh_asset$asset_cd == "16. Cart (animal drawn)- for transporting people and goods" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$car <- ifelse(hh_asset$asset_cd == "22. Private car" &
                           hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$ker_st <- ifelse(hh_asset$asset_cd == "1. Kerosene stove" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$cyl_st <- ifelse(hh_asset$asset_cd == "2. Cylinder gasstove" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$ele_st <- ifelse(hh_asset$asset_cd == "3. Electric stove" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$ele_mitad <- ifelse(hh_asset$asset_cd == "19. Mitad-Electric" &
                                 hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$energy_st <- ifelse(hh_asset$asset_cd == "20. Energy saving stove (lakech, mirt etc)" &
                                 hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$refr <- ifelse(hh_asset$asset_cd == "21. Refrigerator" &
                            hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$shelf <- ifelse(hh_asset$asset_cd == "26. Shelf for storing goods" &
                             hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$bio_st <- ifelse(hh_asset$asset_cd == "27. Biogas stove (pit)" &
                              hh_asset$s11q00 == "1. YES", 1,0)
  hh_asset$total <- 1
  
  hh_access <- hh_asset[,c("ea_id","bike", "motor", "cart_h", "cart_a", "car", 
                           "ker_st","cyl_st","ele_st","ele_mitad","energy_st",
                           "refr", "shelf", "bio_st", "total")]
  hh_accessAsset_agg <- aggregate(hh_access, by=list(as.factor(hh_access$ea_id)), sum)
  
  hh_accessAsset_agg$bike_perc <- hh_accessAsset_agg$bike/hh_accessAsset_agg$total
  hh_accessAsset_agg$motor_perc <- hh_accessAsset_agg$motor/hh_accessAsset_agg$total
  hh_accessAsset_agg$cart_h_perc <- hh_accessAsset_agg$cart_h/hh_accessAsset_agg$total
  hh_accessAsset_agg$cart_a_perc <- hh_accessAsset_agg$cart_a/hh_accessAsset_agg$total
  hh_accessAsset_agg$car_perc <- hh_accessAsset_agg$car/hh_accessAsset_agg$total
  
  hh_accessAsset_agg$ker_perc <- hh_accessAsset_agg$ker_st/hh_accessAsset_agg$total
  hh_accessAsset_agg$cyl_perc <- hh_accessAsset_agg$cyl_st/hh_accessAsset_agg$total
  hh_accessAsset_agg$ele_st_perc <- hh_accessAsset_agg$ele_st/hh_accessAsset_agg$total
  hh_accessAsset_agg$ele_mit_perc <- hh_accessAsset_agg$ele_mitad/hh_accessAsset_agg$total
  hh_accessAsset_agg$ene_perc <- hh_accessAsset_agg$energy_st/hh_accessAsset_agg$total
  hh_accessAsset_agg$refr_perc <- hh_accessAsset_agg$refr/hh_accessAsset_agg$total
  hh_accessAsset_agg$shelf_perc <- hh_accessAsset_agg$shelf/hh_accessAsset_agg$total
  hh_accessAsset_agg$bio_perc <- hh_accessAsset_agg$bio_st/hh_accessAsset_agg$total
  
  hh_accessAsset_agg$bike_z <- calc_zScore(hh_accessAsset_agg$bike_perc,hh_accessAsset_agg[,c("bike_perc")])
  hh_accessAsset_agg$motor_z <- calc_zScore(hh_accessAsset_agg$motor_perc,hh_accessAsset_agg[,c("motor_perc")])
  hh_accessAsset_agg$cart_h_z <- calc_zScore(hh_accessAsset_agg$cart_h_perc,hh_accessAsset_agg[,c("cart_h_perc")])
  hh_accessAsset_agg$cart_a_z <- calc_zScore(hh_accessAsset_agg$cart_a_perc,hh_accessAsset_agg[,c("cart_a_perc")])
  hh_accessAsset_agg$car_z <- calc_zScore(hh_accessAsset_agg$car_perc,hh_accessAsset_agg[,c("car_perc")])
  
  hh_accessAsset_agg$ker_z <- calc_zScore(hh_accessAsset_agg$ker_perc,hh_accessAsset_agg[,c("ker_perc")])
  hh_accessAsset_agg$cyl_z <- calc_zScore(hh_accessAsset_agg$cyl_perc,hh_accessAsset_agg[,c("cyl_perc")])
  hh_accessAsset_agg$ele_st_z <- calc_zScore(hh_accessAsset_agg$ele_st_perc,hh_accessAsset_agg[,c("ele_st_perc")])
  hh_accessAsset_agg$ele_mit_z <- calc_zScore(hh_accessAsset_agg$ele_mit_perc,hh_accessAsset_agg[,c("ele_mit_perc")])
  hh_accessAsset_agg$ene_z <- calc_zScore(hh_accessAsset_agg$ene_perc,hh_accessAsset_agg[,c("ene_perc")])
  hh_accessAsset_agg$refr_z <- calc_zScore(hh_accessAsset_agg$refr_perc,hh_accessAsset_agg[,c("refr_perc")])
  hh_accessAsset_agg$shelf_z <- calc_zScore(hh_accessAsset_agg$shelf_perc,hh_accessAsset_agg[,c("shelf_perc")])
  hh_accessAsset_agg$bio_z <- calc_zScore(hh_accessAsset_agg$bio_perc,hh_accessAsset_agg[,c("bio_perc")])
  
  hh_accessAsset_agg$ea_id <- NULL
  names(hh_accessAsset_agg)[1] <- 'ea_id'
  hh_accessAsset_agg$ea_id <- as.character(hh_accessAsset_agg$ea_id)
  
  
  rm(hh_asset,hh_access)
  
  #################################################################################
  ########### CAUSES OF FOOD INSECURITY ##############
  
  hh_foodIns <- read.csv("sect8_hh_w4.csv")
  
  #hh_foodIns <- hh_foodIns[hh_foodIns$s8q06 == "1. YES" ,]
  hh_foodIns <- hh_foodIns[,c("ea_id","s8q06", "s8q08_1",
                              "s8q08_2","s8q08_3")]
  
  FI_expense <- "6. FOOD IN THE MARKET WAS VERY EXPENSIVE"
  FI_transpoCost <- "7. NOT ABLE TO REACH THE MARKET DUE TO HIGH TRANSPORTATION COSTS"
  FI_distance <- "9. MARKET VERY FAR FROM THE VILLAGE"
  
  hh_foodIns$FI_expense <- ifelse(hh_foodIns$s8q08_1 == FI_expense |
                                    hh_foodIns$s8q08_2 == FI_expense |
                                    hh_foodIns$s8q08_3 == FI_expense,1,0)
  hh_foodIns$FI_transpoCost <- ifelse(hh_foodIns$s8q08_1 == FI_transpoCost |
                                        hh_foodIns$s8q08_2 == FI_transpoCost |
                                        hh_foodIns$s8q08_3 == FI_transpoCost,1,0)
  hh_foodIns$FI_dist <- ifelse(hh_foodIns$s8q08_1 == FI_distance |
                                 hh_foodIns$s8q08_2 == FI_distance |
                                 hh_foodIns$s8q08_3 == FI_distance,1,0)
  hh_foodIns$FI <- ifelse(hh_foodIns$s8q06 == "1. YES", 1,0)
  
  hh_foodIns_cause <- hh_foodIns[,c("ea_id","FI_expense","FI_transpoCost","FI_dist","FI")]
  hh_foodIns_agg <- aggregate(hh_foodIns_cause, by=list(hh_foodIns_cause$ea_id), sum)
  hh_foodIns_agg$FI_expense_perc <- hh_foodIns_agg$FI_expense/hh_foodIns_agg$FI
  hh_foodIns_agg$FI_transpoCost_perc <- hh_foodIns_agg$FI_transpoCost/hh_foodIns_agg$FI
  hh_foodIns_agg$FI_dist_perc <- hh_foodIns_agg$FI_dist/hh_foodIns_agg$FI
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  hh_foodIns_agg[is.nan(hh_foodIns_agg)] <- 0
  
  hh_foodIns_agg$FI_expense_z <- calc_zScore(hh_foodIns_agg$FI_expense_perc, hh_foodIns_agg[,c("FI_expense_perc")])
  hh_foodIns_agg$FI_transpoCost_z <- calc_zScore(hh_foodIns_agg$FI_transpoCost_perc, hh_foodIns_agg[,c("FI_transpoCost_perc")])
  hh_foodIns_agg$FI_dist_z <- calc_zScore(hh_foodIns_agg$FI_dist_perc, hh_foodIns_agg[,c("FI_dist_perc")])
  
  hh_foodIns_agg$ea_id <- NULL
  names(hh_foodIns_agg)[1] <- 'ea_id'
  hh_foodIns_agg$ea_id <- as.character(hh_foodIns_agg$ea_id)
  
  rm(hh_foodIns, hh_foodIns_cause)
  #################################################################################
  ########### MOBILE PHONE OWNERSHIP ##############
  
  hh_phone <- read.csv("sect11b1_hh_w4.csv")
  
  hh_phone$ownPhone <- ifelse(hh_phone$s11b_ind_01 == "1. YES", 1,0)
  hh_phone$total <- 1
  hh_phone_own <- hh_phone[,c("ea_id","ownPhone","total")]
  hh_phone_agg <- aggregate(hh_phone_own, by = list(hh_phone_own$ea_id), FUN = sum)
  hh_phone_agg$phone_perc <- hh_phone_agg$ownPhone/hh_phone_agg$total
  hh_phone_agg$phone_z <- calc_zScore(hh_phone_agg$phone_perc, hh_phone_agg[,c("phone_perc")])
  
  hh_phone_agg$ea_id <- NULL
  names(hh_phone_agg)[1] = 'ea_id'
  
  hh_phone_agg <- hh_phone_agg[,c("ea_id","phone_perc","phone_z")]
  hh_phone_agg$ea_id <- as.character(hh_phone_agg$ea_id)
  
  rm(hh_phone_own, hh_phone)
  
  #################################################################################
  ########### ACCESS TO BASIC SERVICES ##############
  
  com_access <- read.csv("sect04_com_w4.csv")
  com_access$ea_id <- as.character(com_access$ea_id)
  
  com_access <- com_access[,c("ea_id", "cs4q01",
                              #"cs4q03","cs4q04__13","cs4q05__13",
                              "cs4q02",
                              "cs4q06", "cs4q09",
                              "cs4q10", "cs4q12b",
                              "cs4q13", "cs4q15")]
  names(com_access) <- c("ea_id",
                         "road_type",
                         #"pass_vehicle",
                         #"pass_publicTranspo",
                         #"pass_lorry",
                         "dist_aspRoad",
                         "dist_bus",
                         "dist_woreda",
                         "fare_woreda",
                         "dist_MUC",
                         "fare_MUC",
                         "dist_markets")
  com_access$road_type <- substring(com_access$road_type,1,1)
  com_access$road_type <- as.integer(com_access$road_type)
  com_access$road_type <- ifelse(com_access$road_type == 1, 3,
                                 ifelse(com_access$road_type == 2, 2,
                                        ifelse(com_access$road_type == 3, 1,
                                               ifelse(com_access$road_type == 4, 0,0))))
  
  #com_access$pass_vehicle <- substring(com_access$pass_vehicle,1,1)
  #com_access$pass_vehicle <- as.integer(com_access$pass_vehicle)
  #com_access$pass_vehicle <- ifelse(com_access$pass_vehicle==1,1,0)
  
  #com_access$pass_publicTranspo <- substring(com_access$pass_publicTranspo,1,1)
  #com_access$pass_publicTranspo <- as.integer(com_access$pass_publicTranspo)
  #com_access$pass_publicTranspo <- ifelse(com_access$pass_publicTranspo==1,1,0)
  
  #com_access$pass_lorry <- substring(com_access$pass_lorry,1,1)
  #com_access$pass_lorry <- as.integer(com_access$pass_lorry)
  #com_access$pass_lorry <- ifelse(com_access$pass_lorry==1,1,0)
  
  # NAs are caused by the town being within the woreda, so distance/fare is set to 0
  com_access$dist_woreda[is.na(com_access$dist_woreda)] <- 0
  com_access$dist_MUC[is.na(com_access$dist_MUC)] <- 0
  com_access$dist_markets[is.na(com_access$dist_markets)] <- 0
  com_access$fare_woreda[is.na(com_access$fare_woreda)] <- 0
  com_access$fare_MUC[is.na(com_access$fare_MUC)] <- 0
  
  com_access$road_type_z <- calc_zScore(com_access$road_type, com_access[,c("road_type")])
  com_access$dist_aspRoad_z <- calc_zScore(com_access$dist_aspRoad, com_access[,c("dist_aspRoad")])
  com_access$dist_bus_z <- calc_zScore(com_access$dist_bus, com_access[,c("dist_bus")])
  com_access$dist_woreda_z <- calc_zScore(com_access$dist_woreda, com_access[,c("dist_woreda")])
  com_access$fare_woreda_z <- calc_zScore(com_access$fare_woreda, com_access[,c("fare_woreda")])
  com_access$dist_MUC_z <- calc_zScore(com_access$dist_MUC, com_access[,c("dist_MUC")])
  com_access$fare_MUC_z <- calc_zScore(com_access$fare_MUC, com_access[,c("fare_MUC")])
  com_access$dist_Mkt_z <- calc_zScore(com_access$dist_markets, com_access[,c("dist_markets")])
  
  #################################################################################
  ########### BASIC INFO ##############
  
  com_LULC <- read.csv("sect03_com_w4.csv")
  com_LULC$ea_id <- as.character(com_LULC$ea_id)
  
  com_LULC <- com_LULC[,c("ea_id",
                          "cs3q02",
                          "cs3q03",
                          "cs3q07",
                          "cs3q08")]
  names(com_LULC) <- c("ea_id",
                       'popn',
                       'num_hh',
                       'LULC',
                       'slope')
  
  com_LULC$LULC <- substring(com_LULC$LULC,1,1)
  com_LULC$LULC <- as.integer(com_LULC$LULC)
  com_LULC$LULC <- as.factor(com_LULC$LULC)
  
  com_LULC$slope <- substring(com_LULC$slope,1,1)
  com_LULC$slope <- as.integer(com_LULC$slope)
  com_LULC$slope <- ifelse(com_LULC$slope == 1, 4,
                           ifelse(com_LULC$slope == 2, 3,
                                  ifelse(com_LULC$slope == 5, 2,
                                         ifelse(com_LULC$slope == 3, 1,
                                                ifelse(com_LULC$slope == 4, 0,0)))))
  
  com_LULC$popn_z <- calc_zScore(com_LULC$popn,com_LULC[,c("popn")])
  com_LULC$num_hh_z <- calc_zScore(com_LULC$num_hh,com_LULC[,c("num_hh")])
  com_LULC$slope_z <- calc_zScore(com_LULC$slope,com_LULC[,c("slope")])
  
  #################################################################################
  ########### ACCESS TO ASSISTANCE ##############
  
  hh_assist <- read.csv("sect14_hh_w4.csv")
  
  #hh_assist <- hh_assist[hh_assist$s14q01 == "1. YES",]
  
  hh_assist <- hh_assist[,c("ea_id",
                            "s14q03","s14q05","s14q06")]
  
  # set NA value of assistance to 0
  hh_assist[is.na(hh_assist)] <- 0
  
  hh_assist_agg <- aggregate(hh_assist, by=list(hh_assist$ea_id), sum)
  hh_assist_agg$total_assist <- hh_assist_agg$s14q03+
    hh_assist_agg$s14q05+
    hh_assist_agg$s14q06
  hh_assist_agg$ea_id <- NULL
  names(hh_assist_agg)[1] <- 'ea_id'
  
  hh_assist_agg <- hh_assist_agg[,c("ea_id", "total_assist")]
  hh_assist_agg$assist_z <- calc_zScore(hh_assist_agg$total_assist,hh_assist_agg[,c("total_assist")])
  
  hh_assist_agg <- hh_assist_agg[,c("ea_id","total_assist", "assist_z")]
  hh_assist_agg$ea_id <- as.character(hh_assist_agg$ea_id)
  
  rm(hh_assist)
  
  #################################################################################
  ########### LIVESTOCK CONSUMPTION ##############
  
  #percentage of HH in community that use livestock grown for own consumption
  
  hh_ls <- read.csv("sect8_1_ls_w4.csv")
  
  hh_ls$ls <- ifelse(hh_ls$ls_s8_1q06 == "3. FOOD FOR THE FAMILY", 1,0)
  hh_ls$total <- 1
  
  hh_ls <- hh_ls[,c("ea_id","ls","total")]
  hh_ls_agg <- aggregate(hh_ls, by=list(hh_ls$ea_id), sum)
  hh_ls_agg$ls_perc <- hh_ls_agg$ls/hh_ls_agg$total 
  hh_ls_agg$ls_z <- calc_zScore(hh_ls_agg$ls_perc,hh_ls_agg[,c("ls_perc")])
  
  hh_ls_agg$ea_id <- NULL
  names(hh_ls_agg)[1] <- 'ea_id'
  hh_ls_agg <- hh_ls_agg[,c("ea_id","ls_perc", "ls_z")]
  hh_ls_agg$ea_id <- as.character(hh_ls_agg$ea_id)
  
  rm(hh_ls)
  
  #################################################################################
  ########### CROP CONSUMPTION ##############
  
  hh_crop <- read.csv("sect11_ph_w4.csv")
  
  hh_crop <- hh_crop[,c("ea_id","s11q01","s11q27b")]
  names(hh_crop) <- c('ea_id','crop','crop_cons_perc')
  hh_crop$crop <- substring(hh_crop$crop,1,1)
  hh_crop$crop <- as.integer(hh_crop$crop)
  
  hh_crop <- hh_crop[hh_crop$crop < 9,]
  
  hh_crop$ea_id <- as.character(hh_crop$ea_id)
  hh_crop$crop_cons_perc[is.na(hh_crop$crop_cons_perc)] <- 0
  
  hh_crop_cons_agg <- aggregate(hh_crop$crop_cons_perc, by=list(hh_crop$ea_id, hh_crop$crop), mean)
  
  ## create dummy columns per crop
  # not done!!!
  
  hh_crop_cons_agg$ea_id <- NULL
  names(hh_crop_cons_agg) <- c('ea_id','crop_cons_perc')
  hh_crop_cons_agg$ea_id <- as.character(hh_crop_cons_agg$ea_id)
  
  rm(hh_crop)
  
  #################################################################################
  ########### MILK CONSUMPTION ##############
  
  #percentage of HH in community that use milk produced for own consumption
  
  hh_milk <- read.csv("sect8_4_ls_w4.csv")
  
  hh_milk$milk <- ifelse(hh_milk$ls_s8_4q06 == "1. SELF CONSUMPTION", 1,0)
  hh_milk$total <- 1
  
  hh_milk <- hh_milk[,c("ea_id","milk", "total")]
  hh_milk_agg <- aggregate(hh_milk, by=list(hh_milk$ea_id), sum)
  hh_milk_agg$milk_perc <- hh_milk_agg$milk/hh_milk_agg$total
  
  hh_milk_agg$ea_id <- NULL
  names(hh_milk_agg)[1] <- 'ea_id'
  hh_milk_agg <- hh_milk_agg[,c("ea_id","milk_perc")]
  hh_milk_agg$milk_z <- calc_zScore(hh_milk_agg$milk_perc, hh_milk_agg[,c("milk_perc")])
  
  hh_milk_agg$ea_id <- as.character(hh_milk_agg$ea_id)
  
  rm(hh_milk)
  
  #################################################################################
  ########### MARKET PRICES ##############
  
  #################################################################################
  ########### MARKET PRICES ##############
  
  com_mkt <- read.csv("./sect10b_com_w4.csv")
  
  com_mkt <- com_mkt[,c(1,2,4,13,14,15)]
  names(com_mkt) <- c('ea_id', 'crop','region', 'unit', 'o_weight','o_price')
  #input_crop <- c(1:16)
  #com_mkt <- com_mkt[com_mkt$crop %in% input_crop,] # limit only to crop 1-X
  com_mkt$crop_group <- ifelse(com_mkt$crop %in% c(1:6,60), 1,
                        ifelse(com_mkt$crop %in% c(7:11,110,111), 2,
                        ifelse(com_mkt$crop %in% c(12,13,131), 3,
                        ifelse(com_mkt$crop %in% c(14,143:145), 4,
                        ifelse(com_mkt$crop %in% c(15,151,152), 5,
                        ifelse(com_mkt$crop %in% c(16,170:173), 6, 7))))))
  com_mkt <- com_mkt[com_mkt$crop_group < 7,]
  com_mkt <- na.omit(com_mkt) # remove NAs/market does not sell this crop
  com_mkt$ea_id <- as.character(com_mkt$ea_id)
  
  
  # weight conversion
  food_conv <- read.csv("./Food_CF_Wave4.csv")
  #food_conv <- food_conv[food_conv$item_cd %in% input_crop,]
  
  com_mkt$weightKG <- 0
  for(i in unique(com_mkt$region)){
    for(j in unique(com_mkt[com_mkt$region==i,]$crop)){
      for(k in unique(com_mkt[com_mkt$region==i & com_mkt$crop==j,]$unit)){
        # setting conversion configuration
        reg_ind <- ifelse(i < 8, i+4, # i.e. if region = 1, use variable 5 (mean_cf1)
                          ifelse(i == 12, 11,
                                 ifelse(i==5|i==13|i==15, 12, 4))) 
        # i.e. if region = 5, use var 12 (mean_cf99) else use var 4 (mean_cf_nat)
        
        conv <- ifelse(i>7&i!=12, # safety ifelse for other possible values. Will use national cf
                       food_conv[food_conv$item_cd_cf==j&food_conv$unit_cd==k,]$mean_cf_nat,
                       na.omit(food_conv[food_conv$item_cd_cf==j&food_conv$unit_cd==k,reg_ind])) 
        #set cf to be used in next step. Remove NAs to match market price data
        
        if(!is.na(conv)){
          # actual conversion if conversion units are valid
          #print(paste(i,j,k,reg_ind,conv))
          com_mkt[com_mkt$region==i&com_mkt$crop==j&com_mkt$unit==k,]$weightKG <- 
            com_mkt[com_mkt$region==i&com_mkt$crop==j&com_mkt$unit==k,]$o_weight*conv
        }else{#remove invalid conversion units (i.e. bananas in litres)
          com_mkt <- com_mkt[-c(com_mkt$region==i&com_mkt$crop==j&com_mkt$unit==k),]
        }
      }
    }
  }
  
  # data cleaning
  com_mkt$pricePerKg <- com_mkt$o_price/com_mkt$weightKG
  com_mkt <- com_mkt[com_mkt$pricePerKg < 1000 & com_mkt$pricePerKg > 0.1,] # remove outliers
  # end of data cleaning
  
  # using mean prices per crop group
  mkt_agg <- aggregate(com_mkt$pricePerKg, 
                       by=list(com_mkt$ea_id, com_mkt$crop_group), mean)
  names(mkt_agg)[1] <- 'ea_id'
  names(mkt_agg)[2] <- 'crop_group'
  names(mkt_agg)[3] <- 'avg_price'
  
  for(i in seq(unique(mkt_agg$crop_group))){
    curr_crop_price <- paste0('price_',unique(mkt_agg$crop_group)[i])
    mkt_agg[[curr_crop_price]] <- 0
  }
  
  for(i in seq(1,nrow(mkt_agg))){
    curr_ea <- mkt_agg[i,]$ea_id
    curr_crop <- mkt_agg[i,]$crop_group
    curr_price <- mkt_agg[i,]$avg_price
    
    target_crop <- paste0('price_',as.character(curr_crop))
    mkt_agg[mkt_agg$ea_id==curr_ea,][[target_crop]] <- curr_price
  }
  
  mkt_agg$crop_group <- mkt_agg$avg_price <- NULL
  mkt_complete <- aggregate(mkt_agg[,2:7], by=list(mkt_agg$ea_id), mean)
  names(mkt_complete)[1] <- 'ea_id'
  rm(list=c('food_conv','mkt_agg'))
  # using specific market prices
  # mkt_agg <- aggregate(com_mkt$ea_id, by=list(com_mkt$ea_id), mean)
  # mkt_agg$x <- NULL
  # names(mkt_agg)[1] <- 'ea_id'
  # for(i in seq(1,length(unique(com_mkt$crop)))){
  #   curr_crop_price <- paste0('price_',unique(com_mkt$crop)[i])
  #   mkt_agg[[curr_crop_price]] <- 0
  # }
  # 
  # for(i in seq(1,nrow(com_mkt))){
  #   curr_ea <- com_mkt[i,]$ea_id
  #   curr_crop <- com_mkt[i,]$crop
  #   curr_price <- com_mkt[i,]$pricePerKg
  # 
  #   target_crop <- paste0('price_',as.character(curr_crop))
  #   mkt_agg[mkt_agg$ea_id==curr_ea,][[target_crop]] <- curr_price
  # }
  # nrow(mkt_agg[mkt_agg$price_2==0,]) # 120 community markets don't sell wheat (or no data)
  # nrow(mkt_agg[mkt_agg$price_4==0,]) # 27 community markets don't sell maize (or no data)
  # 
  # mkt_complete <- mkt_agg[mkt_agg$price_2>0&
  #                           mkt_agg$price_4>0,]
  # names(mkt_complete)[2] <- 'price_wheat'
  # names(mkt_complete)[3] <- 'price_maize'
  
  #################################################################################
  ########### GEOVARIABLES ##############
  
  hh_geo <- read.csv("ETH_HouseholdGeovariables_Y4.csv")
  #hh_geo$ea_id <- as.character(hh_geo$ea_id)
  
  hh_geo_agg <- aggregate(hh_geo[,-2], by=list(hh_geo$ea_id), mean)
  hh_geo_agg$household_id <- NULL
  names(hh_geo_agg)[1] <- 'ea_id'
  
  rm(hh_geo)
  
  ################################################################################
  ########### JOIN SURVEY DATA WITH GEOPOINTS ##############
  
  hh_geo_loc <- hh_geo_agg[,c('ea_id', 'lat_mod', 'lon_mod')]
  
  com_joined <- merge(x = hh_geo_loc, 
                      y = hh_geo_agg[,c(-40,-41)], by = "ea_id", all.x = TRUE)
  
  com_joined <- merge(x = com_joined, 
                      y = com_access, by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = com_LULC, by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = mkt_complete, by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = hh_accessAsset_agg[,!(colnames(hh_accessAsset_agg) == "total")], 
                      by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = hh_assist_agg, 
                      by = "ea_id", all.x = TRUE)
  #com_joined <- merge(x = com_joined, 
  #                    y = hh_crop_cons_agg[,!(colnames(hh_crop_cons_agg) == "total")], 
  #                    by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = hh_foodIns_agg, 
                      by = "ea_id", all.x = TRUE)
  #com_joined <- merge(x = com_joined, 
  #                    y = hh_ls_agg[,!(colnames(hh_ls_agg) == "total")], 
  #                    by = "ea_id", all.x = TRUE)
  #com_joined <- merge(x = com_joined, 
  #                    y = hh_milk_agg[,!(colnames(hh_milk_agg) == "total")], 
  #                    by = "ea_id", all.x = TRUE)
  com_joined <- merge(x = com_joined, 
                      y = hh_phone_agg[,!(colnames(hh_phone_agg) == "total")], 
                      by = "ea_id", all.x = TRUE)
  com_joined <- com_joined[is.na(com_joined$lat_mod)==FALSE,]
  com_joined_complete <- com_joined[complete.cases(com_joined),]
  save.image(file='LSMS_prepData.RData')
}else{
  load('LSMS_prepData.RData')
}
sapply(com_joined, function(x) sum(is.na(x)))
write.csv(com_joined, "LSMS_com_geo_v4.csv")
write.csv(com_joined_complete, "LSMS_com_geo_complete_v4.csv")

########### END OF DATA PREPARATION ##############
################################################################################




