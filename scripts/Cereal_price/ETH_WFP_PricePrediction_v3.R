# Cereal price prediction in R
# 
# Florencio Campomanes V
# 2022-05-04
#
# This script requires the ff R package/s:
#   randomForest - price prediction for cereal price
#
# This script requires the ff datasets:
#   (1) WFPVAM global food price dataset (Source: https://data.humdata.org/dataset/wfp-food-prices)
#   (2) Monthly CPI data (Source: https://www.fao.org/faostat/en/#data/CP)
#   (3) Monthly precipitation, travel time to cities, spatially joined with market and price data (for training)
#   (4) Monthly precipitation, travel time to cities, spatially joined with input grids (for prediction)
#
# Notes:
# (a) All file paths and names should be changed as needed
#

setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")
library(randomForest)
library(ggplot2)
library(ggthemr)
library(cowplot)
library(plotly)
library(stringr)
library(tidyverse)
ggthemr("fresh")

rm(list=ls(all=TRUE))

# Load saved data (no need to run rest of script)
# load('./Markets/SpatPred/ETH_Monthly_Price_v4.RData')

# read WFP VAM global food prices
d <- read.csv('./Markets/wfpvam_foodprices.csv')

# filter to only Ethiopia
d <- d[d$adm0_name=="Ethiopia",]

# remove duplicate data
crop_df <- d[,c('cm_id','cm_name')]
crop_df <- crop_df[!duplicated(crop_df),]

# get indices of specific crops in each crop group
cereal <- c(str_which(unique(crop_df$cm_name), 'Barley'),
            str_which(unique(crop_df$cm_name), 'Maize'),
            str_which(unique(crop_df$cm_name), 'Millet'),
            str_which(unique(crop_df$cm_name), 'Sorghum'),
            # str_which(unique(crop_df$cm_name), 'Teff'),
            str_which(unique(crop_df$cm_name), 'Wheat'),
            str_which(unique(crop_df$cm_name), 'Rice'))
pulse <- c(str_which(unique(crop_df$cm_name), 'Beans'),
           str_which(unique(crop_df$cm_name), 'Chickpeas'),
           str_which(unique(crop_df$cm_name), 'Lentils'),
           str_which(unique(crop_df$cm_name), 'Peas'))
oilseed <- c(str_which(unique(crop_df$cm_name), 'Lin seed'),
             str_which(unique(crop_df$cm_name), 'Niger seed'),
             str_which(unique(crop_df$cm_name), 'Sesame'),
             str_which(unique(crop_df$cm_name), 'Rape seed'))
veg <- c(str_which(unique(crop_df$cm_name), 'Beetroot'),
         str_which(unique(crop_df$cm_name), 'Cabbage'),
         str_which(unique(crop_df$cm_name), 'Carrots'),
         str_which(unique(crop_df$cm_name), 'Garlic'),
         str_which(unique(crop_df$cm_name), 'Kale'),
         str_which(unique(crop_df$cm_name), 'Lettuce'),
         str_which(unique(crop_df$cm_name), 'Onions'),
         str_which(unique(crop_df$cm_name), 'Peppers'),
         str_which(unique(crop_df$cm_name), 'Swiss chard'),
         str_which(unique(crop_df$cm_name), 'Tomatoes'))
frt <- c(str_which(unique(crop_df$cm_name), 'Avocados'),
         str_which(unique(crop_df$cm_name), 'Bananas'),
         str_which(unique(crop_df$cm_name), 'Guava'),
         str_which(unique(crop_df$cm_name), 'Mangoes'),
         str_which(unique(crop_df$cm_name), 'Oranges'),
         str_which(unique(crop_df$cm_name), 'Papaya'))
tuber <- c(str_which(unique(crop_df$cm_name), 'Potatoes'),
           str_which(unique(crop_df$cm_name), 'Sweet potatoes'),
           str_which(unique(crop_df$cm_name), 'Taro'))

# create data frame for each crop group using indices taken above
cereal_id <- crop_df[cereal,]$cm_id
pulse_id <- crop_df[pulse,]$cm_id
oilseed_id <- crop_df[oilseed,]$cm_id
veg_id <- crop_df[veg,]$cm_id
frt_id <- crop_df[frt,]$cm_id
tuber_id <- crop_df[tuber,]$cm_id

# flush indices (memory management)
rm(list=(c('cereal','pulse','oilseed','veg',
           'frt','tuber','crop_df')))

# give crop group IDs
d$crop_group <- ifelse(d$cm_id %in% cereal_id, 1,
                ifelse(d$cm_id %in% pulse_id, 2,
                ifelse(d$cm_id %in% oilseed_id, 3,
                ifelse(d$cm_id %in% veg_id, 4,
                ifelse(d$cm_id %in% frt_id, 5,
                ifelse(d$cm_id %in% tuber_id, 6,7))))))

#croplist <- c('Maize (white) - Retail')
#d <- d[d$cm_name %in% croplist,]
#d <- d[d$mp_year > 2015,]

d$yrmonth <- paste0(as.character(d$mp_year),'-',as.character(d$mp_month))
d <- d[d$um_name=='KG'|d$um_name=='100 KG',]

# flatten price with monthly CPI
cpi_df <- read.csv('./Markets/CPI/FAOSTAT_data_5-4-2022.csv')
# cpi_df <- na.omit(cpi_df)
cpi_df$month_num <- as.integer(factor(cpi_df$Months, levels = month.name))
cpi_df$yrmonth <- paste0(cpi_df$Year,'-',cpi_df$month_num)
names(cpi_df)[names(cpi_df)=='Value'] <- 'CPI'
# names(cpi_df)[names(cpi_df)=='year'] <- "mp_year"

d_cpi <- left_join(d, cpi_df[,c('yrmonth', 'CPI')], by="yrmonth")
d_cpi <- d_cpi[!is.na(d_cpi$CPI),]
d_cpi[d_cpi$mp_price>50,]$um_name <- '100 KG'
d_cpi$price_adj <- d_cpi$mp_price/(d_cpi$CPI/100)
names(d_cpi)[names(d_cpi)=='mkt_name'] <- 'market'
d_cpi <- d_cpi[d_cpi$crop_group==1,]

# write.csv(d_cpi,'./Markets/ETH_dCPI.csv')

# aggregate (mean) individual crop prices by crop group
d_cpi_agg <- aggregate(d_cpi$price_adj, by=list(d_cpi$market,
                                      d_cpi$mp_year,
                                      d_cpi$mp_month,
                                      d_cpi$crop_group,
                                      d_cpi$um_name), mean)
names(d_cpi_agg) <- c('market','year','month','crop_group','um_name','price_adj')

#### Wholesale to retail price adjustment ####
# if market has both wholesale and retail prices,
# convert to retail based on relationship of existing prices in that market
toRetail <- function(df){
  sum <- 0
  d_crop <- df[df$crop_group==1,]
  wrt_df <- data.frame()
  l_col <- c('market','crop_group','price_adj')
  for(i in seq(1,nrow(d_crop))){
    curr <- d_crop[i,]
    curr_mkt <- curr$market
    curr_year <- curr$year
    curr_month <- curr$month
    if(curr$um_name=="100 KG"){
      w <- curr[,l_col]
      rt <- d_crop[d_crop$um_name=="KG"&
                     d_crop$market==curr_mkt&
                     d_crop$year==curr_year&
                     d_crop$month==curr_month,l_col]
      if(nrow(rt)>0){
        sum <- sum +1
        names(rt)[3] <- 'r_price'
        temp <- data.frame(rt)
        temp$w_price <- w$price_adj
        # names(temp)[ncol(temp)] <- 'w_price'
        wrt_df <- rbind(wrt_df,temp)
      }
    }
  }
  wrt_df$w_price_adj <- wrt_df$w_price/100
  # linear model to adjust wholesale to retail price
  lm.rt <- lm(wrt_df$r_price ~ wrt_df$w_price_adj)
  cereal_wrt_cf <- lm.rt$coefficients[2]
  interc <- lm.rt$coefficients[1]
  # rm(list=c('rt','w','wrt_df','temp','d_crop',
  #           'curr','curr_mkt','curr_yr','i','lm.rt','l_col'))
  df$rt_price <- df$price_adj
  df[df$crop_group==1&
       df$um_name=='100 KG',]$rt_price <- 
  (df[df$crop_group==1&
        df$um_name=='100 KG',]$price_adj)*cereal_wrt_cf
  df[df$price_adj > 1 & df$um_name=='KG'&
       df$crop_group==1,]$rt_price <-
  (df[df$price_adj > 1 & df$um_name=='KG'&
        df$crop_group==1,]$price_adj)*cereal_wrt_cf
  print(sum)
  ret_list <- list(cereal_wrt_cf,df,wrt_df,interc, lm.rt)
  return(ret_list)
}
toRet <- toRetail(d_cpi_agg)
cereal_wrt <- toRet[[1]]
d_cpi_agg <- toRet[[2]]
wrt_df <- toRet[[3]]
cereal_int <- toRet[[4]]
lm.rt <- toRet[[5]]

d_agg <- aggregate(d_cpi_agg$rt_price, 
                        by=list(d_cpi_agg$market,d_cpi_agg$year,
                        d_cpi_agg$crop_group, d_cpi_agg$um_name), mean)
d_agg_month <- aggregate(d_cpi_agg$rt_price, 
                         by=list(d_cpi_agg$market,d_cpi_agg$month,
                                d_cpi_agg$crop_group, d_cpi_agg$um_name), mean)
names(d_agg) <- c('market','year','crop_group','weight','price')
names(d_agg_month) <- c('market','month','crop_group','weight','price')

d_cpi_agg_whole <- d_cpi_agg[d_cpi_agg$um_name=='100 KG',]
d_cpi_agg_rt <- d_cpi_agg[d_cpi_agg$um_name=='KG',] 

# scatterplot of wholesale to retail conversion
cereat_wrt_plot <- ggplot(wrt_df, aes(x=w_price_adj,y=r_price))+
                  geom_point()+
                  geom_abline(intercept=cereal_int,
                              slope=cereal_wrt, size=2, color="black")+
                  
                  xlab("Wholesale cereal price")+
                  ylab("Retail cereal price")
wrt_rmse <- sqrt(mean(lm.rt$residuals^2))
summary(lm.rt)
cereat_wrt_plot
ggsave('./Markets/SpatPred/plots/WRT_CRP_plot.png',
       width=6,height=4,units="in")
# plot(as.factor(cpi_df$year),cpi_df$CPI)

# plot inter-annual CPI-deflated price
d_annual <- aggregate(d_agg$price, 
                     by=list(d_agg$market, d_agg$year), 
                     FUN=mean)
names(d_annual) <- c('market','year','price')
plot(d_annual$year, d_annual$price)
d_annual_mean <- aggregate(d_annual$price, by=list(d_annual$year), mean)
names(d_annual_mean) <- c('year', 'mean_price')
spline.price <- as.data.frame(spline(d_annual_mean$year,
                                     d_annual_mean$mean_price))
ann_price_plot <- ggplot(spline.price,aes(x=x,y=y))+
                  geom_line(size=1.5)+
                  xlab("Year")+
                  ylab("Mean CPI adjusted Cereal price")
ann_price_plot
ggsave('./Markets/SpatPred/plots/annual_mean_CRP_plot.png',
       width=8,height=4,units="in")

# plot intra-annual CPI-deflated price
d_month <- aggregate(d_agg_month$price, 
                      by=list(d_agg_month$market,d_agg_month$month), 
                      FUN=mean)
names(d_month) <- c('market','month','price')
boxplot(d_month$price~d_month$month)
d_month_mean <- aggregate(d_month$price, by=list(d_month$month), mean)
names(d_month_mean) <- c('month', 'mean_price')
m_spline.price <- as.data.frame(spline(d_month_mean$month,
                                       d_month_mean$mean_price))
m_price_plot <- ggplot()+
  geom_boxplot(aes(x=as.factor(month),y=price),data=d_month)+
  # geom_line(data=m_spline.price,aes(x=x,y=y),size=1.5, color="black")+
  xlab("Month")+
  ylab("Mean CPI adjusted Cereal price")
m_price_plot
ggsave('./Markets/SpatPred/plots/month_CRP_boxplot.png',
       width=8,height=4,units="in")
write.csv(d_month, './Markets/ETH_WFP_CerealPrice_month_v3.csv')

# after spatial joining monthly price with input grid in GIS
# RF prediction

# rm(list=ls(all=TRUE))

markets <- read.csv('./Markets/ETH_MarketInfo_Precip_v2.csv')
mktPrice_input <- left_join(d_month, markets[,c(-1)], by=c("market"="MarketName"))

# restructuring monthly price data frame
mktPrice_input$m_precip <- ifelse(mktPrice_input$month==1, mktPrice_input$p_01,
                            ifelse(mktPrice_input$month==2, mktPrice_input$p_02,
                             ifelse(mktPrice_input$month==3, mktPrice_input$p_03,
                              ifelse(mktPrice_input$month==4, mktPrice_input$p_04,
                               ifelse(mktPrice_input$month==5, mktPrice_input$p_05,
                                ifelse(mktPrice_input$month==6, mktPrice_input$p_06,
                                 ifelse(mktPrice_input$month==7, mktPrice_input$p_07,
                                  ifelse(mktPrice_input$month==8, mktPrice_input$p_08,
                                   ifelse(mktPrice_input$month==9, mktPrice_input$p_09,
                                    ifelse(mktPrice_input$month==10, mktPrice_input$p_10,
                                     ifelse(mktPrice_input$month==11, mktPrice_input$p_11,
                                      ifelse(mktPrice_input$month==12, mktPrice_input$p_12,
                                                                        mktPrice_input$p_00))))))))))))
# mktPrice_input$m_precip <- mktPrice_input$p_00
p_vars <- array(dim=12)
for(i in 1:12){
  if(i<10){
    m <- paste0('p_0',as.character(i))
  }else{
    m <- paste0('p_',as.character(i))
  }
  p_vars[i] <- m
}
mktPrice_vars <- c('market','month','price','m_precip',
                   'TTC2','TTC4','TTC7','popden','laty','lonx')
mktPrice_input <- mktPrice_input[,mktPrice_vars]
mktPrice_input$month <- as.factor(mktPrice_input$month)

# only use these variables (removed laty and lonx)
price_pred <- c('m_precip','TTC2','TTC4','TTC7','popden')
mktPrice_input <- mktPrice_input[mktPrice_input$laty>0,]
mktPrice_input <- na.omit(mktPrice_input)

train_val_split <- function(df, split=0.3){
  set.seed(123)
  df$id <- paste0(df$market, df$month)
  #take 20% of samples for validation for each class
  vdf <- df[sample(nrow(df)*split, replace=F),]
  
  #take remaining samples for training
  tdf <- subset(df, !(id %in% vdf$id)) 
  
  d_list <- list(tdf, vdf)
  return(d_list)
}

# split dataset for training and validation
d_list <- train_val_split(mktPrice_input)
mktPrice_t <- d_list[[1]]

# proceed only using training dataset
mktPrice_pred <- mktPrice_t[,price_pred]

# initial value of mTry as sqrt of number of predictor vars
mtry <- floor(sqrt(length(mktPrice_pred)))
ntree <- 1000
# find best mTry
r_forest_tune <- randomForest::tuneRF(mktPrice_pred,mktPrice_t$price, 
                                      mtryStart = mtry, 
                                      ntreeTry = ntree, 
                                      doBest=TRUE)
best.m <- r_forest_tune$mtry
month_price_rf <- mktPrice_t[,c('price',price_pred)]
pr_rf_oob <- vector(length=100)

# 100 repetitions to address random nature of RF
for(i in seq(100)){
  price_rf_temp <- randomForest(price~.,data=month_price_rf,
                           mtry=2, importance=TRUE)
  pr_rf_oob[i] <- price_rf_temp$rsq
}
mean_oob_r2 <- mean(pr_rf_oob)
price_rf <- randomForest(price~.,data=month_price_rf,
                            mtry=2, importance=TRUE)
price_rf$importance
price_rf

# plot metrics
y_price <- mktPrice_t$price
plot_obsFit <- function(y_obs, y_pred){
  pred_data <- as.data.frame(cbind(y_obs,y_pred))
  names(pred_data)[1] <- 'Observed'
  names(pred_data)[2] <- 'Predicted'
  
  pl_cor <- ggplot(pred_data, aes(Observed, Predicted)) + 
    geom_point()+
    geom_abline(color="black",size=1)+
    coord_cartesian(xlim=c(0,0.2),ylim=c(0,0.2))+
    annotate(geom="text",
    x=0.02, y=0.15, label="OOB R2: 0.5
    RMSE: 0.012
    nRMSE: 15.5%")
  return(pl_cor)
}
y_pred <- price_rf$predicted
pl_cor_t <- plot_obsFit(y_price , y_pred)
pl_cor_t
range(y_price)
range(y_pred)
t_mse <- mean((y_price-y_pred)^2)
t_rmse <- sqrt(t_mse)
t_mae <- mean(abs(y_price-y_pred))
t_corrXY <- cor(y_price,y_pred)
t_rsqXY <- t_corrXY^2

###################### VALIDATION ################

mktPrice_v <- d_list[[2]]
mktPrice_val <- mktPrice_v[,price_pred]

val_price <- predict(price_rf,mktPrice_val)
y_price_val <- mktPrice_v$price
pl_cor_v <- plot_obsFit(y_price_val , val_price)
pl_cor_v
ggsave('./Markets/SpatPred/plots/CRP_V_fitplots.png',
       width=7,height=4,units="in")

v_mse <- mean((y_price_val-val_price)^2)
v_rmse <- sqrt(t_mse)
v_mae <- mean(abs(y_price_val-val_price))
v_corrXY <- cor(y_price_val,val_price)
v_rsqXY <- v_corrXY^2

norm_t_rmse <- (t_rmse/(max(y_pred)-min(y_pred)))
norm_v_rmse <- (v_rmse/(max(val_price)-min(val_price)))
norm_t_mae <- (t_mae/(max(y_pred)-min(y_pred)))
norm_v_mae <- (v_mae/(max(val_price)-min(val_price)))

#### Performance plots ####
plot_grid(pl_cor_t, pl_cor_v, labels="AUTO", ncol=2)
# ggsave('./RandomForest/RF_plots_1km/Fit_plots_v3.png',
#        width=10,height=4,units="in")

pl_t_err <- plot_error(y_price,y_pred,sq=1)
pl_v_err <- plot_error(y_price_val,val_price, sq=1)
# pl_v_err
plot_grid(pl_t_err,pl_v_err, labels="AUTO", ncol=2)
# ggsave('./RandomForest/RF_plots_1km/SqError_plots_v2.png',
#        width=10,height=4,units="in")
pl_v_sqerr <- plot_error(y_price_val,val_price, sq=0)
pl_t_sqerr <- plot_error(y_price,y_pred, sq=0)
plot_grid(pl_t_sqerr,pl_v_sqerr, labels="AUTO", ncol=2)
# ggsave('./RandomForest/RF_plots_1km/Error_plots_v3.png',
#        width=10,height=4,units="in")

# Create a dataframe summary of the metrics
rf_metrics <- data.frame(matrix(NA, nrow = 2, ncol = 5))
names(rf_metrics) <- c('crop','mse','rmse','mae','rsq')
for(i in seq(1,2)){
  y_pred <- rfs[[i]]$predicted
  if(i == 1){
    y_price <- y_price_1
  }else{
    y_price <- y_price_2
  }
  pl_cor <- plot_obsFit(y_price, y_pred)
  print(pl_cor)
  range(y_price)
  range(y_pred)
  t_mse <- mean((y_price-y_pred)^2)
  t_rmse <- sqrt(t_mse)
  t_mae <- mean(abs(y_price-y_pred))
  corrXY <- cor(y_price,y_pred)
  rsqXY <- corrXY^2
  rf_metrics[i, ] <- list(i,t_mse,t_rmse,t_mae,corrXY)
}

#### Predict monthly cereal price for all of Ethiopia ####
test <- read.csv('./Data_gridded_1km/value_grids/ETH_valueGrid1km_v8.csv')
out <- data.frame(matrix(nrow=0,ncol=ncol(mktPrice_input)))
for(i in 1:12){
  if(i<10){
    m <- paste0('p0',as.character(i))
  }else{
    m <- paste0('p',as.character(i))
  }
  
  month_df <- test[,c('c_id',m,price_pred[!(price_pred) %in% c('m_precip','month')])]
  names(month_df)[names(month_df)==m] <- 'm_precip'
  month_df$month <- as.factor(i)
  month_df$month <- factor(month_df$month,levels=c("1","2","3","4",
                                "5","6","7","8",
                                "9","10","11","12"))
  month_df$price <- predict(price_rf,month_df[,!names(month_df) %in% 'c_id'])
  write.csv(month_df,paste0('./Markets/SpatPred/monthlyPrice/ETH_1km_Price_month',substr(m,2,4),'_v2.csv'))
  out <- rbind(out,month_df)
}
out_agg <- aggregate(out$price, by=list(out$c_id), median)
names(out_agg) <- c('c_id','price')
out_agg$month <- as.factor(0)
out$month <- factor(out$month,levels=c("0","1","2","3","4",
                                                 "5","6","7","8",
                                                 "9","10","11","12"))
out_agg$month <- factor(out_agg$month,levels=c("0","1","2","3","4",
                                       "5","6","7","8",
                                       "9","10","11","12"))

out_final <- rbind(out[,c('c_id','price','month')], out_agg)

# save.image(file='./Markets/SpatPred/ETH_Monthly_Price_v3.RData')
write.csv(out_final,paste0('./Markets/SpatPred/monthlyPrice/ETH_1km_Price_AllMonths_v2.csv'))

write.csv(out_final[out_final$month==0,],paste0('./Markets/SpatPred/monthlyPrice/ETH_1km_Price_AnnualMedian_v2.csv'))

save.image(file='./Markets/SpatPred/ETH_Monthly_Price_v4.RData')
