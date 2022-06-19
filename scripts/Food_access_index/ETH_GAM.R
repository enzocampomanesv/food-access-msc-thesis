# FAI extrapolation using GAM in R
# 
# Florencio Campomanes V
# 2022-06-17
#
# This script requires the ff R packages for GAM optimization and viz:
#     mgcv - GAM implementation
#     tidymv - visualize GAM
#
# This script requires two datasets:
#   (1) the pop-weighted average input data 
#       from the ETH_geoAveraging_1km.R script.
#
#   (2) the complete input dataset (test) created in GIS.
#
# Notes:
# (a) All file paths and names should be changed as needed
#
setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")

library(mgcv)
library(ggplot2)
library(ggthemr)
library(RColorBrewer)
library(cowplot)
library(plotly)
library(scales)
library(tidymv)
library(tidyverse)
ggthemr("fresh")

rm(list=ls(all=TRUE))
# load("./RShiny/GAM_dataPrep_v7.RData")

d1 <- read.csv("./Data_gridded_1km/ETH_1km_GAMinput_v8.csv")
d1$X <- NULL
d1$ea_id <- as.character(d1$ea_id)
for(i in seq(4,ncol(d1)-3)){
  names(d1)[i] <- substring(names(d1)[i], 3)
}
d1 <- d1[,c('ea_id','RU','Road_prox','mktTT','bio_19','CRP', 'FAI')]
names(d1) <- c('ea_id','RU','RPX','TTM','B19','CRP','FAI')
train_val_split <- function(df, split=0.3){
  set.seed(123)
  # divide data into rural/urban based on LSMS grouping
  df_u <- df[df$RU=='2. URBAN',]
  df_r <- df[df$RU=='1. RURAL',]
  
  #take 20% of samples for validation for each class
  vdf_u <- df_u[sample(nrow(df_u)*split, replace=F),]
  vdf_r <- df_r[sample(nrow(df_r)*split, replace=F),]
  
  #take remaining samples for training
  tdf_u <- subset(df_u, !(ea_id %in% vdf_u$ea_id)) 
  tdf_r <- subset(df_r, !(ea_id %in% vdf_r$ea_id))
  
  # recombine urban/rural, but now divided into train/val
  train <- rbind(tdf_u,tdf_r)
  val <- rbind(vdf_u,vdf_r)
  
  d_list <- list(train, val)
  return(d_list)
}

d_list <- train_val_split(d1)
train_data <- d_list[[1]] # take first in list (training)


# plot data

plot_dataXY <- function(x){
  plotXY <- ggplot(train_data, aes_string(x=x,y="FAI"))+
            geom_point(alpha=0.3,color="black")
  return(plotXY)
}
terms <- c('RPX', 'TTM', 'B19', 'CRP')
data_plots <- lapply(terms, plot_dataXY)

plot_grid(data_plots[[1]],data_plots[[2]],
          data_plots[[3]],data_plots[[4]],ncol=2)
ggsave('./GAM/plots/v3/dataplots_XY_v3.png',
       width=10,height=5,units="in")
# GAM
# vars selected from VSURF then removed insig after first GAM run
# sp <- seq(0,1,by=0.1)
bs <- c('tp','cr','ps')
grid <- expand.grid(bs,bs,bs,bs,stringsAsFactors = FALSE)
names(grid) <- c('bs1','bs2','bs3','bs4')

## Grid search for best smoothing params
best_gcv <- 10000
best_aic <- 10000
best_i <- 0
best_gam <- NULL
for(i in seq(1,nrow(grid))){
  gam_temp <- gam(FAI~s(RPX, bs=grid[i,]$bs1) +
                       s(TTM, sp=0.6,  bs=grid[i,]$bs2) +
                       s(B19, bs=grid[i,]$bs3) +
                       s(CRP, bs=grid[i,]$bs4),
                      method='REML',
                      data=train_data)
  if(length(summary(gam_temp)$s.pv[summary(gam_temp)$s.pv<0.05])==(length(terms))){
    if(gam_temp$aic < best_aic){
      print(grid[i,])
      best_i <- i
      best_aic <- gam_temp$aic
      best_gcv <- gam_temp$gcv.ubre[1]
      best_gam <- gam_temp
    }
  }
}
train_data$CRP <- train_data$CRP/100
best_gam <- gam(FAI~s(RPX, bs='cr') +
                  s(TTM, sp=0.6, bs='cr') +
                  s(B19,bs='ps')+
                  s(CRP,bs='ps'),
                method="REML",
                data=train_data)

grid[best_i,]
summary(best_gam)
best_aic
# summary(trial_gam)

# Plot GAM
# best_gam_plot <- plot_smooths(best_gam,series="TTM")
pdplots <- lapply(terms, plot_smooths, model=best_gam)
# pdplots <- lapply(c(terms), plot_smooths, model=trial_gam)

# ggplotly(pdplots[[2]])
plot_grid(pdplots[[1]],pdplots[[2]],
          pdplots[[3]],pdplots[[4]],ncol=2)
ggsave('./GAM/plots/v3/GAM_responsecurves_v4.png',
       width=10,height=5,units="in")

# par(mfrow=c(1,2))
# plot(y_response,gam_out$fitted.values)
# abline(coef=c(0,1))
# hist(best_gam$fitted.values)

## Validation
gam_val <- d_list[[2]]
# for(i in seq(4,ncol(gam_val)-3)){
#   names(gam_val)[i] <- substring(names(gam_val)[i], 3)
# }
# gam_val <- gam_val[,c(input_list,'FAI')]
# gam_val <- rename_vars(gam_val)
# gam_val <- rename_preds(gam_val)
val_sel <- gam_val[,c('RPX','TTM','B19','CRP')]
gam_pred <- predict(best_gam,val_sel)

# par(mfrow=c(1,1))
# hist(gam_pred)
range(gam_pred)
range(gam_val$FAI)
cor(gam_val$FAI,gam_pred)
cor(best_gam$fitted.values,train_data$FAI)
plot_obsFit <- function(y_obs, y_pred){
  pred_data <- as.data.frame(cbind(y_obs,y_pred))
  names(pred_data)[1] <- 'Observed'
  names(pred_data)[2] <- 'Predicted'
  
  pl_cor <- ggplot(pred_data, aes(Observed, Predicted)) + 
    geom_point()+
    geom_abline(color="black",size=1)+
    coord_cartesian(xlim=c(-10,20),ylim=c(-10,20))+
    annotate(geom="text",
             x=-7, y=15, label="R2: 0.62")
  return(pl_cor)
}

gam_vFit <- plot_obsFit(gam_val$FAI,gam_pred)
gam_tFit <- plot_obsFit(train_data$FAI,best_gam$fitted.values)
plot_grid(gam_tFit,gam_vFit, labels=c('   Training','  Validation'))
ggsave('./GAM/plots/v3/GAM_TV_fit_v3.png',
       width=10,height=4,units="in")

fit_error <- function(y_obs, y_pred, sq=1){
  if(sq==1){
    errors <- (y_obs-y_pred)^2
    ylab <- 'Squared Error'
  }else{
    errors <- (y_pred-y_obs)
    ylab <- 'Error'
  }
  if(sq==1){
    # err_lm <- lm(errors~poly(as.vector(y_pred),2))
    err_lm <- lm(errors~as.vector(y_pred))
  }else{
    err_lm <- lm(errors~as.vector(y_pred))
  }
  err_pred <- predict(err_lm, data.frame(y_pred=seq(min(y_pred),max(y_pred),by=0.1)))
  err_df <- cbind(data.frame(y_pred=seq(min(y_pred),max(y_pred),by=0.1)),err_pred)
  names(err_df) <- c("predicted", "error")
  return(err_df)
}

plot_error <- function(y_obs, y_pred, sq=1, loess=0){
  if(sq==1){
    errors <- (y_obs-y_pred)^2
    ylab <- 'Squared Error'
  }else{
    errors <- (y_pred-y_obs)
    ylab <- 'Error'
  }
  
  pred_data <- as.data.frame(cbind(errors,y_pred))
  names(pred_data)[1] <- ylab
  names(pred_data)[2] <- 'Predicted'
  if(sq==1){
    ylim <- c(0,40)
  }else{
    ylim <- c(-10,10)
  }
  # eq = function(x){(1.2**x)}
  eq <- fit_error(y_obs, y_pred, sq=sq)
  pl_cor <- ggplot(pred_data, aes_string("Predicted",as.name(ylab))) + 
    geom_point()+
    # stat_function(fun=eq, size=1.2, alpha=0.5)+
    geom_line(data=eq, aes(x=predicted,y=error),
              color="black",size=1.2, alpha=0.5)+
    # geom_smooth(se=FALSE,color="black",size=1.2, alpha=0.5)+
    coord_cartesian(xlim=c(-10,20),ylim=ylim)+
    theme(axis.text.x=element_text(size=11))
  if(loess==1){
    pl_cor <- pl_cor +
      geom_smooth(method="loess")
  }
  if(sq==1){
    pl_cor <- pl_cor+
      annotate(geom="text",
               x=-8, y=35, label="            RMSE: 3.99
               nRMSE: 14.6%")
  }else if(sq ==0){
    pl_cor <- pl_cor+
      annotate(geom="text",
               x=-9, y=7, label="           MAE: 3.09
               nMAE: 13.4%")
  }
  return(pl_cor)
}
gam_tSQE <- plot_error(train_data$FAI,best_gam$fitted.values)
gam_vSQE <- plot_error(gam_val$FAI,gam_pred)
plot_grid(gam_tSQE,gam_vSQE, labels=c('   Training','  Validation'))
ggsave('./GAM/plots/v3/GAM_TV_sqE_v3.png',
       width=10,height=4,units="in")

gam_tErr <- plot_error(train_data$FAI,best_gam$fitted.values, sq=0)
gam_vErr <- plot_error(gam_val$FAI,gam_pred, sq=0)
plot_grid(gam_tErr,gam_vErr)
ggsave('./GAM/plots/v3/GAM_err_v3.png',
       width=10,height=4,units="in")

plot_grid(gam_vFit,
          gam_vSQE,
          gam_vErr,
          ncol=1)
ggsave('./GAM/plots/v3/GAM_allAccPlots_val_v2.png',
       width=6,height=8,units="in")

gam_rsqV <- cor(gam_val$FAI,gam_pred)^2
gam_rsqT <- cor(best_gam$fitted.values,train_data$FAI)^2
gam_rmseV <- sqrt(mean((gam_val$FAI-gam_pred)^2))
gam_rmseT <- sqrt(mean((best_gam$fitted.values-train_data$FAI)^2))
gam_nrmseV <- (gam_rmseV/(max(gam_pred)-min(gam_pred)))
gam_nrmseT<- (gam_rmseT/(max(best_gam$fitted.values)-min(best_gam$fitted.values)))
gam_maeT <- mean(abs(best_gam$fitted.values-train_data$FAI))
gam_maeV <- mean(abs(gam_val$FAI-gam_pred))
norm_t_mae <- (gam_maeT/(max(best_gam$fitted.values)-min(best_gam$fitted.values)))
norm_v_mae <- (gam_maeV/(max(gam_val$FAI)-min(gam_val$FAI)))

## Predict
test_data_orig <- read.csv("./Data_gridded_1km/value_grids/ETH_valueGrid1km_v9.csv")
test_data_cf <- data.frame(test_data_orig)
test_data_orig$URCA <- factor(test_data_orig$URCA, levels=c("1","4","7","10",
                                                            "2","5","8","11",
                                                            "3","6","9","12",
                                                            "13","14"))
names(test_data_orig)[names(test_data_orig)=='TTM'] <- 'mktTT'
names(test_data_orig)[names(test_data_orig)=='RoadProx'] <- 'Road_prox'
pred <- test_data_orig[,c('c_id','Road_prox','mktTT','bio_19','CRP')]
names(pred) <- c('c_id','RPX','TTM','B19','CRP')
pred_FAI <- predict(best_gam,pred[,names(pred)[-1]])
range(pred_FAI)

names(test_data_cf)[names(test_data_cf)=='TTM_Cf'] <- 'mktTT'
names(test_data_cf)[names(test_data_cf)=='RoadProx'] <- 'Road_prox'
pred_cf <- test_data_cf[,c('c_id','Road_prox','mktTT','bio_19','CRP')]
names(pred_cf) <- c('c_id','RPX','TTM','B19','CRP')
pred_FAI_cf <- predict(best_gam,pred_cf[,names(pred_cf)[-1]])
range(pred_FAI_cf)

# cap the predictions to reasonable FAI values for histogram

cap_FAI <- function(df_FAI, cap=50){
  df_FAI[df_FAI>cap] <- cap
  df_FAI[df_FAI< -cap] <- -cap
  return(df_FAI)
}
pred_FAI <- cap_FAI(pred_FAI)
pred_FAI_cf <- cap_FAI(pred_FAI_cf)

histogram(pred_FAI)
histogram(pred_FAI_cf)

mycolors <- colorRampPalette(brewer.pal(3, "RdYlBu"))(50)
p<-ggplot(data.frame(FAI=pred_FAI), aes(x=FAI)) + 
  geom_histogram(color=NA, fill=mycolors, bins = 50)+
  ylab("Frequency")+
  xlab("Food Access Index")+
  scale_x_continuous(breaks=seq(-40,40,5))
p

export_FAI <- function(pred, cf=0){
  pred <- pred
  f_res <- cbind(as.data.frame(test_data_orig[,c('c_id','URCA')]),pred)
  f_res <- f_res[,c(1,2,ncol(f_res))]
  if(cf==0){
    names(f_res)[names(f_res)=='pred_FAI'] <- 'FAI'
    write.csv(f_res, "./Data_gridded_1km/FAI_results/FAI_predictions_optimGAM_v10.csv")
  }else{
    names(f_res)[names(f_res)=='pred_FAI_cf'] <- 'FAI'
    write.csv(f_res, "./Data_gridded_1km/FAI_results/FAI_predictions_optimGAM_cf_v10.csv")
  }
}
URCA_lab <- read.csv("./UrbanRural/URCA_labels.csv")
URCA_lab$URCA <- as.factor(URCA_lab$URCA)
URCA_lab$URCA_lab <- str_wrap(URCA_lab$URCA_lab, width = 12)
plot_urcaFAI <- function(pred, title="", urca_sub=c(1,2,3,13,14), outlier = 19){
  f_res <- cbind(as.data.frame(test_data_orig[,c('c_id','URCA','NAME_1')]),pred)
  f_res$URCA <- as.factor(f_res$URCA)
  out <- left_join(x=f_res,y=URCA_lab,by="URCA")
  # f_res <- f_res[,c(1,2,ncol(f_res))]
  out$URCA <- factor(out$URCA,levels=c("1","4","7","10",
                                   "2","5","8","11",
                                   "3","6","9","12",
                                   "13","14"))
  out$URCA_lab <- str_wrap(out$URCA_lab, width = 12)
  urcaxfai <- ggplot(subset(out, (URCA %in% urca_sub)), 
                     aes(x=URCA,y=pred))+
    geom_boxplot(outlier.shape = outlier) +
    scale_x_discrete(breaks=out$URCA, labels=out$URCA_lab, limits=rev(levels(out$URCA)))+
    geom_hline(yintercept = 0, color="red", size=1.3, alpha=0.8)+
    ylab("Food Access Index")+
    xlab("URCA")+
    ylim(c(-30,30))+
    ggtitle(title)
  if(length(urca_sub)==14){
    urcaxfai <- urcaxfai +
      theme(axis.text.x=element_text(
        size=11,
        vjust=0.5, hjust=1))+
      coord_flip()
  }else{
    urcaxfai <- urcaxfai +
      theme(axis.text.x=element_text(size=11))
  }
  return(urcaxfai)
}
# urca_FAI <- plot_urcaFAI(pred_FAI,urca_sub=c(1:14),
#                          title="Food Access in Tigray Region (Pre-war)")
# urca_FAI
# ggsave('./GAM/plots/URCA_predGAM_Tigray.png',
#        width=10,height=5,units="in")
# urca_FAI_cf <- plot_urcaFAI(pred_FAI_cf,urca_sub=c(1:14),
#                             title="Food Access in Tigray Region (Conflict)")
# plot_grid(urca_FAI,urca_FAI_cf,ncol=1)
# ggsave('./GAM/plots/URCA_predGAM_Tigray_compare.png',
#        width=10,height=8,units="in")
plot_urcaFAI(pred_FAI,title="Food Access Levels per URCA (All)",urca_sub=c(1:14))
ggsave('./GAM/plots/URCAall_predGAM_flipXY_v2.png',
       width=10,height=8,units="in")


# plot_urcaFAI(pred_FAI_cf, title = "(Conflict)")
# ggsave('./GAM/plots/URCA_predGAM_cf.png',
#        width=10,height=4,units="in")

export_FAI(pred_FAI)
export_FAI(pred_FAI_cf, cf=1)
save.image(file='./RShiny/GAM_dataPrep_v6.RData')
saveRDS(best_gam, file='./GAM/FAI_GAM_v5.rds')

# predict monthly
# for(i in (1:12)){
#   if(i<10){
#     m <- paste0('0',as.character(i))
#   }else{
#     m <- as.character(i)
#   }
#   month_price <- read.csv('./Markets/SpatPred/monthlyPrice/ETH_1km_Price_month',substr(m,2,4),'.csv')
#   
# }
month_price <- read.csv('./Markets/SpatPred/monthlyPrice/ETH_1km_Price_AllMonths_v2.csv')
test <- left_join(test_data_orig[,c('c_id','URCA')],URCA_lab,by="URCA")
month_price <- left_join(month_price, test,by="c_id")
month_price$month <- as.factor(month_price$month)

urca_colors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(5)

urcaxPrice<- month_price %>% 
  mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  filter(URCA %in% c(1,2,3,13,14))%>%
  ggplot(aes(x=month,y=price))+
  geom_boxplot(aes(fill=URCA_lab),
               # outlier.shape = NA,
               color="grey60",
               notchwidth = 0.2) +
  scale_fill_manual(values=urca_colors)+
  ylim(0,0.15)+
  ylab("Cereal Price")+
  xlab("Month")+
  ggtitle("Cereal Price per URCA across months")+
  # geom_hline(yintercept=1,size=1.2,alpha=0.8,color="dodgerblue2")+
  guides(fill=guide_legend(title="URCA"))+
  theme(axis.text.x=element_text(size=11,color="white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.line = element_line(color="white"),
        panel.grid.minor = element_line(color="grey15"),
        panel.grid.major = element_line(color="grey15"),
        axis.title = element_text(color="white"),
        axis.text = element_text(color="white"),
        title=element_text(color="white"),
        legend.key=element_rect(fill="black",color="black"),
        legend.text = element_text(color="white"),
        legend.background = element_rect(fill="black",color=NA))
urcaxPrice
ggsave('./GAM/plots/URCA_Price.png',
       width=10,height=6,units="in")
month_price_agg <- aggregate(month_price$price, 
                             by=list(month_price$URCA,month_price$month),
                             mean)
names(month_price_agg) <- c('URCA','Month','Price')
month_price_agg <- left_join(month_price_agg, 
                             URCA_lab, 
                             by="URCA")
urca_colors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(5)
urcaxPriceTS<- month_price_agg %>%
  mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  filter(URCA %in% c(1:3,13:14)&
           !(Month %in% c(0)))%>%
  ggplot(aes(x=Month,y=Price,group=URCA))+
  geom_smooth(aes(color=URCA_lab), size=1.2, alpha=0.8, se=FALSE) +
  scale_color_manual(values=urca_colors)+
  # ylim(7,9)+
  ylab("Cereal Price")+
  xlab("Month")+
  ggtitle("Cereal Price per URCA across months")+
  # geom_hline(yintercept=1,size=1.2,alpha=0.8,color="dodgerblue2")+
  guides(color=guide_legend(title="URCA"))+
  # theme(axis.text.x=element_text(size=11,color="white"),
        # plot.background = element_rect(fill="black"),
        # panel.background = element_rect(fill="black"),
        # axis.line = element_line(color="white"),
        # panel.grid.minor = element_line(color="grey15"),
        # panel.grid.major = element_line(color="grey15"),
        # axis.title = element_text(color="white"),
        # axis.text = element_text(color="white"),
        # title=element_text(color="white"),
        # legend.key=element_rect(fill="black",color="black"),
        # legend.text = element_text(color="white"),
        # legend.background = element_rect(fill="black",color=NA),
        theme(legend.spacing.y = unit(0.5, 'cm'))
urcaxPriceTS
ggsave('./GAM/plots/URCA_PriceTS_v2.png',
       width=10,height=6,units="in")
out_price <- data.frame("c_id"=unique(test$c_id))

for(i in seq(0,12)){
  if(i<10){
    m <- paste0('0',as.character(i))
  }else{
    m <- as.character(i)
  }
  price_m <- month_price[month_price$month==i,c('c_id','price')]
  names(price_m)[2] <- paste0('price_',m)
  out_price <- left_join(out_price,price_m,by="c_id")
}
test <- left_join(test,out_price,by="c_id")
test <- left_join(test,pred,by="c_id")
out_monthly <- data.frame()
for(i in seq(0,12)){
  if(i<10){
    m <- paste0('0',as.character(i))
  }else{
    m <- as.character(i)
  }
  price_var <- paste0('price_',m)
  # pred_price <- c('RPX','TTM','B19','CRP','c_id','URCA','URCA_lab')
  temp_df <- data.frame(test)
  temp_df$month <- as.factor(i)
  temp_df$month <- factor(temp_df$month,
                          levels=c("1","2","3","4",
                                 "5","6","7","8",
                                 "9","10","11","12"))
  temp_df$CRP <- temp_df[[price_var]]
  temp_df$FAI <- cap_FAI(predict(best_gam,
                                 temp_df[,terms]))
  out_monthly <- rbind(out_monthly, temp_df)
}
f_res <- cbind(as.data.frame(test_data_orig[,c('c_id')]),pred_FAI)
names(f_res)[1] <- 'c_id'
out_m <- left_join(out_monthly,f_res,by="c_id",all.x=TRUE)
out_m$cFAI <- (out_m$FAI-out_m$pred_FAI)/abs(out_m$pred_FAI)
out_m$diffFAI <- out_m$FAI-out_m$pred_FAI
out_m$month <- ifelse(is.na(out_m$month), 0,out_m$month)
out_m$month <- factor(out_m$month,
                        levels=c("0","1","2","3","4",
                                 "5","6","7","8",
                                 "9","10","11","12"))
out_m <- left_join(out_m, test_data_orig[,c('c_id','NAME_1','NAME_2')],
                   by="c_id")

# mycolors <- colorRampPalette(brewer.pal(3, "RdYlBu"))(50)
diffFAI_hist<-ggplot(out_m, aes(x=diffFAI)) + 
  geom_histogram(aes(y = stat(width*density)),
                 color="black",
                 bins = 40)+
  ylab("Percentage of Total")+
  xlab("FAI Difference")+
  scale_x_continuous(breaks=seq(-8,4,2))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))
diffFAI_hist
ggsave('./GAM/plots/FAI_diff_hist_fix.png',
       width=8,height=4,units="in")
# out_m$pred_FAI <- NULL
# out_m$month <- as.factor(out_m$month)
# out_m <- left_join(out_m,month_price[,c('c_id','price')],by="c_id",all.x=TRUE)
save.image(file='./RShiny/GAM_dataPrep_v5.RData')
for(i in seq(0,12)){
  exp_month <- out_m[out_m$month==i,c('c_id','month','URCA','URCA_lab','diffFAI','cFAI','FAI')]
  write.csv(exp_month, paste0("./Data_gridded_1km/FAI_results/monthly/csv/FAI_optimGAM_month",as.character(i),"_v2.csv"))
}
write.csv(out_m, "./Data_gridded_1km/FAI_results/FAI_predictions_optimGAM_monthly_v2.csv")
# rm(out_monthly)

plot_hist <- function(month, c=0){
  temp_df <- out_m[out_m$month==month,]
  if(c==0){
    hist_plot <- ggplot(data=temp_df, 
                        aes_string(x=var_FAI))+
                        geom_density(alpha=.2, fill="#FF6666")+
                        xlim(-40,40)
  }else{
    hist_plot <- ggplot(data=temp_df, 
                        aes_string(x=var_cFAI))+
                        geom_density(alpha=.2, fill="#FF6666")+
                        xlim(-10,10)
  }
  hist_plot
}

get_month_txt <- function(nums){
  txts <- array(dim=length(nums))
  for(i in (1:length(nums))){
    if(i < 10){
      txts[i] <- paste0('0',as.character(i))
    }else{
      txts[i] <- as.character(i)
    }
  }
  return(txts)
}
urca_colors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(5)
urcaxfai <- out_m %>% 
  mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  filter(URCA %in% c(1,2,3,13,14)&
           !(month %in% c(0)))%>%
  ggplot(aes(x=month,y=FAI))+
  geom_boxplot(aes(fill=URCA_lab),
               outlier.shape = NA,
               color="grey60") +
  scale_fill_manual(values=urca_colors)+
  ylim(-20,20)+
  ylab("Food Access Index")+
  xlab("Month")+
  ggtitle("Food Access Index per URCA across months")+
  guides(fill=guide_legend(title="URCA"))+
  theme(axis.text.x=element_text(size=11,color="white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.line = element_line(color="white"),
        panel.grid.minor = element_line(color="grey15"),
        panel.grid.major = element_line(color="grey15"),
        axis.title = element_text(color="white"),
        axis.text = element_text(color="white"),
        title=element_text(color="white"),
        legend.key=element_rect(fill="black",color="black"),
        legend.text = element_text(color="white"),
        legend.background = element_rect(fill="black",color=NA),
        legend.spacing.y = unit(0.5, 'cm'))
urcaxfai
ggsave('./GAM/plots/URCA_monthlyFAI_v2.png',
       width=12,height=6,units="in")
urcaxcFAI<- out_m %>% 
  mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  filter(URCA %in% c(1,2,3,13,14)&
           !(month %in% c(0)))%>%
  ggplot(aes(x=month,y=diffFAI))+
  geom_boxplot(aes(fill=URCA_lab),
               outlier.shape = NA,
               color="grey60",
               notchwidth = 0.2) +
  scale_fill_manual(values=urca_colors)+
  ylim(-1,1)+
  ylab("Difference Food Access Index")+
  xlab("Month")+
  ggtitle("Difference Food Access Index per URCA across months")+
  guides(fill=guide_legend(title="URCA"))+
  theme(axis.text.x=element_text(size=11,color="white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.line = element_line(color="white"),
        panel.grid.minor = element_line(color="grey15"),
        panel.grid.major = element_line(color="grey15"),
        axis.title = element_text(color="white"),
        axis.text = element_text(color="white"),
        title=element_text(color="white"),
        legend.key=element_rect(fill="black",color="black"),
        legend.text = element_text(color="white"),
        legend.background = element_rect(fill="black",color=NA))
urcaxcFAI
ggsave('./GAM/plots/URCA_monthlyDiffFAI_v2.png',
       width=12,height=6,units="in")

out_m_agg_region <- aggregate(out_m[,c('FAI','diffFAI','cFAI')],
                       by=list(out_m$URCA,out_m$month,out_m$NAME_1), mean)
out_m_agg <- aggregate(out_m[,c('FAI','diffFAI','cFAI')],
                       by=list(out_m$URCA,out_m$month), mean)

names(out_m_agg_region)[1:3] <- c('URCA','Month','Region')
names(out_m_agg)[1:2] <- c('URCA','Month')

out_m_agg_region <- left_join(out_m_agg_region, URCA_lab, by="URCA")
out_m_agg_region$URCA_lab <- str_wrap(out_m_agg_region$URCA_lab, width = 12)

out_m_agg <- left_join(out_m_agg, URCA_lab, by="URCA")
out_m_agg$URCA_lab <- str_wrap(out_m_agg$URCA_lab, width = 12)

plot_time <- function(df, metric, urca_sub=c(1,2,3,13,14), region="All"){
  urca_colors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(length(urca_sub))
  urcaxfai_df<- df %>% 
    mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
    filter(URCA %in% urca_sub&
             !(Month %in% c(0)))
  if(region != "All"){
    urcaxfai_df <- urcaxfai_df %>%
      filter(Region==region)
  }
  urcaxfai_line <- urcaxfai_df %>%
    ggplot(aes_string(x="Month",y=metric, group="URCA"))+
    geom_smooth(aes(color=URCA_lab), size=1.2, alpha=0.8,se=FALSE) +
    scale_color_manual(values=urca_colors)+
    xlab("Month")+
    guides(color=guide_legend(title="URCA",byrow=TRUE))+
    theme(axis.text.x=element_text(size=11,color="white"),
          plot.background = element_rect(fill="black",color="black"),
          panel.background = element_rect(fill="black"),
          axis.line = element_line(color="white"),
          panel.grid.minor = element_line(color="grey15"),
          panel.grid.major = element_line(color="grey15"),
          axis.title = element_text(color="white"),
          axis.text = element_text(color="white"),
          title=element_text(color="white"),
          legend.key=element_rect(fill="black",color="black"),
          legend.text = element_text(color="white"),
          legend.background = element_rect(fill="black",color=NA),
          legend.spacing.y = unit(0.5, 'cm'))
  
  if(metric=="FAI"){
    urcaxfai_line <- urcaxfai_line+
      ylim(-25,20)+
      ylab("Food Access Index")+
      ggtitle("Average Food Access Index per URCA across months")
  }else if(metric=="diffFAI"){
    urcaxfai_line <- urcaxfai_line+
      ylim(-0.1,0.1)+
      ylab("FAI Difference")+
      ggtitle("Average Difference Food Access Index per URCA across months")
  }else if(metric=="cFAI"){
    urcaxfai_line <- urcaxfai_line+
      ylim(-0.2,0.2)+
      ylab("FAI %Difference")+
      ggtitle("Average %Difference Food Access Index per URCA across months")
  }else{
    urcaxfai_line <- urcaxfai_line+
      geom_line(aes(y=1),color="dodgerblue2",alpha=0.6)+
      ylim(0.75,1.25)+
      ylab("Temporal Price Index")+
      ggtitle("Average Temporal Price Index per URCA across months")
  }
  return(urcaxfai_line)
}
plot_time(out_m_agg, "FAI", region="All")
ggsave('./GAM/plots/URCA_TS_FAI_v2.png',
       width=12,height=6,units="in")
plot_time(out_m_agg, "FAI", urca_sub=c(1,4,7,10))
plot_time(out_m_agg, "FAI", urca_sub=c(2,5,8,11))
plot_time(out_m_agg, "FAI", urca_sub=c(3,6,9,12))
plot_time(out_m_agg, "FAI", urca_sub=c(4:6))
plot_time(out_m_agg, "FAI", urca_sub=c(7:9))
plot_time(out_m_agg, "FAI", urca_sub=c(10:12))
plot_time(out_m_agg, "FAI", urca_sub=c(10:14))
plot_time(out_m_agg, "diffFAI")
ggsave('./GAM/plots/URCA_TS_diffFAI_v2.png',
       width=12,height=6,units="in")
plot_time(out_m_agg, "cFAI")
ggsave('./GAM/plots/URCA_TS_diffPercFAI.png',
       width=12,height=6,units="in")

rm(list=c('out_monthly','month_TPI','tmp_sum'))
save.image(file='./RShiny/GAM_dataPrep_v4.RData')


#### test

for(i in 1:12){
  if(i<10){
    m <- paste0('price_0',as.character(i))
  }else{
    m <- paste0('price_',as.character(i))
  }
  print(paste(i,mean(out_m[out_m$URCA==1,][[m]]-out_m[out_m$URCA==1,]$price_00)))
}


####
months <- c("01","02","03","04","05","06",
            "07","08","09","10","11","12")
month_plots <- lapply(months, plot_hist,c=1)

plot_grid(month_plots[[1]],month_plots[[2]],month_plots[[3]],
          month_plots[[4]],month_plots[[5]],month_plots[[6]],
          month_plots[[7]],month_plots[[8]],month_plots[[9]],
          month_plots[[10]],month_plots[[11]],month_plots[[12]],ncol=4)


#### latitudinal plots
out_m <- left_join(out_m, test_data_orig[,c('c_id','lat','long')],by="c_id")
out_m_latAgg <- aggregate(out_m[out_m$month==0,]$FAI,
                          by=list(out_m[out_m$month==0,]$lat,
                                  out_m[out_m$month==0,]$URCA,
                                  out_m[out_m$month==0,]$URCA_lab), mean)
names(out_m_latAgg) <- c('laty','URCA','URCA_lab', 'FAI')
out_m_lonAgg <- aggregate(out_m[out_m$month==0,]$FAI,
                          by=list(out_m[out_m$month==0,]$long,
                                  out_m[out_m$month==0,]$URCA,
                                  out_m[out_m$month==0,]$URCA_lab), mean)
names(out_m_lonAgg) <- c('lonx','URCA','URCA_lab', 'FAI')

get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}
density_map <- out_m_latAgg %>% 
  select(laty, FAI) %>% 
  mutate(density = get_density(laty, FAI, n = 100))

density_map_lon <- out_m_lonAgg %>% 
  select(lonx, FAI) %>% 
  mutate(density = get_density(lonx, FAI, n = 100))

# no URCAs


latxFAI <- density_map %>%
  ggplot(aes(x=laty,y=FAI, color=density))+
  geom_point(alpha=0.2) +
  scale_color_gradient2(low = "steelblue1",
                        mid = "yellow2",
                        high = "red",
                        limits=c(0,0.014),
                        midpoint=0.007)+
  # ylim(-20,20)+
  geom_hline(yintercept=0, color="black", size=1.3, alpha=0.5)+
  ylab("FAI")+
  xlab("Latitude")+
  coord_flip()+
  # guides(color="none")+
  ggtitle("Food Access Index across latitudes")
latxFAI
ggsave('./GAM/plots/Lat_FAI.png',
       width=6,height=10,units="in")

lonxFAI <- density_map_lon %>%
  ggplot(aes(x=lonx,y=FAI, color=density))+
  geom_point(alpha=0.2) +
  scale_color_gradient2(low = "steelblue1",
                        mid = "yellow2",
                        high = "red",
                        limits=c(0,0.014),
                        midpoint=0.007)+
  # ylim(-20,20)+
  geom_hline(yintercept=0, color="black", size=1.3, alpha=0.5)+
  ylab("FAI")+
  xlab("Longitude")+
  # guides(color=guide_legend(title="Density"))+
  ggtitle("Food Access Index across longitudes")
lonxFAI
ggsave('./GAM/plots/Lon_FAI.png',
       width=10,height=6,units="in")
plot_grid(latxFAI,lonxFAI)

lonxFAI_smooth <- out_m_lonAgg %>% 
  # mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  # filter(URCA %in% c(1,2,3,13,14))%>%
  ggplot(aes(x=lonx,y=FAI))+
  geom_smooth(size=0.3, alpha=0.8) +
  scale_fill_manual(values=urca_colors)+
  # ylim(-20,20)+
  ylab("FAI")+
  xlab("Latitude")+
  ggtitle("Food Access Index across longitudes")
lonxFAI_smooth
# with URCAs
latxFAI_smooth <- out_m_latAgg %>% 
  # mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)),) %>%
  # filter(URCA %in% c(1,2,3,13,14))%>%
  ggplot(aes(x=laty,y=FAI))+
  geom_smooth(size=0.3, alpha=0.8, se=FALSE) +
  # scale_fill_manual(values=urca_colors)+
  ylab("FAI")+
  xlab("Latitude")+
  coord_flip()+
  ggtitle("Food Access Index across latitudes")
latxFAI_smooth



#TEST

out_0 <- out_m[out_m$month==0,]
maxRPX <- max(out_0$RPX)+1
maxTTM <- max(out_0$TTM)+1
maxB19 <- max(out_0$B19)+1
maxCRP <- max(out_0$CRP)+1

out_0_avgFAI <- out_0[(out_0$FAI > 10),]
out_0_avgFAI[out_0_avgFAI$TTM < 0,]$TTM <- 0
par(mfrow=c(2,2))
hist(out_0_avgFAI$RPX, breaks = seq(from=0,to=maxRPX))
hist(out_0_avgFAI$TTM, breaks = seq(from=0,to=maxTTM))
# hist(out_0_avgFAI$B19, breaks = seq(from=0,to=maxB19))
plot(out_0_avgFAI$RPX,out_0_avgFAI$CRP,
     xlim=c(0,maxRPX), ylim=c(0,maxCRP))
hist(out_0_avgFAI$CRP, breaks = seq(from=0,to=maxCRP))

out_0_avgFAI <- out_0[(out_0$FAI > -0.25)&(out_0$FAI < 0.25),]
par(mfrow=c(2,2))
hist(out_0_avgFAI$RPX, breaks = seq(from=0,to=maxRPX))
hist(out_0_avgFAI$TTM, breaks = seq(from=0,to=maxTTM))
# hist(out_0_avgFAI$B19, breaks = seq(from=0,to=maxB19))
plot(out_0_avgFAI$RPX,out_0_avgFAI$CRP,
     xlim=c(0,maxRPX), ylim=c(0,maxCRP))
hist(out_0_avgFAI$CRP, breaks = seq(from=0,to=maxCRP))

out_0_avgFAI <- out_0[(out_0$FAI < -10),]
par(mfrow=c(2,2))
hist(out_0_avgFAI$RPX, breaks = seq(from=0,to=maxRPX))
hist(out_0_avgFAI$TTM, breaks = seq(from=0,to=maxTTM))
# hist(out_0_avgFAI$B19, breaks = seq(from=0,to=maxB19))
plot(out_0_avgFAI$RPX,out_0_avgFAI$CRP,
     xlim=c(0,maxRPX), ylim=c(0,maxCRP))
hist(out_0_avgFAI$CRP, breaks = seq(from=0,to=maxCRP))


##### compare multi-linear regression

fai_mlr <- lm(FAI~RPX+TTM+B19+CRP,data=train_data)
summary(fai_mlr)
mlr_pred <- predict(fai_mlr,val_sel)

# mlr_rsqV <- cor(gam_val$FAI,mlr_pred)^2
# mlr_rsqT <- cor(fai_mlr$fitted.values,train_data$FAI)^2
# mlr_rmseV <- sqrt(mean((gam_val$FAI-mlr_pred)^2))
# mlr_rmseT <- sqrt(mean((fai_mlr$fitted.values-train_data$FAI)^2))
# mlr_nrmseV <- (mlr_rmseV/(max(mlr_pred)-min(mlr_pred)))
# mlr_nrmseT<- (mlr_rmseT/(max(fai_mlr$fitted.values)-min(fai_mlr$fitted.values)))
# mlr_maeT <- mean(abs(fai_mlr$fitted.values-train_data$FAI))
# mlr_maeV <- mean(abs(gam_val$FAI-mlr_pred))
# mlr_norm_t_mae <- (mlr_maeT/(max(fai_mlr$fitted.values)-min(fai_mlr$fitted.values)))
# mlr_norm_v_mae <- (mlr_maeV/(max(gam_val$FAI)-min(gam_val$FAI)))

calc_acc <- function(pred, model, tv='T'){
  if(tv == 'T'){
    rsq <- cor(model$fitted.values,train_data$FAI)^2
    rmse <- sqrt(mean((model$fitted.values-train_data$FAI)^2))
    nrmse <- (rmse/(max(model$fitted.values)-min(model$fitted.values)))
    mae <- mean(abs(model$fitted.values-train_data$FAI))
    norm_mae <- (mae/(max(model$fitted.values)-min(model$fitted.values)))
  }else{
    rsq <- cor(gam_val$FAI,pred)^2
    rmse <- sqrt(mean((gam_val$FAI-pred)^2))
    nrmse <- (rmse/(max(pred)-min(pred)))
    mae <- mean(abs(gam_val$FAI-pred))
    norm_mae <- (mae/(max(gam_val$FAI)-min(gam_val$FAI)))
  }
  metrics <- list(rsq, rmse, nrmse, mae, norm_mae)
  return(metrics)
}

acc_metrics <- data.frame(matrix(nrow=4, ncol=7))
names(acc_metrics) <- c('method','TV','rsq','rmse','nrmse','mae','nmae')
acc_metrics$method <- rep(c('GAM','GAM','MLR','MLR'))
acc_metrics$TV <- rep(c('T','V'),2)
acc_metrics[1,c(3:7)] <- calc_acc(gam_pred, best_gam)
acc_metrics[2,c(3:7)] <- calc_acc(gam_pred, best_gam, tv='V')
acc_metrics[3,c(3:7)] <- calc_acc(mlr_pred, fai_mlr)
acc_metrics[4,c(3:7)] <- calc_acc(mlr_pred, fai_mlr,tv='V')

plot_pdpmlr <- function(p, mlr_model){
  pred_pdp <- as.data.frame(cbind(mlr_model$model[[p]],mlr_model$model$FAI))
  names(pred_pdp)[1] <- p
  names(pred_pdp)[2] <- 'FAI'
  
  yint <- mlr_model$coefficients[1]
  i <- which(names(fai_mlr$coefficients)==p)
  coef <- mlr_model$coefficients[i]
  
  lr <- function(x){((coef*x)+yint)}
  
  x <- seq(min(mlr_model$model[[p]]),max(mlr_model$model[[p]]))
  ln <- lr(x)
  ln_df <- cbind(data.frame(x),ln)
  names(ln_df) <- c(p, "FAI")
  
  pl_cor <- ggplot(pred_pdp, aes_string(p, 'FAI')) + 
    geom_point()+
    geom_line(aes_string(x=p,y='FAI'),
              data=ln_df,
              color='black',
              size=1)+
    coord_cartesian()
  return(pl_cor)
}

plot_grid(plot_pdpmlr(terms[1], fai_mlr),plot_pdpmlr(terms[2], fai_mlr),
          plot_pdpmlr(terms[3], fai_mlr),plot_pdpmlr(terms[4], fai_mlr),ncol=2)
ggsave('./GAM/plots/v3/MLR_responsecurves_v3.png',
       width=10,height=5,units="in")

mlr_vFit <- plot_obsFit(gam_val$FAI,mlr_pred)
mlr_tFit <- plot_obsFit(train_data$FAI,fai_mlr$fitted.values)
plot_grid(mlr_tFit,mlr_vFit)
ggsave('./GAM/plots/v3/MLR_TV_Fit_v3.png',
       width=10,height=4,units="in")

mlr_tSQE <- plot_error(train_data$FAI,fai_mlr$fitted.values)
mlr_vSQE <- plot_error(gam_val$FAI,mlr_pred)
plot_grid(mlr_tSQE,mlr_vSQE)
ggsave('./GAM/plots/v3/MLR_sqErr_v3.png',
       width=10,height=4,units="in")

mlr_tErr <- plot_error(train_data$FAI,fai_mlr$fitted.values, sq=0)
mlr_vErr <- plot_error(gam_val$FAI,mlr_pred, sq=0)
plot_grid(mlr_tErr,mlr_vErr)
ggsave('./GAM/plots/v3/MLR_TV_err_v3.png',
       width=10,height=4,units="in")

plot_grid(mlr_tFit,mlr_vFit,
          mlr_tSQE,mlr_vSQE,
          mlr_tErr,mlr_vErr,
          labels=c('   Training','  Validation'),
          ncol=2)
ggsave('./GAM/plots/v3/MLR_allAccPlots.png',
       width=8,height=9,units="in")


# plot population
base <- merge(x=out_m[out_m$month==0,],
              y=test_data_orig[,c('c_id','pop')],
              by="c_id", all.x = TRUE)

base$FAI_level <- ifelse(base$FAI<= -5,1,
                   ifelse(base$FAI> -5&base$FAI<=0,2,
                    ifelse(base$FAI>0&base$FAI<=5,3,
                     ifelse(base$FAI>5&base$FAI<10,4,
                      ifelse(base$FAI>10&base$FAI<15,5,6)))))
baseAgg <- aggregate(base$pop, by=list(base$FAI_level), sum)
names(baseAgg)[1] <- 'FAI_level'
names(baseAgg)[2] <- 'Population affected'
baseAgg$`Population affected` <- baseAgg$`Population affected`/1000000
baseAgg$FAI_lab <- ifelse(baseAgg$FAI_level==1,'Less than -5',
                    ifelse(baseAgg$FAI_level==2,'-5 to 0',
                     ifelse(baseAgg$FAI_level==3,'0 to 5',
                      ifelse(baseAgg$FAI_level==4,'5 to 10',
                       ifelse(baseAgg$FAI_level==5,'10 to 15','Greater than 15')))))
mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(6)
FAI_popPlot <- ggplot(baseAgg, 
                      aes(x=reorder(FAI_level,FAI_level),y=`Population affected`))+
              geom_bar(stat='identity', width = 0.5, fill=mycolors)+
              scale_x_discrete(breaks=baseAgg$FAI_level, 
                               labels=baseAgg$FAI_lab)+
              ylab("Population Affected\n(in Millions)")+
              xlab("FAI Level")+
              ggtitle("Population per FAI level")+
              theme(axis.text.x=element_text(size=9))+
              scale_y_continuous(labels=scales::label_number(accuracy=0.1))

FAI_popPlot
ggsave('./GAM/plots/v3/FAIxPop_plot_v3_RdBu_flipXY.png',
       width=7,height=5,units="in")
# T test

hist(((best_gam$fitted.values-train_data$FAI)^2)-
       (fai_mlr$fitted.values-train_data$FAI)^2)

shapiro.test(((best_gam$fitted.values-train_data$FAI)^2)-
               (fai_mlr$fitted.values-train_data$FAI)^2)

t.test((best_gam$fitted.values-train_data$FAI)^2,
       (fai_mlr$fitted.values-train_data$FAI)^2, paired=TRUE)

tuk_df <- data.frame(matrix(nrow=12,ncol=4))
names(tuk_df) <- c('diff', 'p_adj','URCA', 'month')

for(i in seq(from=1,to=168,by=12)){
  urc <- round(i/12)+1
  one_way <- aov(FAI ~ month, data = out_m[out_m$URCA==urc,])
  sum_ANOVA <- summary(one_way)
  if(sum_ANOVA[[1]]$`Pr(>F)`[1] < 0.05){
    tuk <- TukeyHSD(one_way, conf.level=.95)
    for(j in seq(from=0,to=11)){
      tuk_df[i+j,] <- c(tuk$month[j+1,c(1,4)],urc,j+1)
    }
  }
}
tuk_df_sig <- tuk_df[complete.cases(tuk_df)&
                       tuk_df$p_adj<0.05,]
save.image(file='./RShiny/GAM_dataPrep_v7.RData')
one_way <- aov(FAI ~ month, data = out_m[out_m$URCA==11,])
summary(one_way)
tuk <- TukeyHSD(one_way, conf.level=.95)
tuk
# sort(tuk$month[,4])
plot(TukeyHSD(one_way, conf.level=.95), las = 2)

ggplot(out_m[out_m$month==0,], aes(x=RPX, y=FAI))+
  geom_point()

nrow(test_data_orig[test_data_orig$URCA %in% c(3,6,9,12),])/
  nrow(test_data_orig)

nrow(test_data_orig[test_data_orig$URCA %in% c(2,5,8,11),])/
  nrow(test_data_orig)

nrow(test_data_orig[test_data_orig$URCA %in% c(1,4,7,10),])/
  nrow(test_data_orig)

norm_unitVar <- function(v) {
  return((v-mean(v))/sd(v))
}
mean(out_m[out_m$month==12,]$FAI)
out_0 <- out_m[out_m$month==0,]
mean(out_0$FAI)
out_0$nFAI <- norm_unitVar(out_0$FAI)
mean(out_0$nFAI)
hist(out_0$nFAI)
mean_FAI <- mean(out_0$FAI)
mean(out_0[out_0$FAI >= (mean_FAI-0.1)&
           out_0$FAI <= (mean_FAI+0.1),]$RPX)
mean(out_0[out_0$FAI >= (mean_FAI-0.1)&
           out_0$FAI <= (mean_FAI+0.1),]$TTM)
mean(out_0[out_0$FAI >= (mean_FAI-0.1)&
           out_0$FAI <= (mean_FAI+0.1),]$B19)
mean(out_0[out_0$FAI >= (mean_FAI-0.1)&
           out_0$FAI <= (mean_FAI+0.1),]$CRP)

range(out_0[out_0$FAI >= (mean_FAI-0.1)&
             out_0$FAI <= (mean_FAI+0.1),]$RPX)
range(out_0[out_0$FAI >= (mean_FAI-0.1)&
             out_0$FAI <= (mean_FAI+0.1),]$TTM)
range(out_0[out_0$FAI >= (mean_FAI-0.1)&
             out_0$FAI <= (mean_FAI+0.1),]$B19)
range(out_0[out_0$FAI >= (mean_FAI-0.1)&
             out_0$FAI <= (mean_FAI+0.1),]$CRP)

range(out_0[out_0$FAI > 0,]$RPX)
range(out_0[out_0$FAI > 0,]$TTM)
range(out_0[out_0$FAI > 0,]$B19)
range(out_0[out_0$FAI > 0,]$CRP)

nrow(out_0[out_0$TTM<0,])
range(out_0$FAI)

pop_URCA <- data.frame(matrix(nrow=nrow(URCA_lab),ncol=4))
names(pop_URCA) <- c('URCA', 'URCA_lab','pop')
for(i in seq(nrow(URCA_lab))){
  pop_ <- sum(test_data_orig[test_data_orig$URCA==i,]$pop)
  pop_URCA[i,] <- c(URCA_lab[i,],pop_)
}
pop_URCA$URCA <- factor(pop_URCA$URCA,levels=c("1","4","7","10",
                                 "2","5","8","11",
                                 "3","6","9","12",
                                 "13","14"))
pop_URCA$URCA <- as.factor(pop_URCA$URCA)
pop_URCA$popM <- pop_URCA$pop/1000000
pop_URCA$pop_perc <- (pop_URCA$pop/(sum(pop_URCA$pop)))*100

pop_URCA$city <- ifelse(pop_URCA$URCA %in% c(1,4,7,10),'Large',
                        ifelse(pop_URCA$URCA %in% c(2,5,8,11),'Intermediate',
                               ifelse(pop_URCA$URCA %in% c(3,6,9,12),'Small','Hinterland')))
pop_URCA$color <- ifelse(pop_URCA$city=='Large','red',
                         ifelse(pop_URCA$city=='Intermediate','blue',
                                ifelse(pop_URCA$city=='Small','orange','darkgreen')))
pop_URCAPlot <- ggplot(pop_URCA, 
                      aes(x=URCA,y=popM))+
  geom_bar(stat='identity', width = 0.5, fill=pop_URCA$color)+
  scale_x_discrete(breaks=pop_URCA$URCA, 
                   labels=pop_URCA$URCA_lab)+
  ylab("Population\n(in Millions)")+
  xlab("URCA")+
  ggtitle("Population per URCA")+
  theme(axis.text.x=element_text(size=9, angle=90))+
  scale_y_continuous(labels=scales::label_number(accuracy=1))
pop_URCAPlot
ggsave('./GAM/plots/v3/URCA_pop_v2.png',
       width=8,height=4,units="in")
