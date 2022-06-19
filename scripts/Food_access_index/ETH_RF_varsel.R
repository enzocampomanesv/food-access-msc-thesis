# Variable selection (for FAI extrapolation) using Random Forest in R
# 
# Florencio Campomanes V
# 2022-06-17
#
# This script requires the ff R packages for variable selection:
#     randomForest - tuning mTry and testing RFs with different variables
#     VSURF - first phase, variable selection (importance and performance)
#     rfPermute - second phase, variable selection (significance)
#
# This script requires the pop-weighted average input data
# from the ETH_geoAveraging_1km.R script.
#
# Notes:
# (a) All file paths and names should be changed as needed
#
setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")

library(randomForest)
library(VSURF)
library(rfPermute)
library(ggplot2)
library(ggthemr)
library(ggpubr)
library(cowplot)
require(plotly)
library(stringr)
library(tidyverse)
ggthemr("fresh")

#library(iRF)
rm(list=ls(all=TRUE))
# load("./RandomForest/RF_ggplot_implem_1km_v5.RData")

d1 <- read.csv("./Data_gridded_1km/ETH_1km_GAMinput_v8.csv")
d1$X <- NULL
d1$ea_id <- as.character(d1$ea_id)
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
d <- d_list[[1]] # take first in list (training)

# variable name preparation
for(i in seq(4,ncol(d)-3)){
  names(d)[i] <- substring(names(d)[i], 3)
}
sq <- c('SQ0_aH','SQ0_aL','SQ1_aL','SQ2_aH',
        'SQ3_aH','SQ3_aL','SQ4_aH','SQ4_aL',
        'SQ5_aH','SQ5_aL','SQ6_aH','SQ6_aL',
        'SQ7_aH','SQ7_aL')
crop_prd <- c('ban_2010','brl_2010','frt_2010','grd_2010',
              'mlt_2010','mze_2010','oce_2010','pls_2010',
              'rt1_2010','rt3_2010','srg_2010','veg_2010',
              'whe_2010')
spm_yld <- c('bana_spm','barl_spm','bean_spm','chic_spm',
             'grou_spm','lent_spm','maiz_spm','ocer_spm',
             'ofib_spm','opul_spm','orts_spm','pota_spm',
             'rest_spm','rice_spm','smil_spm','sorg_spm',
             'swpo_spm','temf_spm','trof_spm','vege_spm',
             'whea_spm','yams_spm', 'cer_frac')
bio <- array(dim=19)
for(i in seq(1,19)){ #19 bioclim vars
  if(i<10){
    num <- paste0('0',as.character(i))
  }else{
    num <- as.character(i)
  }
  bio[i] <- paste0('bio_',num)
}
input_list <- c('LULC','mktTT','Elev','Slope',
                'Road_prox','CRP',bio,sq,spm_yld)
saveRDS(input_list,'./RandomForest/input_list_CRP.RDS')

# end of variable naming preparation

x_pred <- d[,input_list]

x_pred$LULC <- as.factor(x_pred$LULC)
for(i in seq(1,length(sq))){
  x_pred[[sq[i]]] <- as.factor(x_pred[[sq[i]]])
}

rename_vars <- function(x_pred){
  names(x_pred)[1] <- "LLC"
  names(x_pred)[names(x_pred)=='mktTT'] <- "TTM"
  names(x_pred)[3] <- "ELE"
  names(x_pred)[4] <- "SLO"
  names(x_pred)[5] <- "RPX"
  names(x_pred)[6] <- "CRP"
  #names(x_pred)[7] <- "Pulse Price"
  return(x_pred)
}
x_pred <- rename_vars(x_pred)

y_response <- d[,c("FAI")]
range(y_response)

mtry <- floor(sqrt(ncol(x_pred))) #sqrt of number of predictor vars
ntree <- 1000

# determine best mTry from original set of predictor vars
r_forest_tune <- randomForest::tuneRF(x_pred,y_response,
                                      stepFactor = 0.2,
                                      mtryStart = mtry, 
                                      ntreeTry = ntree, 
                                      doBest=TRUE,
                                      importance=TRUE)
best.m <- r_forest_tune$mtry

all_vars <- cbind(x_pred,y_response)
rf_all <- randomForest(y_response~.,data=all_vars,
                       mtry=35, importance=TRUE)
rf_all
r_forest <- randomForest(y_response~.,data=all_data,
                         mtry=4, importance=TRUE)
r_forest

# Function for variable importance (VI) plotting
plot_importance <- function(rf_imp, fname='',n=15, lim = 30){
  imp_plot <- ggplot(rf_imp[1:n,], 
                     aes(x=reorder(rownames(rf_imp[1:n,]),IncMSE),
                         y=IncMSE))+
    geom_bar(stat='identity', width=0.8,
             position = position_dodge2(width = 0.8, preserve = "single")) +
    ylab("%IncMSE")+
    xlab("Variables")+
    ggtitle("Variable Importance")+
    theme(axis.text.x=element_text(size=11),axis.text.y=element_text(size=14))+
    coord_flip(ylim=c(0,lim))
  return(imp_plot)
}

#### Start of VSURF ####
vsurf <- VSURF(x_pred[,names(x_pred) %in% names_sig],
               y_response,
               mtry=best.m,
               nsd=0.02
)
vsurf_obj <- saveRDS(vsurf, file='./RandomForest/vsurf_v2.rds')
vsurf <- readRDS('./RandomForest/vsurf_v2.rds')
#vsurf$varselect.thres
#vsurf$varselect.interp
if(length(vsurf$varselect.pred)<1){
  sel_var <- vsurf$varselect.interp  
}else{
  sel_var <- vsurf$varselect.pred
}
names(x_pred[,sel_var])
vsurf$mean.perf
plot_VSURF <- function(v){
  ve <- as.data.frame(v$err.interp)
  names(ve)[1] <- 'err'
  ve$x <- seq(1,nrow(ve))
  v_plot <- ggplot(ve,
                   aes(x=x,
                       y=err))+
    geom_line(stat='identity', size=1)+
    geom_point(stat='identity', size=3) +
    geom_vline(xintercept=length(v$varselect.interp), color="black", linetype="dashed")+
    ylab("Error")+
    xlab("# of Determinants")+
    ggtitle("VSURF")+
    theme(axis.text.x=element_text(size=11),axis.text.y=element_text(size=14))+
    coord_cartesian(ylim=c(10,20))
  return(v_plot)
}
pl_vsurf <- plot_VSURF(vsurf)
pl_vsurf
ggsave('./RandomForest/RF_plots_1km/VSURF_error_v3.png',
       width=7,height=5,units="in")

### Plots of summary of VSURF results ###
vsurf_summ <- data.frame(matrix(ncol=2,nrow=5))
names(vsurf_summ)[1] <- 'Phase'
names(vsurf_summ)[2] <- '# of Variables'
vsurf_summ$num <- 0
for(i in seq(1,nrow(vsurf_summ))){
  vsurf_summ[i,]$num = i
}
vsurf_summ[1,]$Phase <- 'Original'
vsurf_summ[2,]$Phase <- 'Thresholding'
vsurf_summ[3,]$Phase <- 'Interpretation'
vsurf_summ[4,]$Phase <- 'Prediction'
vsurf_summ[5,]$Phase <- 'Significant'
vsurf_summ[1,]$`# of Variables` <- length(x_pred)
vsurf_summ[2,]$`# of Variables` <- length(vsurf$varselect.thres)
vsurf_summ[3,]$`# of Variables` <- length(vsurf$varselect.interp)
vsurf_summ[4,]$`# of Variables` <- length(vsurf$varselect.pred)
vsurf_summ[5,]$`# of Variables` <- ncol(x_pred_sel)
vs_summ_plot <- ggplot(vsurf_summ, aes(x=fct_reorder(Phase, num),
                                       y=`# of Variables`,
                                       group=1))+
  geom_line(stat='identity', size=1)+
  geom_point(stat='identity', size=3)+
  geom_text(aes(label=`# of Variables`),vjust=2,size=5)+
  xlab("Step")+
  ylab("# of Variables")+
  theme(axis.text.x=element_text(size=11),axis.text.y=element_text(size=14))+
  coord_cartesian()+
  #scale_x_discrete(breaks=as.factor(vsurf_summ$num),labels=vsurf_summ$Phase)+
  scale_y_continuous(limits=c(0,65))
vs_summ_plot
ggsave('./RandomForest/RF_plots_1km/VSURF_summary_v3.png',
       width=6,height=4,units="in")

#### End of VSURF ####

names(x_pred[,sel_var])
rename_preds <- function(df){
  names(df)[names(df)=='bio_19'] <- 'B19'
  names(df)[names(df)=='bio_08'] <- 'B08'
  names(df)[names(df)=='bio_01'] <- 'B01'
  names(df)[names(df)=='bio_09'] <- 'B09'
  names(df)[names(df)=='barl_spm'] <- 'BRL'
  names(df)[names(df)=='sorg_spm'] <- 'SRG'
  names(df)[names(df)=='bean_spm'] <- 'BNS'
  return(df)
}
x_pred <- rename_preds(x_pred)
names(x_pred[,sel_var])
#x_pred_interp <- x_pred[,vsurf$varselect.interp]
x_pred_sel <- x_pred[,sel_var]

# RF Permute for variable importance (2nd phase)
rf_perm_sel <- rfPermute(x=x_pred_sel,y=y_response)
rf_imp_sel <- as.data.frame(rf_perm_sel$rf$importance)
names(rf_imp_sel)[1] <- 'IncMSE'
plot_importance(rf_imp_sel, n=nrow(rf_imp_sel))
ggsave('./RandomForest/RF_plots_1km/VarImportance_afterVSURF_v3.png',
       width=9,height=4,units="in")

rf_perm_sel$pval[,1,2]
sigvar <- names(rf_perm_sel$pval[,1,2])[rf_perm_sel$pval[,1,2]<0.05]
x_pred_sel <- x_pred_sel[,sigvar]
all_data <- cbind(x_pred_sel,y_response)

plot_grid(plot_importance(rf_imp),plot_importance(rf_imp_sel, n=nrow(rf_imp_sel)), align='h')
ggsave('./RandomForest/RF_plots_1km/VarImportance_beforeAfter_v1.png',
       width=14,height=6,units="in")