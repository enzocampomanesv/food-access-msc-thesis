# Food Access Index Construction in R
# 
# Florencio Campomanes V
# 2022-06-17
#
# This script requires the ff R packages for FAI construction:
#     FactoMiner - for PCA
#     PerformanceAnalytics - if conducting multicollinearity checks
#     corrplot - for multicollinearity diagnostic plots
#   
# This script requires the result from the ETH_ESS_variable_preproc.R script.
# This script also requires the VIFcalc.R script by Thomas Groen.
# 
# Notes:
# (a) All file paths and names should be changed as needed
#

setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia\\")

library(FactoMineR)
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)
library(ggrepel)
library(ggthemr)
library(RColorBrewer)
library(plotly)
# library(reshape2)
# library(processx)
ggthemr("fresh")


rm(list=ls(all=TRUE))
source("./ETH_2018_ESS/VIFcalc.R")

################################################################################
########### START OF PRINCIPAL COMPONENT ANALYSIS ##############

com_joined <- read.csv("./ETH_2018_ESS/LSMS_com_geo_complete_v4.csv")

# select relevant variables
ea <- c("ea_id")
rs <- c("twi","sq1","sq2","sq3","sq4","sq5","sq6","sq7",
             "af_bio_1","af_bio_12",
             "srtm1k","popdensity","cropshare",
             "anntot_avg",
              "ndvi_avg","ndvi_max")
access <- c("road_type", "dist_aspRoad", "dist_bus",
          "dist_woreda", "fare_woreda", "dist_MUC", "fare_MUC",
          "dist_markets")
assist <- c("total_assist")
land <- c("popn","slope","num_hh")
market <- c("price_1","price_2","price_3",
            "price_4","price_5","price_6")
asset <- c("bike_perc", "motor_perc", "cart_h_perc", "cart_a_perc",
                "car_perc", "phone_perc","ker_perc", "cyl_perc",
                "ene_perc", "refr_perc", "shelf_perc",
                "bio_perc",
                "ele_st_perc", "ele_mit_perc")
FI <- c("FI_expense_perc", "FI_transpoCost_perc", "FI_dist_perc")


com_access <- com_joined[,c(access)]
com_asset <- com_joined[,c(asset)]

VIF_remove = 0 # 1 to remove variables a priori based on VIF values

############# INITIAL VARIABLE FILTERING BASED ON CORR AND VIF ###########
####### ASSET #######
if(VIF_remove == 1){
  chart.Correlation(com_asset, 
                    histogram = TRUE, 
                    method = "pearson"
  )
  VIF.asset <- VIFcalc(com_asset)
  VIF.asset
  # remove ele_mit bec high corr with ele_st and refr, and VIF > 5
  com_asset <- com_asset[,names(com_asset) != "ele_mit_perc"]
  VIF.asset <- VIFcalc(com_asset)
  VIF.asset
  # no more VIF > 5
  chart.Correlation(com_asset, 
                    histogram = TRUE, 
                    method = "pearson"
  )
  # high corr bet ele_st and refr
  com_asset <- com_asset[,names(com_asset) != "ele_st_perc"]
  chart.Correlation(com_asset, 
                    histogram = TRUE, 
                    method = "pearson"
  )
  VIF.asset <- VIFcalc(com_asset)
  VIF.asset
  #all VIF values are below 5 and no more corr > 0.75
}
####### ACCESS #######
# fare_MUC and dist_MUC have high corr
# fare_woreda and dist_woreda have high corr
# selection of which to remove will base on VIF
if(VIF_remove==1){
  chart.Correlation(com_access, 
                    histogram = TRUE, 
                    method = "pearson"
  )
  VIF.access <- VIFcalc(com_access) 
  VIF.access
  # fare_MUC has high VIF, recalc VIF without fare_MUC
  VIF.access <- VIFcalc(com_access[,c(-7)])
  VIF.access
  # dist_woreda has VIF>5, recalc VIF without dist_woreda and fare_MUC
  VIF.access <- VIFcalc(com_access[,c(-4,-7)])
  VIF.access
  # no more VIF values > 5, replot corr_plot after removing vars
  com_access <- com_access[,c(-4,-7)]
  chart.Correlation(com_access, 
                    histogram = TRUE, 
                    method = "pearson"
  )
}

#### RS ####
com_rs_only <- com_joined[,c(rs)]
com_rs_only$srtm1k <- NULL
if(VIF_remove==1){
  chart.Correlation(com_rs_only, 
                    histogram = TRUE, 
                    method = "pearson"
  )
  # high corr bet srtm1k:af_bio1, sq1:sq2, sq5:sq6, af_bio12:annual_avg, sq3:sq7
  VIF.rs <- VIFcalc(com_rs_only)
  VIF.rs
  # remove srtm1k bec VIF> 5
  VIF.rs <- VIFcalc(com_rs_only[,c(-11)])
  VIF.rs
  # remove ndvi_max bec VIF > 5
  VIF.rs <- VIFcalc(com_rs_only[,c(-11,-16)])
  VIF.rs
  # remove sq6 bec VIF > 5
  VIF.rs <- VIFcalc(com_rs_only[,c(-11,-16,-7)])
  VIF.rs
  # remove af_bio_12 bec VIF > 5
  VIF.rs <- VIFcalc(com_rs_only[,c(-11,-16,-7,-10)])
  VIF.rs
  # remove sq2 bec VIF > 5
  VIF.rs <- VIFcalc(com_rs_only[,c(-11,-16,-7,-10,-3)])
  VIF.rs
  # remove sq3 bec high VIF
  VIF.rs <- VIFcalc(com_rs_only[,c(-11,-16,-7,-10,-3,-4)])
  VIF.rs
  # replot corr plot after removing vars
  com_rs_only <- com_rs_only[,c(-11,-16,-7,-10,-3,-4)]
  VIF.rs <- VIFcalc(com_rs_only)
  VIF.rs
  chart.Correlation(com_rs_only, 
                    histogram = TRUE, 
                    method = "pearson"
  )
}

#### Food Insecurity ####
if(VIF_remove==1){
  com_FI <- com_joined[,c(FI)]
  chart.Correlation(com_FI, 
                    histogram = TRUE, 
                    method = "pearson"
  )
}
# no high correlations between vars
# in final FAI used in MSc, this was not done.

############ END OF VARIABLE FILTERING ###########

com_base <- com_joined[,c(assist,land,market, FI)] #variables with no filtering

com_rs <- cbind(com_base,com_rs_only)
com_rs <- cbind(com_rs,com_access)
com_rs <- cbind(com_rs,com_asset)

#com_noRS <- cbind(com_base,com_access)
#com_noRS <- cbind(com_noRS,com_asset)

##### PCA #####
pca_RS <- FactoMineR::PCA(com_rs,ncp=2, scale.unit=TRUE,
                          graph=FALSE)

#### scree plot ####
plot_scree <- function(pca, fname=''){
  var_df <- data.frame(PC= rownames(pca$eig),
                                 var_explained=(pca$eig[,2]))
  scree <- var_df[tail(order(var_df$var_explained), 10), ] %>%
    ggplot(aes(x=reorder(PC,-var_explained, sum),y=var_explained, group=1))+
    geom_point(size=4,color="black")+
    geom_line(color="black")+
    xlab("Principal Component")+
    ylab("Variance Explained")+
    labs(title="Scree plot: PCA on scaled data")
  print(scree)
  if(fname!=''){
    out_name <- paste0('./PCA_plots/',fname)
    ggsave(out_name, width = 10, height=6)
  }
  ggplotly(scree)
}
# plot_scree(pca_RS)
# plot_scree(pca_RS, 'PCA_RS_screePlot_v5_CropGrpPrice.png')

# show vars with significant loadings
show_sig_vars <- function(pca_out, coeff_thresh=0.3, pc = 1){
  pca_corr <- pca_out$var$coord[,pc]
  
  print(length(pca_corr))
  pca_insig <- pca_corr[pca_corr<coeff_thresh & 
                                pca_corr>(coeff_thresh*-1)]
  pca_sig <- pca_corr[-which(names(pca_corr) %in% 
                                     names(pca_insig))]
  
  print("insignificant")
  print(pca_insig)
  print("significant")
  print(sort(pca_sig))
  
  print(paste(length(pca_sig),"of",length(pca_sig)+length(pca_insig),
              "are significant"))
  return(pca_sig)
}

#### functions for loading plot ####
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

plot_circ <- function(){
  circ <- circleFun(c(0,0),2,npoints = 500)
  circ.p <-  ggplot() +
    geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
    geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    xlab("PC 1") + 
    ylab("PC 2") +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          panel.border = element_rect(fill= "transparent"))
  ggplotly(circ.p)  
}
plot_var <- function(pca1,fname='',sig_var=1,coeff_thresh=0.3){
  if(sig_var == 1){
    pca.vars <- as.data.frame(pca1$var$coord)
    pca.vars <- pca.vars[pca.vars$Dim.1>coeff_thresh | 
                         pca.vars$Dim.1<(coeff_thresh*-1)|
                         pca.vars$Dim.2>coeff_thresh | 
                         pca.vars$Dim.2<(coeff_thresh*-1),]
    pca.vars$vars <- rownames(pca.vars)
    pca.vars.m <- melt(pca.vars, id.vars = "vars")
    
    pca.ind <- as.data.frame(pca1$ind$coord)
    pc1 <- pca.ind$Dim.1
    pc2 <- pca.ind$Dim.2
    pca.ind$pc1_n <- 2*((pc1-min(pc1))/(max(pc1)-min(pc1)))-1
    pca.ind$pc2_n <- 2*((pc2-min(pc2))/(max(pc2)-min(pc2)))-1
    pca.ind$vars <- rownames(pca.ind)
    pca.ind.m <- melt(pca.ind, id.vars = "vars")
  }else{
    pca.vars <- as.data.frame(pca1$var$coord)
    pca.vars$vars <- rownames(pca.vars)
    pca.vars.m <- melt(pca.vars, id.vars = "vars")
    pca.ind <- as.data.frame(pca1$ind$coord)
    pca.ind$vars <- rownames(pca.ind)
    pca.ind.m <- melt(pca.ind, id.vars = "vars")
  }
  
  circ <- circleFun(c(0,0),2,npoints = 500)
  vars.p <-  ggplot() +
    geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
    geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    geom_segment(data = pca.vars, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
                 arrow = NULL, 
                 lwd = 0.25, alpha = 0.5) + 
    geom_text_repel(data = pca.vars, 
              aes(x = Dim.1*1.05, y =  Dim.2*1.05, 
              label = rownames(pca.vars)),
              max.overlaps = 20, size = 2) +
    #geom_point(data= pca.ind, aes(x=Dim.1,y=Dim.2),
    #           size=1, alpha=0.65)+
    xlab("PC 1") + 
    ylab("PC 2") +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          panel.border = element_rect(fill= "transparent"))
  print(vars.p)
  if(fname!=''){
    out_name <- paste0('./PCA_plots/',fname)
    ggsave(out_name, width=5, height=5)
    #write.csv(pca.vars.m,'./PCA_plots/PCA_RS_SigLoadings.csv')
  }
  ggplotly(vars.p, width = 750, height=750)
}

#significance threshold for n>350 (0.3), for n>250 (0.35)
coeff_thresh = 0.3

# plot_var(pca_RS, sig_var=0,fname='PCA_RS_plot_v5_CropGrpPrice_repel_notSig.png',coeff_thresh=coeff_thresh)
# plot_var(pca_RS)
# plot_var(pca_RS, "PCA_RS_plot.png")

# plot_var(pca_RS,sig_var=0,coeff_thresh=coeff_thresh)

# var loadings
RS1 <- show_sig_vars(pca_RS, pc=1,coeff_thresh=coeff_thresh)
RS2 <- show_sig_vars(pca_RS, pc=2,coeff_thresh=coeff_thresh)

n_RS1 <- names(RS1)

pca_RS_sel <- FactoMineR::PCA(com_rs[,n_RS1],ncp=2, scale.unit=TRUE,
                          graph=FALSE)
plot_scree(pca_RS_sel)
plot_var(pca_RS_sel)
n_RS1 <- show_sig_vars(pca_RS_sel, pc=1,coeff_thresh=coeff_thresh)

pca_var <- pca_RS_sel$var$coord # using with/no RS vars
data_means <- pca_RS_sel$call$centre # get means of each column
data_std <- pca_RS_sel$call$ecart.type # get sd of each column
pca_data <- pca_RS_sel$call$X

pca_data_z <- data.frame(matrix(ncol=ncol(pca_data),nrow=nrow(pca_data)))
com_FAI <- data.frame(matrix(ncol = ncol(pca_data_z), nrow = nrow(pca_data_z))) #empty df

for(i in seq(1,ncol(pca_data))){
  # fill up df by multiplying zScore and loading/coord of 1st PC
    #calculate only for variables with +-coeff_thresh
  pca_data_z[,i] <- (pca_data[,i]-data_means[i])/data_std[i] # calc zScore based on what PCA used
  com_FAI[,i] <- pca_data_z[,i]*pca_var[i] # multiply zScore with loading
  names(com_FAI)[i] <- names(pca_data)[i]
  names(pca_data_z)[i] <- names(pca_data)[i]
}
com_FAI <- com_FAI[colSums(!is.na(com_FAI)) > 0]
com_FAI$FAI <- rowSums(com_FAI)
mean(com_FAI$FAI)

#### calculate FAI ####
pca_var <- pca_RS$var$coord # using with/no RS vars
data_means <- pca_RS$call$centre # get means of each column
data_std <- pca_RS$call$ecart.type # get sd of each column
pca_data <- pca_RS$call$X
#pca_data <- pca_data[,c(-6,-7,-8)] # remove quanti.sup vars (FI vars)
pca_data_z <- data.frame(matrix(ncol=ncol(pca_data),nrow=nrow(pca_data)))
com_FAI <- data.frame(matrix(ncol = ncol(pca_data_z), nrow = nrow(pca_data_z))) #empty df

for(i in seq(1,ncol(pca_data))){
  # fill up df by multiplying zScore and loading/coord of 1st PC
  if(pca_var[i]>coeff_thresh | pca_var[i]< (coeff_thresh*-1)){ 
    #calculate only for variables with +-coeff_thresh
    pca_data_z[,i] <- (pca_data[,i]-data_means[i])/data_std[i] # calc zScore based on what PCA used
    com_FAI[,i] <- pca_data_z[,i]*pca_var[i] # multiply zScore with loading
    names(com_FAI)[i] <- names(pca_data)[i]
    names(pca_data_z)[i] <- names(pca_data)[i]
  }
}
com_FAI <- com_FAI[colSums(!is.na(com_FAI)) > 0]
com_FAI$FAI <- rowSums(com_FAI) # summation of all zScore*loading for FAI

com_ea <- data.frame(com_joined[,c(ea)])
names(com_ea)[1] <- "ea_id"
com_ea$ea_id <- as.character(com_ea$ea_id)
com_FAI <- cbind(com_FAI, com_ea) # join FAI with ea id
com_FAI <- com_FAI[,c(ncol(com_FAI),ncol(com_FAI)-1)] # take only FAI and ea_id
calc_zScore <- function(x,df){
  meanX <- mean(df)
  sdX <- sd(df)
  return((x-meanX)/sdX)
}
com_FAI$FAI_z <- calc_zScore(com_FAI$FAI,com_FAI[,c("FAI")]) #calc zScore of FAI
plot(com_FAI)
sapply(com_FAI, function(x) sum(is.na(x))) # check for NAs
#### write final FAI ####
write.csv(com_FAI, "./ETH_2018_ESS/com_FAI_sig30_noElev_v5_CropGrpPrice.csv")

#### plot histogram of calculated FAI ####
mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(30)
p<-ggplot(com_FAI, aes(x=FAI)) + 
  geom_histogram(color="black", fill=mycolors)+
  ylab("Frequency")+
  xlab("Food Access Index")+
  scale_x_continuous(breaks=seq(-20,25,5))
p
ggsave('./PCA_plots/FAI_histogram_RdBu.png', width=8, height=4)
