# Population weighted averaging geospatial input in R
# 
# Florencio Campomanes V
# 2022-02-25
#
# This script only requires base R packages.
#
# This script should be run after obtaining results from spatial joining
# the value grids with the average grids in GIS.
#
# Notes:
# (a) All file paths and names should be changed as needed
#

setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")

rm(list=ls(all=TRUE))
################################################################################
########### START OF POP-WEIGHTED AVERAGING ##############

d <- read.csv("./Data_gridded_1km/value_grids/ETH_valueGrid1km_v9_avg.csv")
d$ea_id1 <- as.character(d$ea_id1)
names(d)[names(d) == "popn_sum"] <- "pop18"
names(d)[names(d) == "ea_id1"] <- "ea_id"

com <- read.csv("./ETH_2018_ESS/sect03_com_w4.csv")
com <- com[,c(1:2)]
names(com)[2] <- 'RU'
com$ea_id <- as.character(com$ea_id)
com$tcel <- ifelse(com$RU=="1. RURAL", 100,16) # total cells to avg

d <- d[order(d$ea_id),]
d$pop_wt <- 0

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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

d_agg <- data.frame(unique(d$ea_id))
names(d_agg)[1] <- "ea_id"
d_agg$tpop <- d_agg$ncell <- 0
d_agg$m_URCA <- d_agg$m_LULC <- 0
sqvar <- array(length(sq))
#cropvar <- array(length(crop_prd))
yldvar <- array(length(spm_yld))
biovar <- array(length(bio))
for(i in seq(1,length(sq))){
  sqvar[i] <- paste0('m_',sq[i])
  d_agg[[sqvar[i]]] <- 0
}
#for(i in seq(1,length(crop_prd))){
#  cropvar[i] <- paste0('w_',crop_prd[i])
#}
for(i in seq(1,length(spm_yld))){
  yldvar[i] <- paste0('w_',spm_yld[i])
}
for(i in seq(1,length(bio))){
  biovar[i] <- paste0('w_',bio[i])
}
for(i in seq(1:nrow(d_agg))){
  id <- d_agg[i,]$ea_id
  n <- 0 # number of cells for that ea
  t_pop <- 0 # initialize total population at 0
  
  d_ea <- d[d$ea_id==id,]
  d_URCA <- d_ea$URCA
  d_LULC <- d_ea$LULC
  vr <- array(dim=length(sq))
  for(k in seq(1,length(sq))){
    vr[k] <- d_ea[[sq[k]]]
  }
  
  for(j in seq(1:nrow(d_ea))){
    ## get pop weighted avg
    n <- n+1
    t_pop <- t_pop + d_ea[j,]$pop18
  }
  d_agg[i,]$m_URCA <- max(d_URCA)
  d_agg[i,]$m_LULC <- min(d_LULC)
  for(k in seq(1,length(sqvar))){
    d_agg[i,][[sqvar[k]]] <- getmode(vr[k])
  }
  #d_agg[i,]$m_URCA <- floor(mean(d_URCA))
  d_agg[i,]$tpop <- t_pop
  d_agg[i,]$ncell <- n
}
d_join <- merge(x = d[,-which(names(d) %in% sq) ], 
                    y = d_agg, 
                    by = "ea_id", all.x = TRUE)
d_join <- merge(x = d_join,
                y = com,
                by = "ea_id", all.x = TRUE)

d_join$pop_wt <- d_join$pop18/d_join$tpop
d_join$w_mktTT <- d_join$MktTT*d_join$pop_wt
d_join$w_Elev <- d_join$Elev*d_join$pop_wt
d_join$w_Slope <- d_join$Slope*d_join$pop_wt
d_join$w_Road_prox <- d_join$RoadProx*d_join$pop_wt
d_join$w_rwi <- d_join$RWI*d_join$pop_wt
d_join$w_cerPr <- d_join$cerPr*d_join$pop_wt
d_join$w_plsPr <- d_join$plsPr*d_join$pop_wt
d_join$w_CRP <- d_join$CRP*d_join$pop_wt
#for(i in seq(1,length(crop_prd))){
#  d_join[[cropvar[i]]] <- d_join[[crop_prd[i]]]*d_join$pop_wt
#}
for(i in seq(1,length(bio))){
  d_join[[biovar[i]]] <- d_join[[bio[i]]]*d_join$pop_wt
}
for(i in seq(1,length(spm_yld))){
  d_join[[yldvar[i]]] <- d_join[[spm_yld[i]]]*d_join$pop_wt
}

s_input_list <- c('w_mktTT',
                'w_Elev','w_Slope','w_Road_prox','w_rwi',
                'w_cerPr','w_plsPr','w_CRP', biovar,yldvar)
m_input_list <- c('RWI','pop18','ncell','m_URCA','m_LULC','tcel', sqvar)
d_sum <- aggregate(d_join[,s_input_list], by=list(d_join$ea_id), sum)
d_sum$Group.1 <- NULL
d_mean <- aggregate(d_join[,m_input_list], by=list(d_join$ea_id), mean)
d_final <- cbind(d_mean, d_sum)
names(d_final)[1] <- "ea_id"

d_final$incl <- d_final$ncell/d_final$tcel # calc ratio of present cells vs needed cells
d_final <- d_final[d_final$incl>=0.5,] # removes 82 EAs with less than half of cells in 4x4 or 2x2 buffer
d_final$incl <- d_final$ncell <- d_final$tcel <- NULL
sort(unique(d_final$m_URCA))
sort(unique(d_final$m_LULC))

# join FAI
fai <- read.csv("./ETH_2018_ESS/com_FAI_sig30_noElev_v5_CropGrpPrice.csv")
fai$X <- NULL
fai$ea_id <- as.character(fai$ea_id)
fai_final <- merge(x = d_final,
                 y = fai,
                 by = "ea_id", all.x = TRUE)
fai_final <- merge(x = fai_final,
                   y = com[,c(1,2)],
                   by = "ea_id", all.x = TRUE)
fai_final <- fai_final[complete.cases(fai_final),]
write.csv(fai_final, "./Data_gridded_1km/ETH_1km_GAMinput_v9.csv")
