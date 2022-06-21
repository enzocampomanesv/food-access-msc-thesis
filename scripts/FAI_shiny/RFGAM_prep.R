setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")

library(randomForest)
library(mgcv)
library(cowplot)
library(gridGraphics)
require(plotly)
library(ggthemr)
library(tidyverse)
library(ggpubr)
library(stringr)
library(gglorenz)
library(RColorBrewer)
library(ineq)
library(scales)
library(viridis)
# library(ggbeeswarm)
# library(waffle)
# library(extrafont)
# library(extrafontdb)
ggthemr("fresh")

rm(list=ls(all=TRUE))
# loadfonts(device="win",quiet=TRUE)
# load('./RShiny/RF_dataPrep_v5.RData')

test1 <- read.csv('./Data_gridded_1km//value_grids/ETH_valueGrid1km_v8.csv')
f_res_GAM <- read.csv('./Data_gridded_1km//FAI_results//FAI_predictions_optimGAM_v8.csv')
f_res_GAM <- merge(x = f_res_GAM,
               y = test1[,c('c_id','NAME_2')],
               by = "c_id", all.x = TRUE)
names(f_res_GAM)[names(f_res_GAM)=='pred'] <- 'FAI'
gam_out <- readRDS('./GAM/FAI_GAM_v3.rds')
f_res_RF <- read.csv('./Data_gridded_1km//FAI_prediction_v6.csv')
f_res_RF <- merge(x = f_res_RF,
               y = test1[,c('c_id','NAME_2')],
               by = "c_id", all.x = TRUE)
names(f_res_RF)[names(f_res_RF)=='pred_FAI'] <- 'FAI'
r_forest <- readRDS('./RandomForest/RF_v3_fix.rds')
input_list <- readRDS('./RandomForest/input_list_NoPlsPr_keesCereal.RDS')

rename_vars <- function(x_pred, method="RF"){
  if(method=="RF"){
    names(x_pred)[names(x_pred)=='Road_prox'] <- "RPX"
    names(x_pred)[names(x_pred)=='mktTT'] <- "TTM"
    names(x_pred)[names(x_pred)=='Slope'] <- "SLO"
    names(x_pred)[names(x_pred)=='cerPr'] <- "CRP"
    names(x_pred)[names(x_pred)=='bio_19'] <- 'B19'
    names(x_pred)[names(x_pred)=='barl_spm'] <- 'BRL'
    names(x_pred)[names(x_pred)=='sorg_spm'] <- 'SRG'
    names(x_pred)[names(x_pred)=='bean_spm'] <- 'BNS'
  }else{
    names(x_pred)[names(x_pred)=='Road_prox'] <- "RPX"
    names(x_pred)[names(x_pred)=='mktTT'] <- "TTM"
    names(x_pred)[names(x_pred)=='cerPr'] <- "CRP"
    names(x_pred)[names(x_pred)=='bio_19'] <- 'B19'
  }
  
  return(x_pred)
}
get_terms <- function(gam_mod){
  n_terms <- length(gam_mod$smooth)
  terms <- vector("list", length=n_terms)
  for(i in seq(1,n_terms)){
    terms[i] <- gam_mod$smooth[[i]]$term
  }
  return(unlist(terms))
}
terms <- get_terms(gam_out)
prep_data <- function(df, method="RF"){
  if(method=="RF"){
    pred <- names(r_forest$forest$ncat)
  }else{
    pred <- terms
  }
  names(df)[names(df)=='TTM'] <- 'mktTT'
  names(df)[names(df)=='RoadProx'] <- 'Road_prox'
  df <- df[,c(input_list,'c_id','URCA','laty','lonx','pop',
              'NAME_1','NAME_2','TTM_Cf')]
  df <- rename_vars(df)
  #names(df)[names(df)=='bio_19'] <- 'Precipitation of Coldest Quarter'
  df <- df[,c(pred,'c_id','URCA','laty','lonx','pop',
              'NAME_1','NAME_2','TTM_Cf')]
  df$URCA <- as.factor(df$URCA)
  df$URCA <- factor(df$URCA,levels=c("1","4","7","10",
                                      "2","5","8","11",
                                      "3","6","9","12",
                                      "13","14"))
  URCA_lab <- read.csv("./UrbanRural/URCA_labels1.csv")
  URCA_lab$URCA <- as.factor(URCA_lab$URCA)
  df <- merge(x=df,y=URCA_lab,by="URCA", all.x = TRUE)
  return(df)
}
test <- prep_data(test1, "RF")

base_FAI <- function(zone, method="RF"){
  if(zone!='All'){
    if(method=="RF"){
      f_res_sel <- f_res_RF[f_res_RF$NAME_2==zone,c('c_id','URCA_orig','FAI','NAME_2')]
    }else{
      f_res_sel <- f_res_GAM[f_res_GAM$NAME_2==zone,c('c_id','URCA','FAI','NAME_2')]
    }
  }else{
    if(method=="RF"){
      f_res_sel <- f_res_RF[,c('c_id','URCA_orig','FAI','NAME_2')]
    }else{
      f_res_sel <- f_res_GAM[,c('c_id','URCA','FAI','NAME_2')]
    }
  }
  names(f_res_sel)[names(f_res_sel)=='URCA_orig'] <- 'URCA'
  return(f_res_sel)
}

predict_FAI <- function(df, cerMg, precip, cf, zone, method="RF"){
  if(method=="RF"){
    x <- names(r_forest$forest$ncat)
  }else{
    x <- terms
  }
  
  x_pred <- df[,c(x,'c_id','NAME_2','TTM_Cf')]
  if(zone != 'All'){
    x_pred <- x_pred[x_pred$NAME_2==zone,] #filter to zone only
  }
  x_pred$CRP <- x_pred$CRP+(x_pred$CRP*(cerMg/100))
  x_pred$B19 <- x_pred$B19+(x_pred$B19*(precip/100))
  if(cf==TRUE){
    x_pred$TTM <- x_pred$TTM_Cf
  }
  if(method=="RF"){
    y_preds <- predict(r_forest,x_pred[,x])
  }else{
    y_preds <- predict(gam_out,x_pred[,x])
    cap_FAI <- function(df_FAI, cap=50){
      df_FAI[df_FAI>cap] <- cap
      df_FAI[df_FAI< -cap] <- -cap
      return(df_FAI)
    }
    y_preds <- cap_FAI(y_preds)
  }
  y_pred <- cbind(x_pred[,c('c_id')],
                  as.data.frame(y_preds))
  names(y_pred)[1] <- 'c_id'
  names(y_pred)[2] <- 'FAI'
  return(y_pred)
}
plot_map <- function(df, y_pred, base, mod){
  ras_df <- merge(x=y_pred,
                  y=df[,c('lonx','laty','c_id')],
                  by='c_id', all.x=TRUE)
  ras_df <- ras_df[,c('lonx','laty','FAI','c_id')]
  
  names(base)[names(base)=='FAI'] <- 'FAI (baseline)'
  merged <- merge(x=base,
                  y=ras_df,
                  by='c_id', all.x=TRUE)
  names(merged)[names(merged)=='FAI'] <- 'FAI (simulated)'
  
  merged$`FAI (% difference)` <- round(((merged$`FAI (simulated)`-merged$`FAI (baseline)`)/
                                  abs(merged$`FAI (baseline)`))*100,2)
  merged$`FAI (difference)` <- round(merged$`FAI (simulated)`-merged$`FAI (baseline)`,2)
  merged$`FAI (simulated)` <- round(merged$`FAI (simulated)`,2)
  merged$`FAI (baseline)` <- round(merged$`FAI (baseline)`,2)
  
  
  map_colors <- viridis(20)
  diff_colors <- colorRampPalette(c("red3", "seashell2","dodgerblue3"))(20)
  if(mod=='%Difference'){
    map <- ggplot(data=merged,aes(x=lonx,y=laty))+
      geom_tile(aes(fill=`FAI (% difference)`,
                text=paste("Baseline FAI: ", `FAI (baseline)`,
                           "<br>","Simulated FAI: ", `FAI (simulated)`)))+
      scale_fill_gradientn(colors = diff_colors,
                           limits = c(-500,500),
                           oob = squish)
  }else if(mod=='Absolute Difference'){
    map <- ggplot(data=merged,aes(x=lonx,y=laty))+
      geom_tile(aes(fill=`FAI (difference)`,
                    text=paste("Baseline FAI: ", `FAI (baseline)`,
                               "<br>","Simulated FAI: ", `FAI (simulated)`)), data=merged)+
      scale_fill_gradientn(colors = diff_colors,
                           limits = c(-20,20),
                           oob = squish)
    
  }else if (mod=='Simulated'){
    map <- ggplot(data=merged,aes(x=lonx,y=laty))+
      geom_tile(aes(fill=`FAI (simulated)`))+
      scale_fill_gradientn(colors = map_colors,
                           limits = c(-15,15),
                           oob = squish)
  }else{
    map <- ggplot(data=merged,aes(x=lonx,y=laty))+
      geom_tile(aes(fill=`FAI (baseline)`))+
      scale_fill_gradientn(colors = map_colors,
                           limits = c(-15,15),
                           oob = squish)
  }
  map <- map+
    coord_equal()+
    ggtitle("Food Access Index Map")+
    theme_map()
  #ggplotly(map, width=750, height=540)
  ggplotly(map)
}
plot_beeswarm <- function(df, y_pred, base, cerMag,cf,precip){
  df_pred <- merge(x=df,y=y_pred,by="c_id", all.y = TRUE)
  base <- merge(x=df[,c('c_id','URCA_lab')],y=base,by="c_id", all.y = TRUE)
  base$URCA <- factor(base$URCA,levels=c("1","4","7","10",
                                         "2","5","8","11",
                                         "3","6","9","12",
                                         "13","14"))
  #base <- base[base$URCA %in% c(1,2,3,13,14),]
  df_pred$URCA <- factor(df_pred$URCA,levels=c("1","4","7","10",
                                               "2","5","8","11",
                                               "3","6","9","12",
                                               "13","14"))
  #df_pred <- df_pred[df_pred$URCA %in% c(1,2,3,13,14),]
  base$City <- ifelse(base$URCA %in% c(1,4,7,10),'Large',
                      ifelse(base$URCA %in% c(2,5,8,11),'Intermediate',
                             ifelse(base$URCA %in% c(3,6,9,12),'Small',
                                    ifelse(base$URCA %in% c(13),'Dispersed Towns',
                                           'Hinterland'))))
  df_pred$City <- ifelse(df_pred$URCA %in% c(1,4,7,10),'Large (simulated)',
                         ifelse(df_pred$URCA %in% c(2,5,8,11),'Intermediate (simulated)',
                                ifelse(df_pred$URCA %in% c(3,6,9,12),'Small (simulated)',
                                       ifelse(df_pred$URCA %in% c(13),'Dispersed Towns (simulated)',
                                              'Hinterland (simulated)'))))
  urcaxfai <- ggplot(data=NULL, 
                     aes(x=City,y=FAI,col=City))+
    geom_beeswarm(data=base,alpha=0.5,corral="wrap", size=0.4, priority="density") +
    scale_x_discrete(breaks=as.factor(base$City), labels=base$City)
  if(cerMag!=0 | cf==TRUE | precip !=0){
    urcaxfai <- urcaxfai +
      geom_beeswarm(data=df_pred,
                   aes(x=City,y=FAI),
                   alpha=0.3, color="red",corral="wrap",size=0.4, priority="density")
  }
  urcaxfai <- urcaxfai +
    ylab("Food Access Index")+
    xlab("URCA")+
    ggtitle("Food Access Index Levels")+
    geom_hline(yintercept=0, color="blue", linetype="dashed")+
    theme(axis.text.x=element_text(size=9))
  
  #ggplotly(urcaxfai, width=500, height=400)
  ggplotly(urcaxfai)
}

plot_urca <- function(df, y_pred, base, cerMag,cf,precip){
  df_pred <- merge(x=df,y=y_pred,by="c_id", all.y = TRUE)
  base <- merge(x=df[,c('c_id','URCA_lab')],y=base,by="c_id", all.y = TRUE)
  base$URCA <- factor(base$URCA,levels=c("1","4","7","10",
                                                 "2","5","8","11",
                                                 "3","6","9","12",
                                                 "13","14"))
  #base <- base[base$URCA %in% c(1,2,3,13,14),]
  df_pred$URCA <- factor(df_pred$URCA,levels=c("1","4","7","10",
                                               "2","5","8","11",
                                               "3","6","9","12",
                                               "13","14"))
  #df_pred <- df_pred[df_pred$URCA %in% c(1,2,3,13,14),]
  base$City <- ifelse(base$URCA %in% c(1,4,7,10),'Large',
                ifelse(base$URCA %in% c(2,5,8,11),'Intermediate',
                 ifelse(base$URCA %in% c(3,6,9,12),'Small',
                  ifelse(base$URCA %in% c(13),'Dispersed Towns',
                         'Hinterland'))))
  df_pred$City <- ifelse(df_pred$URCA %in% c(1,4,7,10),'Large (simulated)',
                   ifelse(df_pred$URCA %in% c(2,5,8,11),'Intermediate (simulated)',
                    ifelse(df_pred$URCA %in% c(3,6,9,12),'Small (simulated)',
                     ifelse(df_pred$URCA %in% c(13),'Dispersed Towns (simulated)',
                            'Hinterland (simulated)'))))
  df_pred$City <- str_wrap(df_pred$City, width = 12)
  base$City <- str_wrap(base$City, width = 12)
  urcaxfai <- ggplot(data=NULL, 
                     aes(x=City,y=FAI))+
    geom_boxplot(data=base,alpha=1, width=0.1) +
    scale_x_discrete(breaks=as.factor(base$City), labels=base$City)
  if(cerMag!=0 | cf==TRUE | precip !=0){
    comb <- as.data.frame(c(unique(base$City),unique(df_pred$City)))
    names(comb) <- c("City")
    comb$City <- str_wrap(comb$City, width = 12)
    print(comb$City)
    urcaxfai <- urcaxfai +
                geom_boxplot(data=df_pred,
                             aes(x=City,y=FAI),
                             alpha=0.4,color='red',width=0.1)+
                scale_x_discrete(breaks=as.factor(comb$City), labels=comb$City)
  }
  urcaxfai <- urcaxfai +
    ylab("Food Access Index")+
    xlab("URCA")+
    ggtitle("Food Access Index Levels")+
    geom_hline(yintercept=0, color="blue", linetype="dashed")+
    theme(axis.text.x=element_text(size=9))
  
  #ggplotly(urcaxfai, width=500, height=400)
  ggplotly(urcaxfai)
}
norm_zero <- function(v){
  n <- (v-min(v))/(max(v)-min(v))
}
plot_lorenz <- function(df,y_pred, n=c(1,2,3,13,14)){
  df_pred <- merge(x=y_pred,y=df,by="c_id", all.x = TRUE)
  nb.cols <- length(n)
  names(df_pred)[names(df_pred)=='pred_FAI'] <- 'FAI'
  df_pred$FAI_norm <- norm_zero(df_pred$FAI)
  mycolors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(nb.cols)
  zeroy<-(0-min(df_pred$FAI))/(max(df_pred$FAI)-min(df_pred$FAI))
  posy<-(5-min(df_pred$FAI))/(max(df_pred$FAI)-min(df_pred$FAI))
  negy<-(-5-min(df_pred$FAI))/(max(df_pred$FAI)-min(df_pred$FAI))
  
  lcurve <- df_pred %>%
    filter(URCA %in% n)%>%
    mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)))%>%
    ggplot(aes(FAI_norm, color=URCA_lab))+
    scale_color_manual(values=mycolors) +
    stat_lorenz(position='identity') +
    coord_equal() +
    geom_abline(linetype="dashed")+
    #geom_hline(yintercept=zeroy, linetype="dashed", color="black", alpha=0.4)+
    #geom_hline(yintercept=negy, linetype="dashed", color="red", alpha=0.4)+
    #geom_hline(yintercept=posy, linetype="dashed", color="darkgreen", alpha=0.4)+
    theme(legend.title = element_blank()) +
    labs(x = "Cumulative Percentage of Population",
         y = "Cumulative Percentage of Food Access",
         title = "Food Access Inequality")
  #ggplotly(lcurve, width = 500, height=400)
  ggplotly(lcurve)
}
plot_pop <- function(df, y_pred,base1, cerMag,cf,precip){
  d_pop <- merge(x=y_pred,
                 y=df[,c('c_id','pop')],
                 by="c_id", all.x = TRUE)
  base <- merge(x=base1,
                y=df[,c('c_id','pop')],
                by="c_id", all.x = TRUE)
  d_pop$FAI_level <- ifelse(d_pop$FAI<= -5,1,
                      ifelse(d_pop$FAI> -5&d_pop$FAI<=0,2,
                       ifelse(d_pop$FAI>0&d_pop$FAI<=5,3,
                        ifelse(d_pop$FAI>5&d_pop$FAI<10,4,
                         ifelse(d_pop$FAI>10&d_pop$FAI<15,5,6)))))
  base$FAI_level <- ifelse(base$FAI<= -5,1,
                     ifelse(base$FAI> -5&base$FAI<=0,2,
                      ifelse(base$FAI>0&base$FAI<=5,3,
                       ifelse(base$FAI>5&base$FAI<10,4,
                        ifelse(base$FAI>10&base$FAI<15,5,6)))))
  
  d_popAgg <- aggregate(d_pop$pop, by=list(d_pop$FAI_level), sum)
  names(d_popAgg)[1] <- 'FAI_level'
  names(d_popAgg)[2] <- 'Population affected'
  d_popAgg$`Population affected` <- d_popAgg$`Population affected`/1000000
  d_popAgg$FAI_lab <- ifelse(d_popAgg$FAI_level==1,'Less than -5',
                       ifelse(d_popAgg$FAI_level==2,'-5 to 0',
                        ifelse(d_popAgg$FAI_level==3,'0 to 5',
                         ifelse(d_popAgg$FAI_level==4,'5 to 10',
                          ifelse(d_popAgg$FAI_level==5,'10 to 15','Greater than 15')))))
  #d_popAgg$FAI_level <- as.factor(d_popAgg$FAI_level)
  
  baseAgg <- aggregate(base$pop, by=list(base$FAI_level), sum)
  names(baseAgg)[1] <- 'FAI_level'
  names(baseAgg)[2] <- 'Population affected'
  baseAgg$`Population affected` <- baseAgg$`Population affected`/1000000
  baseAgg$FAI_lab <- ifelse(baseAgg$FAI_level==1,'Less than -5',
                      ifelse(baseAgg$FAI_level==2,'-5 to 0',
                       ifelse(baseAgg$FAI_level==3,'0 to 5',
                        ifelse(baseAgg$FAI_level==4,'5 to 10',
                         ifelse(baseAgg$FAI_level==5,'10 to 15','Greater than 15')))))
  #baseAgg$FAI_level <- as.factor(baseAgg$FAI_level)
  if(cerMag!=0 | cf==TRUE | precip !=0){
    baseAgg$Scenario <- 'Baseline'
    d_popAgg$Scenario <- 'Simulated'
    in_col <- c('FAI_level','Population affected','Scenario','FAI_lab')
    merged <- rbind(baseAgg[,in_col],d_popAgg[,in_col])
    FAI_popPlot <- ggplot(merged,
                          aes(x=reorder(FAI_level,FAI_level),y=`Population affected`,fill=Scenario))+
                          geom_bar(position="dodge",stat='identity', width = 0.5)+
                          scale_x_discrete(breaks=merged$FAI_level, 
                                           labels=merged$FAI_lab)
  }else{
    FAI_popPlot <- ggplot(baseAgg, 
                          aes(x=reorder(FAI_level,FAI_level),y=`Population affected`))+
                          geom_bar(stat='identity', width = 0.5)+
                          scale_x_discrete(breaks=baseAgg$FAI_level, 
                                            labels=baseAgg$FAI_lab)
  }
  FAI_popPlot <- FAI_popPlot+
    ylab("Population Affected\n(in Millions)")+
    xlab("FAI Level")+
    ggtitle("Population per FAI level")+
    theme(axis.text.x=element_text(size=9))+
    scale_y_continuous(labels=scales::label_number(accuracy=0.1))+
    coord_flip()
  #ggplotly(FAI_popPlot, width=500, height=300)
  ggplotly(FAI_popPlot)
}
plot_pop_waf <- function(df, y_pred,base1, cerMag,cf,precip){
  d_pop <- merge(x=y_pred,
                 y=df[,c('c_id','pop')],
                 by="c_id", all.x = TRUE)
  base <- merge(x=base1,
                y=df[,c('c_id','pop')],
                by="c_id", all.x = TRUE)
  d_pop$FAI_level <- ifelse(d_pop$FAI<= -5,1,
                      ifelse(d_pop$FAI> -5&d_pop$FAI<=0,2,
                       ifelse(d_pop$FAI>0&d_pop$FAI<=5,3,
                        ifelse(d_pop$FAI>5&d_pop$FAI<10,4,
                         ifelse(d_pop$FAI>10&d_pop$FAI<15,5,6)))))
  base$FAI_level <- ifelse(base$FAI<= -5,1,
                     ifelse(base$FAI> -5&base$FAI<=0,2,
                      ifelse(base$FAI>0&base$FAI<=5,3,
                       ifelse(base$FAI>5&base$FAI<10,4,
                        ifelse(base$FAI>10&base$FAI<15,5,6)))))
  
  d_popAgg <- aggregate(d_pop$pop, by=list(d_pop$FAI_level), sum)
  names(d_popAgg)[1] <- 'FAI_level'
  names(d_popAgg)[2] <- 'Population affected'
  d_popAgg$`Population affected` <- d_popAgg$`Population affected`/100000
  d_popAgg$FAI_lab <- ifelse(d_popAgg$FAI_level==1,'Less than -5',
                       ifelse(d_popAgg$FAI_level==2,'-5 to 0',
                        ifelse(d_popAgg$FAI_level==3,'0 to 5',
                         ifelse(d_popAgg$FAI_level==4,'5 to 10',
                          ifelse(d_popAgg$FAI_level==5,'10 to 15','Greater than 15')))))
  #d_popAgg$FAI_level <- as.factor(d_popAgg$FAI_level)
  
  baseAgg <- aggregate(base$pop, by=list(base$FAI_level), sum)
  names(baseAgg)[1] <- 'FAI_level'
  names(baseAgg)[2] <- 'Population affected'
  baseAgg$`Population affected` <- baseAgg$`Population affected`/100000
  baseAgg$FAI_lab <- ifelse(baseAgg$FAI_level==1,'Less than -5',
                      ifelse(baseAgg$FAI_level==2,'-5 to 0',
                       ifelse(baseAgg$FAI_level==3,'0 to 5',
                        ifelse(baseAgg$FAI_level==4,'5 to 10',
                         ifelse(baseAgg$FAI_level==5,'10 to 15','Greater than 15')))))
  #baseAgg$FAI_level <- as.factor(baseAgg$FAI_level)
  
  if(cerMag!=0 | cf==TRUE | precip !=0){
    baseAgg$Scenario <- 'Baseline'
    d_popAgg$Scenario <- 'Simulated'
    in_col <- c('FAI_level','Population affected','Scenario','FAI_lab')
    merged <- rbind(baseAgg[,in_col],d_popAgg[,in_col])
    n_row <- round((sum(baseAgg$`Population affected`)/8),2)
    n_row <- ifelse(n_row>8,round(n_row/2,2),n_row)
    pop_metrics <- merged$`Population affected`
    # rownames(pop_metrics) <- as.list(merged$FAI_lab)
  }else{
    n_row <- round((sum(baseAgg$`Population affected`)/8),2)
    n_row <- ifelse(n_row>8,round(n_row/2),n_row)
    pop_metrics <- baseAgg$`Population affected`
    # rownames(pop_metrics) <- as.list(baseAgg$FAI_lab)
  }
  plt_waf <- waffle(pop_metrics, rows=n_row,
                    title="Population per FAI level", legend_pos = "bottom",
                    use_glyph="male",glyph_size = 9,
                    glyph_font_family = "Font Awesome 5 Free Solid",
                    glyph_font="FontAwesome")
  ggplotly(plt_waf)
}

plot_ineq <- function(df, y_pred, base1, cerMag,cf,precip, ineq_var="gini"){
  n = c(1,2,3,13,14)
  base <- base1[,c('c_id','URCA','FAI')]
  base$city <- ifelse(base$URCA %in% c(1,4,7,10),'Large',
                ifelse(base$URCA %in% c(2,5,8,11),'Intermediate',
                 ifelse(base$URCA %in% c(3,6,9,12),'Small',
                  ifelse(base$URCA %in% c(13),'Dispersed Towns',
                                                              'Hinterland'))))
  base$FAI_norm <- norm_zero(base$FAI)
  base$scenario <- 'Baseline'
  df_pred <- merge(x=y_pred,y=df,by="c_id", all.x = TRUE)
  df_pred <- df_pred[,c('c_id','URCA','FAI')]
  df_pred$city <- ifelse(df_pred$URCA %in% c(1,4,7,10),'Large',
                    ifelse(df_pred$URCA %in% c(2,5,8,11),'Intermediate',
                     ifelse(df_pred$URCA %in% c(3,6,9,12),'Small',
                      ifelse(df_pred$URCA %in% c(13),'Dispersed Towns',
                                                              'Hinterland'))))
  df_pred$FAI_norm <- norm_zero(df_pred$FAI)
  df_pred$scenario <- 'Simulated'
  
  merged <- rbind(base, df_pred)
  merged$URCA <- as.factor(merged$URCA)
  merged <- merged[merged$URCA %in% n,]
  
  URCA_inq <- aggregate(merged[,2], by=list(merged$URCA, merged$scenario), mean)
  URCA_inq <- as.data.frame(URCA_inq[,c(1,2)])
  names(URCA_inq)[1] <- "URCA"
  names(URCA_inq)[2] <- "Scenario"
  URCA_inq$gini <- 0
  #URCA_lab <- read.csv("./UrbanRural/URCA_labels.csv")
  #URCA_lab$URCA <- as.factor(URCA_lab$URCA)
  #URCA_inq <- merge(x=URCA_inq,y=URCA_lab,by="URCA", all.x = TRUE)
  #URCA_inq$newURCA_lab <- str_wrap(URCA_inq$URCA_lab, width = 12)
  URCA_inq$URCA <- factor(URCA_inq$URCA,levels=c("1","4","7","10",
                                                 "2","5","8","11",
                                                 "3","6","9","12",
                                                 "13","14"))
  URCA_inq$city <- ifelse(URCA_inq$URCA %in% c(1,4,7,10),'Large',
                    ifelse(URCA_inq$URCA %in% c(2,5,8,11),'Intermediate',
                     ifelse(URCA_inq$URCA %in% c(3,6,9,12),'Small',
                      ifelse(URCA_inq$URCA %in% c(13),'Dispersed Towns',
                                                              'Hinterland'))))
  URCA_inq$color <- ifelse(URCA_inq$city=='Large','red',
                           ifelse(URCA_inq$city=='Intermediate','orange',
                                  ifelse(URCA_inq$city=='Small','blue','darkgreen')))
  for(i in unique(URCA_inq$city)){
    URCA_inq[URCA_inq$city==i&URCA_inq$Scenario=='Baseline',]$gini <-
      ineq(base[base$city==i,c('FAI_norm')], type = c("Gini"))
    URCA_inq[URCA_inq$city==i&URCA_inq$Scenario=='Simulated',]$gini <- 
      ineq(df_pred[df_pred$city==i,c('FAI_norm')], type = c("Gini"))
  }
  #URCA_inq <- URCA_inq[URCA_inq$URCA %in% n,]
  base_inq <- URCA_inq[URCA_inq$Scenario=='Baseline',]
  ineq_var <- 'gini'
  ineq <- URCA_inq[,c(ineq_var)]
  ineq_base <- URCA_inq[URCA_inq$Scenario=='Baseline',c(ineq_var)]
  
  if(cerMag!=0 | cf==TRUE | precip !=0){
    plot_ineq <- ggplot(URCA_inq, 
                        aes_string(x="URCA",y=ineq_var,fill="Scenario"))+
      geom_bar(position="dodge",stat="identity")+
      ylab("Gini Index")+
      geom_text(aes(label=sprintf("%0.2f",round(ineq,digits=2))), 
                vjust=0.7, size=3,position=position_dodge(width=1))+
      scale_x_discrete(breaks=as.factor(URCA_inq$URCA),
                       labels=str_wrap(URCA_inq$city, width = 10))
  }else{
    plot_ineq <- ggplot(base_inq,
                        aes_string(x="URCA",y=ineq_var))+
      geom_bar(position="dodge",stat="identity")+
      ylab("Gini Index")+
      geom_text(aes(label=sprintf("%0.2f",round(ineq_base,digits=2))), 
                vjust=0.7, size=3)+
      scale_x_discrete(breaks=as.factor(base_inq$URCA),
                       labels=str_wrap(base_inq$city, width = 10))
  }
  plot_ineq <- plot_ineq +
    theme(axis.text.x=element_text(
      vjust=0.5,
      hjust=1, size=10),
      axis.title.y=element_text(size=11))+
    ggtitle("Inequalities in Food Access")
  
  #ggplotly(plot_ineq, width=500,height=350)
  ggplotly(plot_ineq)
}
rm(test1)
save.image(file='./RShiny/RFGAM_dataPrep_v2.RData')
