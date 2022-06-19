# Food access inequality quantification in R
# 
# Florencio Campomanes V
# 2022-06-17
#
# This script requires the ff R package/s:
#   DescTools - inequality indices calculation (Gini)
#   gglorenz - Lorenz curve plot
#
# This script requires the annual average FAI predictions 
# from the ETH_GAM.R script.
#
# Notes:
# (a) All file paths and names should be changed as needed
#
setwd("C:\\Users\\enzoc\\Desktop\\MGEO\\NRM\\MSc_Thesis\\Ethiopia")

library(DescTools)
library(ggplot2)
library(gglorenz)
library(cowplot)
library(plotly)
library(RColorBrewer)
library(ggthemr)
library(stringr)
library(tidyverse)
ggthemr("fresh")
rm(list=ls(all=TRUE))
d <- read.csv("./Data_gridded_1km/FAI_results/FAI_predictions_optimGAM_v10.csv")

# d <- left_join(d,pop,by='c_id')
names(d)[names(d)=='pred'] <- 'FAI'
norm_zero <- function(v) {
  n <- (v-min(v))/(max(v)-min(v))
}

d$FAI_norm <- 0
for(i in unique(d$URCA)){
  d[d$URCA==i,]$FAI_norm <- norm_zero(d[d$URCA==i,]$FAI)
}

# d$FAI_norm <- norm_zero(d$FAI*d$p_perc)
names(d)[names(d)=='URCA_orig'] <- "URCA"

URCA_inq <- aggregate(d, by=list(d$URCA), mean)
URCA_inq <- as.data.frame(URCA_inq[,1])
URCA_inq$gini <-  0
# URCA_inq$atk <-URCA_inq$rs <-URCA_inq$theil <- 0
names(URCA_inq)[1] <- "URCA"
URCA_inq$URCA <- as.factor(URCA_inq$URCA)


# for(i in unique(URCA_inq$URCA)){
#   URCA_inq[i,]$gini <- ineq(d[d$URCA==i,c('FAI_norm')], type = c("Gini"))
#   URCA_inq[i,]$atk <- ineq(d[d$URCA==i,c('FAI_norm')], type = c("Atkinson"))
#   URCA_inq[i,]$rs <- ineq(d[d$URCA==i,c('FAI_norm')], type = c("RS"))
#   URCA_inq[i,]$theil <- ineq(d[d$URCA==i,c('FAI_norm')], type = c("Theil"))
# }
for(i in unique(URCA_inq$URCA)){
  URCA_inq[i,]$gini <- ineq(d[d$URCA==i,c('FAI_norm')], type = c("Gini"))
  # URCA_inq[i,]$atk <- ineq(f_ineq[f_ineq$URCA==i,c('FAI_norm')], type = c("Atkinson"))
  # URCA_inq[i,]$rs <- ineq(f_ineq[f_ineq$URCA==i,c('FAI_norm')], type = c("RS"))
  # URCA_inq[i,]$theil <- ineq(f_ineq[f_ineq$URCA==i,c('FAI_norm')], type = c("Theil"))
}

URCA_lab <- read.csv("./UrbanRural/URCA_labels.csv")
URCA_lab$URCA <- as.factor(URCA_lab$URCA)
URCA_inq <- merge(x=URCA_inq,y=URCA_lab,by="URCA", all.x = TRUE)
URCA_inq$newURCA_lab <- str_wrap(URCA_inq$URCA_lab, width = 12)
URCA_inq$URCA <- factor(URCA_inq$URCA,levels=c("1","4","7","10",
                                               "2","5","8","11",
                                               "3","6","9","12",
                                               "13","14"))
URCA_inq$city <- ifelse(URCA_inq$URCA %in% c(1,4,7,10),'Large',
                        ifelse(URCA_inq$URCA %in% c(2,5,8,11),'Intermediate',
                               ifelse(URCA_inq$URCA %in% c(3,6,9,12),'Small','Hinterland')))
URCA_inq$color <- ifelse(URCA_inq$city=='Large','red',
                         ifelse(URCA_inq$city=='Intermediate','blue',
                                ifelse(URCA_inq$city=='Small','orange','darkgreen')))
plot_ineqURCA <- function(URCA_inq, ineq_var, plotly=0, title=1, xl=1, big_ylab=1){
  
  if(ineq_var=="gini"){
    ylabel <- "Gini Index"
    plot_fname <- './Inequality/Gini_barplot.png'
  }else if(ineq_var== "atk"){
    ylabel <- "Atkinson's Measure"
    plot_fname <- './Inequality/Atk_barplot.png'
  }else if(ineq_var== "rs"){
    ylabel <- "Ricci Schutz coefficient"
    plot_fname <- './Inequality/RS_barplot.png'
  }else if(ineq_var== "theil"){
    ylabel <- "Theil's entropy"
    plot_fname <- './Inequality/Theil_barplot.png'
  }
  txt_color <- "black"
  #URCA_inq$URCA_num <- as.integer(URCA_inq$URCA)
  #URCA_inq <- URCA_inq[URCA_inq$URCA_num<4 | URCA_inq$URCA_num>12,]
  ineq <- URCA_inq[,names(URCA_inq)==ineq_var]
  plot_urca <- ggplot(URCA_inq, aes_string(x="URCA",y=ineq_var))+
    geom_bar(stat="identity", fill=URCA_inq$color)+
    ylab(ylabel)+
    geom_text(aes(label=sprintf("%0.2f",round(ineq,digits=2))), 
              hjust=-0.4, size=3, color=txt_color)+
    scale_x_discrete(breaks=URCA_inq$URCA,
                     labels=URCA_inq$newURCA_lab,
                     limits=rev(levels(URCA_inq$URCA)))+
    coord_flip()
    if(title==1){
      plot_urca <- plot_urca + ggtitle("Inequalities in Food Access")
    }
  if(xl==0& big_ylab==1){
    plot_urca <- plot_urca + theme(axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank(),
                                   axis.title.y=element_text(size=14,color=txt_color),
                                   axis.text.y=element_text(color=txt_color))
  }else if(xl==1& big_ylab==0){
    plot_urca <- plot_urca + theme(axis.text.x=element_text(
      angle=90, vjust=0.5,
      hjust=1, size=10,color=txt_color),
      axis.title.y=element_text(size=10,color=txt_color),
      axis.text.y=element_text(color=txt_color))
  }else if(xl==0 & big_ylab==0){
    plot_urca <- plot_urca + theme(axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank(),
                                   axis.title.y=element_text(size=10,color=txt_color),
                                   axis.text.y=element_text(color=txt_color))
  }else{
    plot_urca <- plot_urca + theme(axis.text.x=element_text(
      vjust=0.5,
      hjust=1, size=10,color=txt_color),
      axis.title.y=element_text(size=14,color=txt_color),
      axis.text.y=element_text(color=txt_color))
  }
  # For making plots for presentation (black bg, white text)
  # plot_urca <- plot_urca+
  #   theme(plot.background = element_rect(fill="black",color="black"),
  #         panel.background = element_rect(fill="black"),
  #         axis.line = element_line(color="white"),
  #         panel.grid.minor = element_line(color="grey15"),
  #         panel.grid.major = element_line(color="grey15"),
  #         title=element_text(color="white"),
  #         legend.key=element_rect(fill="black",color="black"),
  #         legend.text = element_text(color="white"),
  #         legend.background = element_rect(fill="black",color=NA),
  #         legend.spacing.y = unit(0.5, 'cm'))
  # print(plot_urca)
  # ggsave(plot_fname)
  if(plotly==1){
    ggplotly(plot_urca)
  }
  return(plot_urca)
}
pl_gini <- plot_ineqURCA(URCA_inq,"gini",0,0,1)
pl_gini
ggsave('./GAM/plots/Gini_GAM_flipXY.png', width=12, height=8)
pl_gini <- plot_ineqURCA(URCA_inq,"gini",0,0,0)
pl_atk <- plot_ineqURCA(URCA_inq,"atk",0,0,0)
pl_rs <- plot_ineqURCA(URCA_inq,"rs",0,0,1)
pl_theil <- plot_ineqURCA(URCA_inq,"theil",0,0,1)
plot_grid(pl_gini, pl_atk,
          pl_rs, pl_theil)
# ggsave('./GAM/plots/AllIneq_GAM.png',
#        width=14,height=8,unit="in")

names(d)[names(d)=='URCA_orig'] <- 'URCA'
d <- merge(x=d,y=URCA_lab,by="URCA", all.x = TRUE)
d$URCA <- as.factor(d$URCA)
d$URCA <- factor(d$URCA,levels=c("1","4","7","10",
                                 "2","5","8","11",
                                 "3","6","9","12",
                                 "13","14"))
d$city <- ifelse(d$URCA %in% c(1,4,7,10),1,
                 ifelse(d$URCA %in% c(2,5,8,11),2,
                        ifelse(d$URCA %in% c(3,6,9,12),3,
                               ifelse(d$URCA %in% c(13),4,
                                      5))))
d$city <- as.factor(d$city)
d$city <- factor(d$city,levels=c("1","2","3","4","5"))
d$city_lab <- ifelse(d$city==1,'Large City',
                     ifelse(d$city==2,'Intermediate City',
                            ifelse(d$city==3,'Small City',
                                   ifelse(d$city==4,'Dispersed Towns',
                                          'Hinterland'))))
d$city_lab <- as.factor(d$city_lab)

plot_lorenz <- function(d, n=c(seq(1,14)), t=""){
  nb.cols <- length(n)
  mycolors <- colorRampPalette(rev(brewer.pal(8, "RdYlGn")))(nb.cols)
  #zeroy<-(0-min(d$pred_FAI))/(max(d$pred_FAI)-min(d$pred_FAI))
  #posy<-(5-min(d$pred_FAI))/(max(d$pred_FAI)-min(d$pred_FAI))
  #negy<-(-5-min(d$pred_FAI))/(max(d$pred_FAI)-min(d$pred_FAI))
  d_sort <- d[order(d$URCA,d$FAI_norm),]
  lcurve <- d_sort %>%
    filter(URCA %in% n)%>%
    mutate(URCA_lab=fct_reorder(URCA_lab,desc(URCA)))%>%
    ggplot(aes(FAI_norm, color=as.factor(URCA_lab)))+
    scale_color_manual(values=mycolors) +
    stat_lorenz(position='identity', size=1.2) +
    # coord_equal() +
    geom_abline(linetype="dashed", color="black")+
    #geom_hline(yintercept=zeroy, linetype="dashed", color="black", alpha=0.4)+
    #geom_hline(yintercept=negy, linetype="dashed", color="red", alpha=0.4)+
    #geom_hline(yintercept=posy, linetype="dashed", color="darkgreen", alpha=0.4)+
    theme(legend.title = element_blank(),
          legend.position="bottom",
          legend.text = element_text(size=12)) +
    labs(x = "Cumulative Percentage of Communities",
         y = "Cumulative Percentage of Food Access",
         title = t)
  # if(nb.cols %in% c(4,5,6)){
  #   lcurve <- lcurve+
  #     guides(color=guide_legend(nrow=2,byrow=TRUE))
  # }
  print(lcurve)
  ggplotly(lcurve, width = 750, height=750)
}
# plot_lorenz(d)
# out_fname <- paste0('./GAM/plots/LorenzCurve_14_v4.png')
# ggsave(out_fname, width=10, height=5)
# hrs1 <- plot_lorenz(d, n=c(4,5,6))
# out_fname <- paste0('./GAM/plots/LorenzCurve_1hr_v4.png')
# ggsave(out_fname, width=7, height=5)
# hrs12 <- plot_lorenz(d, n=c(7,8,9))
# out_fname <- paste0('./GAM/plots/LorenzCurve_1-2hr_v4.png')
# ggsave(out_fname, width=7.5, height=5)
# hrs23 <- plot_lorenz(d, n=c(10,11,12))
# out_fname <- paste0('./GAM/plots/LorenzCurve_2-3hr_v4.png')
# ggsave(out_fname, width=7.5, height=5)

cnt_plot <- plot_lorenz(d, n=c(1,2,3,13,14))
cnt_plot
out_fname <- paste0('./GAM/plots/LorenzCurve_5_v4.png')
ggsave(out_fname, width=9, height=5)
large <- plot_lorenz(d, n=c(1,4,7,10), t="Large cities")
large
out_fname <- paste0('./GAM/plots/LorenzCurve_large_v4.png')
ggsave(out_fname, width=8, height=5)
inter <- plot_lorenz(d, n=c(2,5,8,11), t="Intermediate cities")
out_fname <- paste0('./GAM/plots/LorenzCurve_inter_v4.png')
ggsave(out_fname, width=8, height=5)
small <- plot_lorenz(d, n=c(3,6,9,12), t="Small cities")
out_fname <- paste0('./GAM/plots/LorenzCurve_small_v4.png')
ggsave(out_fname, width=8, height=5)


plot_grid(large,inter,small,nrow=3)
out_fname <- paste0('./GAM/plots/LorenzCurve_grid_v4.png')
ggsave(out_fname, width=9, height=11.5)
