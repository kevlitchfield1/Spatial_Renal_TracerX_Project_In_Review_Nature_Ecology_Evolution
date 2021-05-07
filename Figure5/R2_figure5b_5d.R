setwd("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure5")

library(ggplot2)
library(ggpubr)
library(reshape2)
library(RColorBrewer)
library(ggnewscale)

### Fig.5b:
Source_data_fig_5b <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure5/Source_data_fig_5b.csv", row.names=1)
head(Source_data_fig_5b)
ggscatter(Source_data_fig_5b, x="Genomic_distance", y="Spatial_dist_abs",
          add="reg.line",
          color="grey90",
          add.params=list(color="blue",fill="lightblue"),
          cor.coef=T, cor.method="spearman",
          cor.coef.size=5,
          conf.int.level = 0.95, conf.int=T,
          main=paste("Spatial distance vs genomic distance"),
          xlab="Euclidean genomic distance", 
          ylab="Spatial distance (mm)")+
  font("title", size = 20, face = "bold")+
  font("xlab", size = 20, face = "bold")+
  font("ylab", size = 20, face = "bold")+
  font("xy.text", size = 18, face = "plain")
ggsave("../Results/genomic_distance/abs_spearman_grey90.pdf", useDingbats=FALSE, width=8,height=6, dpi=600)

### Fig.5c:
Source_data_fig_5c <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure5/Source_data_fig_5c.csv", row.names=1)
head(Source_data_fig_5c)
subclonal_dist1 <- Source_data_fig_5c[complete.cases(Source_data_fig_5c$distance_travelled),]
subclonal_dist1
subclonal_dist1 <- subclonal_dist1[subclonal_dist1$event!="VHL"
                                   & subclonal_dist1$event!="loss_3p"
                                   & subclonal_dist1$event!="KDM5C" 
                                   & subclonal_dist1$event!="TSC1"
                                   & subclonal_dist1$event!="ARID1A" 
                                   & subclonal_dist1$event!="PIK3CA",]
mut_list <- c("VHL","PBRM1","SETD2","PIK3CA","MTOR","PTEN","KDM5C","BAP1","TP53","TSC1","TSC2","ARID1A","TCEB1")

subclonal_dist1[subclonal_dist1$event %in% mut_list,"Colour"] <- "SNV"
subclonal_dist1[!(subclonal_dist1$event %in% mut_list),"Colour"] <- "CNV"

ggplot(subclonal_dist1, aes(x=fct_reorder(event, desc(distance_travelled), median), y=distance_travelled, fill=Colour))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("lightblue","gold"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=22),
        axis.text.y = element_text(size=22),
        axis.title = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.position = "top")+
  labs(x="Subclonal driver event", y="Maximum spatial distance (mm)", fill="Driver event category")
ggsave("../Results/distance_travelled/new/all_bicolour_mutations_adjusted.pdf", plot=last_plot(), height=7.5, width=10, dpi=600)

### Fig.5d:
Source_data_fig_5d <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure5/Source_data_fig_5d.csv", row.names=1)
head(Source_data_fig_5d)
ggplot(Source_data_fig_5d, aes(Subclonal_frequency, Distance_travelled, label = event)) +
  geom_point(aes(color=Colour), size=2.5)+
  geom_text_repel(parse=T, size=6, nudge_x = 0.003, nudge_y=2)+
  #geom_smooth(method = "lm", se = T, col="blue",fill="lightblue")+
  #geom_abline(intercept=-0.45, slope=0.1, linetype="dashed", color="red")+
  geom_vline(xintercept=median(Source_data_fig_5d$Subclonal_frequency),
             linetype="dashed", color="red")+
  geom_hline(yintercept=median(Source_data_fig_5d$Distance_travelled),
             linetype="dashed", color="red")+
  scale_color_manual(values=c("steelblue","orange"))+
  theme_classic()+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title = element_text(size=22),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.position = "top")+
  labs(x="Subclonal frequency", y="Space occupied (mm)", color="Driver event category")
ggsave("../Results/distance_travelled/new/distance_timing_subclonal.pdf", useDingbats=FALSE, height=7.5, width=10, dpi=600)
