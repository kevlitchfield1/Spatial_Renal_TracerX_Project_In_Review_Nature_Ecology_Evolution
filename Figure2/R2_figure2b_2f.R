setwd("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2")

library(ggplot2)
library(ggpubr)
library(reshape2)
library(RColorBrewer)
library(ggnewscale)

### Fig.2b
Source_data_fig_2b <- clonal_subclonal_wgii[,c("Centrality","wgii")]
write.csv(Source_data_fig_2b, file="/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig_2b.csv")
Source_data_fig_2b <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig_2b.csv", row.names=1)
head(Source_data_fig_2b)
Source_data_fig_2b
### Boxplot:
ggplot(Source_data_fig_2b, aes(x=Centrality, y=wgii, fill=Centrality))+
  geom_boxplot(width=0.6)+
  theme_classic()+
  scale_fill_manual(values=c(rgb(248,202,173,max=255),rgb(196,90,17,max=255)))+
  labs(x="Location", y="Burden of Somatic Copy Number Alterations\n(wGII score)", fill="Location")+
  theme(axis.text.x=element_text(size=22),
        axis.text.y=element_text(size=22),
        legend.position = "top",
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=20),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22))+
  stat_compare_means(size=6.5)
ggsave("../Results/Comparison_centrality/new/centrality_wgii_boxplot.pdf",plot=last_plot(),width=8,height=6,dpi=600)

### Fig.2c:
Source_data_fig_2c <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig2c.csv", row.names=1)
head(Source_data_fig_2c)
p <- round(fisher.test(Source_data_fig_2c$Centrality,Source_data_fig_2c$Ki67_group)$p.val,5)
value1 <- nrow(Source_data_fig_2c[Source_data_fig_2c$Ki67_group == "Ki67 low" & Source_data_fig_2c$Centrality=="Tumour centre",])/nrow(Source_data_fig_2c[Source_data_fig_2c$Centrality=="Tumour centre",])
value2 <- nrow(Source_data_fig_2c[Source_data_fig_2c$Ki67_group == "Ki67 low" & Source_data_fig_2c$Centrality=="Tumour margin",])/nrow(Source_data_fig_2c[Source_data_fig_2c$Centrality=="Tumour margin",])
ggplot(Source_data_fig_2c, aes(x=Centrality, y=Ki67_group, fill=Ki67_group)) +
  geom_bar(aes(y = ..count../sum(..count..)), position="fill", 
           color="black", width=0.6)+
  scale_fill_brewer(palette = "Set1", direction = 1)+
  labs(x="", y="Frequency", fill="")+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        axis.title=element_text(size=24),
        legend.title=element_text(size=24),
        legend.text=element_text(size=24))+
  annotate(geom="text", x=1, y=1.05, label=paste0("Fisher's exact p = ",p),
           color="black", size=8)+
  annotate(geom = "segment", x=1.3, xend=1.7, y=value1, yend=value2)+
  annotate(geom="segment", x=1.3, xend=1.7, y=1, yend=1)
ggsave("../ki67_new_2x2.pdf", plot=last_plot(), height=7.5, width=10,dpi=600)

### Fig.2d:
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

Source_data_fig_2d <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig2d.csv", row.names=1)
head(Source_data_fig_2d)
p <- round(fisher.test(Source_data_fig_2d$Necrosis, Source_data_fig_2d$Centrality)$p.val,3)
colnames(Source_data_fig_2d)[2] <- "Necrosis"
Source_data_fig_2d$Centrality <- factor(Source_data_fig_2d$Centrality, levels=c("Tumour centre","Tumour margin"))
Source_data_fig_2d_m <- melt(Source_data_fig_2d)
Source_data_fig_2dm_sum <- data_summary(Source_data_fig_2d_m, varname="value", groupnames=c("Centrality","variable"))
head(Source_data_fig_2dm_sum)
ggplot(data=Source_data_fig_2dm_sum, aes(x=variable, y=value, fill=Centrality)) +
  geom_bar(stat="identity", color="black", width=0.7,
           position=position_dodge(width=1))+
  theme_classic()+
  scale_fill_manual(values=c(rgb(248,202,173,max=255),rgb(196,90,17,max=255)))+
  labs(x="", y="Frequency", fill="Location")+
  annotate(geom="text", x=0.75, y=0.071, label=paste0("Fisher's exact p = ",p), size=6.5)+
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title = element_text(size=22),
        legend.position = "top",
        legend.text = element_text(size=22),
        legend.title = element_text(size=22))
ggsave("../Results/Comparison_centrality/new/centrality_necrosis_boxplot_wholeset.pdf",plot=last_plot(),width=8,height=6,dpi=600)


### Fig.2e:
Source_data_fig_2e <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig2e.csv", row.names=1)
head(Source_data_fig_2e)
p <- round(fisher.test(Source_data_fig_2e$Grade,Source_data_fig_2e$Centrality)$p.val,3)
ggplot(Source_data_fig_2e, aes(x=Grade,y = Centrality, fill=Centrality)) +
  geom_bar(aes(y = ..count../sum(..count..)), position="fill", 
           color="black", width=0.6)+
  scale_fill_brewer(palette = "Set1", direction = 1)+
  labs(x="Grade", y="Frequency", fill="")+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        axis.title=element_text(size=24),
        legend.title=element_text(size=24),
        legend.text=element_text(size=24))+
  annotate(geom="text", x=1.3, y=1.05, label=paste0("Fisher's exact p = ", p),
           color="black", size=8)
ggsave("../Results/Comparison_grades/Grades_fisher_comparison.pdf", plot=last_plot(),height=7.5, width=10, dpi=600)

### Fig.2f:
Source_data_fig_2f <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure2/Source_data_fig2f.csv", row.names=1)
head(Source_data_fig_2f)
value1 <- nrow(Source_data_fig_2f[Source_data_fig_2f$Centrality=="Tumour centre" & Source_data_fig_2f$met_clone=="Non-metastasising clone",])/nrow(Source_data_fig_2f[Source_data_fig_2f$Centrality=="Tumour centre",])
value2 <- nrow(Source_data_fig_2f[Source_data_fig_2f$Centrality=="Tumour margin" & Source_data_fig_2f$met_clone=="Non-metastasising clone",])/nrow(Source_data_fig_2f[Source_data_fig_2f$Centrality=="Tumour margin",])
p <- round(fisher.test(Source_data_fig_2f$met_clone, Source_data_fig_2f$Centrality)$p.val,3)
ggplot(Source_data_fig_2f, aes(x=Centrality,y = met_clone,fill=met_clone)) +
  geom_bar(aes(y = ..count../sum(..count..)), position="fill", 
           color="black", width=0.56)+
  scale_fill_brewer(palette = "Set1", direction = 1)+
  labs(x="", y="Frequency", fill="")+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        axis.title.y=element_text(size=24),
        legend.title=element_text(size=24),
        legend.text=element_text(size=24))+
  annotate(geom="text", x=1, y=1.07, label=paste0("Fisher's exact p = ",p),
           color="black", size=8)+
  # y=28/(103+28), yend=48/(80+48)
  annotate(geom = "segment", x=1.28, xend=1.72, y=value1, yend=value2)+
  annotate(geom="segment", x=1.28, xend=1.72, y=1, yend=1)
ggsave("../Results/met_clone/fisher_comparison.pdf", plot=last_plot(),height=7.5, width=10, dpi=600)
