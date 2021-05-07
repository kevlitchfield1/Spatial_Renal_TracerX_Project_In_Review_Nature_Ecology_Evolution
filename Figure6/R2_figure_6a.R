setwd("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure6")

library(stringr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

Source_data_fig_6a <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure6/Source_data_fig_6a.csv", row.names=1)
head(Source_data_fig_6a)
Source_data_fig_6a$Growth <- factor(Source_data_fig_6a$Growth, levels=c("Dispersed","Contiguous"))

ggplot(data=Source_data_fig_6a, aes(x=Growth, y=Number, fill=Growth)) +
  geom_bar(stat="identity", color="black", width=0.7)+
  theme_classic()+
  coord_flip()+
  scale_fill_manual(values=c("royalblue","pink"))+
  labs(x="", y="Number of cases", fill="Clonal expansion")+
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        axis.text.x = element_text(size=22, face="bold"),
        axis.text.y = element_text(size=22, face="bold"),
        axis.title = element_text(size=22, face="bold"),
        legend.position = "top",
        legend.text = element_text(size=22),
        legend.title = element_text(size=22, face="bold"))
ggsave("../Results/Dispersal/clonal_dispersal_stats_1.pdf",plot=last_plot(),width=10,height=7.5,dpi=600)
