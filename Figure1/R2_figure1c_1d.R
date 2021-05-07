library(ggplot2)

file1 <- read.csv("../Source_data_fig1c.csv")

### Distribution of tumour volume:
ggplot(file1, aes(x=max_diameter)) + 
  geom_histogram(binwidth=20, colour="black",fill="steelblue")+
  theme_classic()+
  labs(x="Tumour size (mm)", y="Frequency", title = "Distribution of tumour size")+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22, face="bold"))
ggsave("../Results/distributions/fig1c_distribution of tumour size.pdf", width=8, height=6, dpi=600)

### Plot the distribution of distance_to_boundary:
all_boundary <- read.csv("/Users/zhaoy2/Desktop/Spatial_project/manuscript/2nd revision/Codes/Figure1/Source_data_fig1d.csv")
ggplot(all_boundary, aes(x=dist_to_boundary)) + 
  geom_histogram(binwidth=0.98, colour="black",fill="steelblue")+
  theme_classic()+
  labs(x="Distance to boundary (mm)", y="Frequency", title = "Distribution of distances between region and boundary")+
  theme(plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title = element_text(size=22),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22))
ggsave("../Results/distributions/fig1d_distribution of distance to boundary.pdf", width=8, height=6, dpi=600)

