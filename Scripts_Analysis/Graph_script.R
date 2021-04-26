####graphics for the study
library(tidyverse)
library(patchwork)
library(wesanderson)
pal <- wes_palette("Zissou1",4, type = "discrete")


df<-read.csv("Modelling_data.csv") #load raw data

df %>%
  group_by(Island_Type) %>%
  summarise(n())


df<-df %>%
  mutate(range = (Max_A - Min_A)) %>%
  filter(!Study.ID == 29)


#categorical variables with z-value
ecoregions<-df %>%
  ggplot(.) + 
  geom_boxplot(aes(x = Ecoregion, y = Z, fill = Ecoregion)) +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(fill="Biogeographic \nRealm") +
  theme(axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=12, face= "bold"),
        legend.position = "none",
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,angle = 0, face = 'bold', vjust = 0.5))

#By Sar_type
SAR_tyype<-df %>%
  ggplot(.) + 
  geom_boxplot(aes(x = SAR_type, y = Z),fill = "grey") +
  scale_fill_viridis_d(option = "E") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14,angle = 0, face = 'bold', vjust = 0.5),
        axis.title.x = element_blank())

df$Island_Type2<-as.factor(df$Island_Type)
levels(df$Island_Type2)<-c("Continental\n(true island)", "Mainland\nIslands",
                           "Oceanic\n(true island)")

#by island type
island_type<-df %>%
  ggplot(.) + 
  geom_boxplot(aes(x = Island_Type2, y = Z),fill = "grey") +
  labs(fill= "Island Type")+
  scale_fill_manual(values  = pal) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=12, face= "bold"),
        legend.position = "none",
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,angle = 0, face = 'bold', vjust = 0.5))

ecoregions + {
  SAR_tyype + {
    island_type
  }
} +
  plot_layout(ncol=1) +
  plot_annotation(tag_levels = "A")


ggsave('figxy.jpg', 
       height = 5, 
       width = 7,
       dpi = 500)



###preciptiation, r2, z-value
fancy <- df %>%
  filter(! Study.ID ==29) %>%
  dplyr::select(Ecoregion, SAR_type,Lat, Range.order.of.magnitude.,Mean_Annual_Precip, R2, Z)
top <- fancy %>%
  dplyr::select(Ecoregion, Lat,SAR_type,Range.order.of.magnitude., Mean_Annual_Precip, Z) 
top$category = rep("Z", nrow(top))
bottom <- fancy %>%
  dplyr::select(Ecoregion,Lat, SAR_type,Range.order.of.magnitude.,Mean_Annual_Precip, R2) %>%
  mutate(Z = -1*R2) %>%
  dplyr::select(-R2)
bottom$category = rep("R2", nrow(bottom))

fancy<-rbind(top, bottom)


lb1 <- paste("R^2") #label for r-square

figure_x<-ggplot(data = fancy) + 
  geom_bar(stat= "identity", aes(x = Mean_Annual_Precip, y = Z, fill = Ecoregion), 
           position = position_dodge(width = 1), width = 4) +
  geom_point(aes(x = Mean_Annual_Precip, y = Z, fill = Ecoregion),
             pch = 21, color = "black", size = 1.8,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0) +
  labs(x = "Mean Annual Precipitation", y = "", fill = "Biogeographic \nRealm",
       color = "Biogeographic \nRegion") +
  scale_y_continuous(labels = abs(pretty(fancy$Z))) +
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +
  coord_flip(xlim = c(0,3100), ylim = c(-1,1),expand = FALSE) +
  annotate("text", x = 2900, y = 0.5,size = 6, label = "Z-value")+
  annotate("text", x = 2900, y = -0.5,size = 6, label = lb1, parse =T) +
  theme_bw() +
  theme(axis.title.y = element_text(face= "bold", size =14),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10))

figure_x

ggsave('figx.jpg', 
       height = 5, 
       width = 6,
       dpi = 500)

