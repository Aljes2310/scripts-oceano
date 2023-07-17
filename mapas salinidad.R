library(ncdf4)
library(lubridate)
library(spData)
library(ggsn)
library(kableExtra)
library(cowplot)
data("world")
require(rerddap)
require(tidyverse)
library(metR)
library(cmocean)
library(viridis)
library(MBA)
library(metR)
library(cmocean)
library(reshape2)
library(ggpubr)
library(ggrepel)
library(openxlsx)

#parseo data temperatura, salinidad de netcdf a df
sal_temp_0m="salinidad superficial.nc"

n=nc_open(sal_temp_0m)
temp=ncvar_get(n,"thetao")
sal=ncvar_get(n,"so")
depth=ncvar_get(n,"depth")
lat=ncvar_get(n,"latitude")
lon=ncvar_get(n,"longitude")
time=ncvar_get(n,"time")
time=(time/24)+as.Date("1950-01-01")

theta= melt(temp)
so=melt(sal)
salinidad=as.numeric(so$value)
theta$Var1=as.factor(theta$Var1)
theta$Var2=as.factor(theta$Var2)
theta$Var3=as.factor(theta$Var3)
levels(theta$Var1)=as.numeric(lon)
levels(theta$Var2)=as.numeric(lat)
levels(theta$Var3)=time

theta$salinidad=salinidad
colnames(theta)=c("Longitud", "Latitud", "DATE", "thetao", "sal")


theta$Latitud=as.numeric(as.character(theta$Latitud))
theta$Longitud=as.numeric(as.character(theta$Longitud))
theta$DATE=as.Date(theta$DATE)
theta=as_tibble(theta)


#df promedio
theta=theta%>% mutate(month = month(DATE), year=year(DATE))
theta=na.omit(theta)


ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
ODV_colours=rev(ODV_colours)
puer=read.xlsx("puertos10.xlsx",1)
puer=puer%>%filter(Puerto != "Ocoña")



#mapa promedio mensual
df_promedio=theta%>%
  group_by(year=theta$year, month=theta$month, Longitud=theta$Longitud, Latitud=theta$Latitud) %>% 
  summarise_if(is.numeric, mean , na.rm=TRUE)


ggplot() + 
  geom_raster(data = df_promedio%>%filter(year==2017), 
              aes(x = Longitud, y = Latitud, fill = sal), interpolate = FALSE)+
  geom_contour(data = df_promedio, aes(x = Longitud, y = Latitud, z = sal), breaks=seq(34.6,36, 0.1), color="black")+
  stat_contour(data = df_promedio, aes(x = Longitud, y = Latitud,z = sal),breaks = c(35.1), colour = "white", size=1)+
  geom_text_contour(data = df_promedio, aes(x = Longitud, y = Latitud, z = sal), breaks=c(34.8,35.1,35.3), skip=0, stroke = 0.1)+
  geom_sf(data = spData::world, col = "black", fill = "ivory")+
  coord_sf(xlim = c(-80,-69.5), ylim = c(-20,-14))+
  scale_fill_gradientn(name = "Salinidad(PSU)", limits = c(34.4,36), breaks=seq(34.2,36,0.2),
                       colours = ODV_colours, na.value = "White")+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, colour = 1),
        panel.spacing.x = unit(10, "mm"),
        plot.title = element_text(hjust = 0.5), legend.key.height = unit(1, 'cm'),
        axis.title.x = element_text(vjust = -1),  axis.title.y = element_text(vjust = 2))+
  scale_x_continuous(breaks = seq(-78,-70,2))+
  labs(x = NULL, y = NULL)+
  geom_point(data = puer%>%filter(Y<=-15 & Y>=-18.4),aes(x = X,y = Y), size = 3, color = 'red', alpha = .15) +
  geom_text_repel(data = puer%>%filter(Y<=-15& Y>=-18.4) ,aes(x = X,y = Y,label = Puerto),
                  nudge_x=2.3, nudge_y=0.2 ,size=3)+
  facet_wrap(~month, nrow = 4)+
  ggtitle("Salinidad del Mar Superficial (2017)")

