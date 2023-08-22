library(lubridate)
library(spData)
library(ggsn)
library(kableExtra)
library(cowplot)
library(reshape)
data("world")
require(rerddap)
require(tidyverse)
library(openxlsx)
library(MBA)
whatis=ed_search("NCEP Global Forecast System")

info("ncep_global_lon180")     
"https://coastwatch.pfeg.noaa.gov/erddap/griddap/NCEP_Global_Best.graph"

setwd("Z:/1_Acústica/argos manuales/")

lon = c(-90,-70)

lat =  c(-20,0)
## set temporal extent
FECHA = c("2022-10-10", "2022-10-14")


windo = griddap("ncep_global_lon180", 
               latitude =  lat,
               longitude = lon,
               time = FECHA, 
               fmt = "csv")


windo_df=cbind(windo[,c("time","latitude","longitude","ugrd10m","vgrd10m")])
attach(windo_df)
windo_df[6]=sqrt(ugrd10m^2+ vgrd10m^2)
names(windo_df)[6]="resultante"
windo_df=as_tibble(windo_df)

windo_df = windo_df %>% 
  mutate(DATE = lubridate::as_date(time))

names(windo_df)[1]="Fecha"
names(windo_df)[4:5]=c("u", "v")

eljajajajaj=windo_df%>%
  mutate(year=year(windo_df$Fecha), month=month(windo_df$Fecha), day=day(windo_df$Fecha)) %>% 
  dplyr::select(year, month, day, longitude, latitude, u, v ,resultante)

require(sf)
require(spData)
data("world")
require(gganimate)
library(mapview)
library(mapedit)
library(oce)
library(ggrepel)
library(openxlsx)


puer = read.xlsx("puertos10.xlsx", 1)


aoi = spData::world %>% 
  sf::st_crop(xmin = min(eljajajajaj$longitude)-0.3,xmax=max(eljajajajaj$longitude)+0.3,
              ymin = min(eljajajajaj$latitude)-0.3, ymax=max(eljajajajaj$latitude)+0.3)


col=c("darkblue","blue","cyan","yellow","red","darkred")

#ESCOGER DIA A PLOTEAR
ikenai_bordeline=eljajajajaj%>%filter(day == 14)
mba <- mba.surf(ikenai_bordeline[,c('longitude', 'latitude', 'resultante')], 300, 300)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y) 
ikenai_bordeline_df <- melt(mba$xyz.est$z, varnames = c('longitude', 'latitude'), value.name = 'value')
ikenai_bordeline_df=na.omit(ikenai_bordeline_df)


  p=ggplot() +
  geom_raster(data = ikenai_bordeline_df, 
              aes(x = longitude, y = latitude, fill = value), 
              interpolate= F, na.rm = T)+
  scale_fill_gradientn(limits = c(0,14),colours = col, breaks=seq(0,14,1), 
                       na.value = "white", name = "Speed\n(m/s)")+
  #geom_segment(data = windo_df%>%filter(day==16), aes(x = longitude, xend = longitude+u/60, y = latitude, yend = latitude+v/60), arrow = arrow(length = unit(0.2, "cm")))+
    ggspatial::layer_spatial(data = aoi) +
    coord_sf(xlim = c(-90,-70), 
             ylim = c(-20,-0))+
    theme_bw()+
    theme(panel.background =  element_blank(), legend.key.height  = unit(2.5, "cm"),
          panel.border= element_blank(),
          axis.text = element_text(size = 9, colour = 1),
          panel.grid = element_line(colour = NA), axis.title.x = element_text(vjust = -8),  
          axis.title.y = element_text(vjust = -8)) +
    geom_point(data = puer,aes(x = X,y = Y,label = Puerto), size = 4.5, color = 'red', alpha = .15) +
    geom_text_repel(data = puer ,aes(x = X,y = Y,label = Puerto),
                    nudge_x=1.2, nudge_y=0.1 ,size=3)+
    labs(x = "", y = "")
  
  
  p+annotate(geom="text",x = -76, y = -4.8, size = 4,label="14 Octubre 2022",  colour = "red", parse = F)
  


  ------------------------------------------------------------------



