library(lubridate)
library(spData)
library(ggsn)
library(kableExtra)
library(cowplot)
library(reshape)
data("world")
require(rerddap)
require(tidyverse)

#Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-
#present (1 Day Composite) 
whichchl = ed_search(query = "MODIS Chlorophyll-a Global 1 Day")
whatis=ed_search("NCEP Global Forecast System")

info("erdMBchla3day_LonPM180")

info("erdMBchla1day_LonPM180")     #1 day 


#pagina de datasets https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000

# set spatial extent

lon = c(-90,-70)

lat =  c(-20,0)
## set temporal extent
FECHA = c("2022-08-01", "2022-08-10")

      
chla = griddap("erdMBchla3day_LonPM180", 
               latitude =  lat,
               longitude = lon,
               time = FECHA, 
               fmt = "csv")

chla = chla %>% 
  mutate(DATE = lubridate::as_date(time))

names(chla)[1]="Fecha"

chla=chla%>%
  mutate(year=year(chla$Fecha), month=month(chla$Fecha), day=day(chla$Fecha)) %>% 
  select(year, month, day, longitude, latitude, chlorophyll)

chla=as_tibble(chla)

chla_prom=chla%>%select(chlorophyll)%>%
group_by(longitude=chla$longitude,latitude=chla$latitude)%>%
  summarise_if(is.numeric, mean , na.rm=TRUE)

setwd("Z:/2_Variables Oceanográficas/3_Clorofilla/2022/8_Agosto/")
write.csv(chla_prom,"chla_prom120822.csv", ,row.names = F)


--------------------------------------

View(chla)

#paleta <- c("darkblue", "cyan", "cadetblue1", "limegreen", "yellow", "orange", "darkorange", "red", "darkred", "brown")
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa",
                 "blue","darkblue")


chla=na.omit(chla)
chla$day=as.numeric(chla$day)

dia_inicial=day(FECHA[1])
dia_final= day(FECHA[2])-7

#moving average orden= i+2
list1=list()
for(i in dia_inicial:dia_final) {df=chla%>%filter(day>=i & day<=i+7) 
  
  df1=df%>%select(chlorophyll)%>%
  group_by(latitude=df$latitude,longitude=df$longitude)%>%
  summarise_if(is.numeric, mean , na.rm=TRUE)
  
  list1[[i]]=list(df)}

chla1day=list1[[24]][[1]]
chla_prom=chla1day %>%select(chlorophyll)%>%
    group_by(latitude=df$latitude,longitude=df$longitude)%>%
    summarise_if(is.numeric, mean , na.rm=TRUE)

chla_prom=na.omit(chla_prom)

#Interpolacion
library(reshape)
library(MBA)
library(metR)
library(colorRamps)

mba <- mba.surf(chla_prom[,c('longitude', 'latitude', 'chlorophyll')], 300, 300)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y) 
df2 <- melt(mba$xyz.est$z, varnames = c('longitude', 'latitude'), value.name = 'chlorophyll')

#mapa chla blankeado
ggplot()+ 
  geom_raster(data = df2, aes(x = longitude, y = latitude, fill = value), 
              interpolate= F, na.rm = T)+
  geom_sf(data = world, fill = "white", col = "white")+
  coord_sf(xlim = c(lon[1], lon[2]), ylim = c(lat[1], lat[2]))+
  scale_fill_gradientn(colours = rev(ODV_colours), trans = scales::log10_trans() )+
  theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 9, colour = 1),
        panel.grid = element_line(colour = NA),
        panel.spacing.x = unit(10, "mm"),
        plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_colorbar(title = expression(Chlorophyll-a~(mg~m^{-3})),
                               title.position = "right",
                               title.theme = element_text(angle = 90), 
                               barwidth = unit(.5, "cm"),barheight = unit(7.5, "cm"),title.hjust = .5))+
  labs(x = NULL, y = NULL)+
  ggtitle("") + 
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position="none",
        panel.border= element_blank())











------------------------------------------------------------

chla.all= chla %>% 
  mutate(year = year(time), month = month(time)) %>% 
  select(year, month, longitude, latitude, chlorophyll)

chla.all=as_tibble(chla.all)

colnames(chla.all)= c("year", "month", "Longitud", "Latitud", "chla")


#zona pota
zonal16_17=chla.all%>%
  filter(Latitud<=-16 & Latitud>=-17 & Longitud <=-73& Longitud >=-75)

zonal17_18=chla.all%>%
  filter(Latitud<=-17 & Latitud>=-18 & Longitud <=-72& Longitud >=-74)

zonal18_19=chla.all%>%
  filter(Latitud<=-18 & Latitud>=-19 & Longitud <=-71& Longitud >=-73)

chla.df=bind_rows(zonal16_17,zonal17_18,zonal18_19)


chla.df$Intervalo=ifelse(chla.df$Latitud<=-16 & chla.df$Latitud>=-17, "16-17?S", 
                 ifelse(chla.df$Latitud<=-17 & chla.df$Latitud>=-18, "17-18?S",
                        ifelse(chla.df$Latitud<=-18 & chla.df$Latitud>=-19, "18-19?S", NA)))


#PROMEDIO MENSUAL
chla.month = chla.df%>% dplyr::select(chla)%>% 
  group_by(year=chla.df$year, month=chla.df$month, Intervalo=chla.df$Intervalo) %>% 
  summarise_if(is.numeric, mean , na.rm=TRUE)


chla.month$month= as.integer(chla.month$month)
colnames(chla.month)= c("DATE", "year", "month", "Longitud", "Latitud", "chla")

chla.month=na.omit(chla.month)

library(scales) 
#ESCALA DE COLORES
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")

chla.month=na.omit(chla.month)
max(chla.month$chlorophyll)
  
  ggplot()+ 
    geom_raster(data = chla.month, aes(x = longitude, y = latitude, fill = chlorophyll))+
    geom_sf(data = world, fill = "grey80", col = "black")+
    coord_sf(xlim = c(lon[1], lon[2]), ylim = c(lat[1], lat[2]))+
    scale_fill_gradientn(colours = ODV_colours , trans = scales::log10_trans())+
    theme_bw()+
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 9, colour = 1),
          panel.grid = element_line(colour = NA),
          panel.spacing.x = unit(10, "mm"),
          plot.title = element_text(hjust = 0.5))+
    guides(fill = guide_colorbar(title = expression(Chlorophyll-a~(mg~m^{-3})),
                                 title.position = "right",
                                 title.theme = element_text(angle = 90), 
                                 barwidth = unit(.5, "cm"),barheight = unit(7.5, "cm"),title.hjust = .5))+
    labs(x = NULL, y = NULL)+
    facet_wrap(~Month, nrow = 2)+
    ggtitle("Clorofila a 2017")
  