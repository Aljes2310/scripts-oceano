
library(ggplot2)
library(ggjoy)
library(tidyverse)
library(openxlsx)       
library(reshape2)
library(stringr)
library(stringi)
-------------------------------------------------------------------------------------
#tallas
theme_set(theme_minimal())
setwd("C:/Users/aalvarado/OneDrive - PESQUERA DIAMANTE S.A/Escritorio/argos manuales/")

bio=read.xlsx("biometria calas.xlsx",1)


bio1=melt(bio[1:33])
Latitud=rep(bio$truncar,33)

tallas=as.numeric(str_replace_all(bio1$variable, "X", ""))

dfbio=data.frame(tallas, Latitud, bio1$value)
colnames(dfbio)=c("Tallas","Latitud","Frequencia")
dfbio=na.omit(dfbio)

sizes=rep(dfbio$Tallas,dfbio$Frequencia)
latitud=rep(dfbio$Latitud,dfbio$Frequencia)

dfbio2.0=data.frame(sizes,latitud)
dfbio2.0=dfbio2.0%>%filter(latitud>=-15 & latitud<=-4)

ggplot(dfbio2.0, aes(x = sizes, y = as.factor(latitud))) +
  geom_density_ridges(fill="#00AFBB", bandwidth=1)+
  geom_vline(xintercept = 12, linetype="dotted", 
             color = "red", size=1.5)+
  scale_x_continuous(breaks=seq(0,18,1)) +
  scale_y_discrete(expand=c(0.0002, 0.002))+ labs(x="Tallas(cm)",y="Latitud")+theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 12, colour = 1),
        panel.grid = element_line(colour = NA), axis.title.x = element_text(size=12,vjust = -1),  axis.title.y = element_text(size=12,vjust = 2))

ggplot(dfbio2.0, aes(x = sizes, y = as.factor(latitud))) +
  geom_density_ridges(fill="#00AFBB", bandwidth=0.3)+
  scale_x_continuous(breaks=seq(0,18,2)) +
  scale_y_discrete(expand=c(0.0002, 0.002), limits=rev)+ labs(x="Tallas(cm)",y="DC(mn)")+theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 9, colour = 1),
        panel.grid = element_line(colour = NA))


ggplot(dfbio2.0, aes(x = sizes, y = as.factor(latitud))) +
  geom_density_ridges(fill="#00AFBB")+
  scale_x_continuous(breaks=seq(0,18,2)) +
  scale_y_discrete(expand=c(0.0002, 0.002))+ labs(x="Tallas(cm)",y="Latitud")





#%Porcentajes de juveniles basado en el peso
yuudachi=NULL
list_rango=list()
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))}

lats=sort(unique(Latitud))
DC=sort(unique(Latitud))


lats=lats[!lats %in% -14]


for(i in 1:length(lats)){
  df_raa=dfbio%>%filter(Latitud==lats[i])
  
  #rango de tallas min y max
  min_max=df_raa[df_raa$Frequencia>0,]
  min_max_df=min_max%>%dplyr::select(Frequencia)%>%
    group_by(Tallas=min_max$Tallas, Latitud=min_max$Latitud)%>%
    summarise_if(is.numeric,sum,na.rm=TRUE)
  
  minimo=min(min_max_df$Tallas)
  maximo=max(min_max_df$Tallas)
  moda=min_max_df$Tallas[which.max(min_max_df$Frequencia)]
  rango=data.frame(minimo,maximo,moda)
  list_rango[[i]]=rango 
  
  #%juveniles
  n=sum(df_raa$Frequencia)
  chica=df_raa%>%filter(Tallas<=11.5)
  chicaa=sum(chica$Frequencia)
  yuudachi[i]=chicaa*100/n     }


yuudachi=round(yuudachi,2)
stastic=bind_rows(list_rango)

df_juveniles=data.frame(lats,yuudachi)
Porcentajes_juveniles=na.omit(df_juveniles)
Porcentajes_juveniles=Porcentajes_juveniles%>%arrange(desc(lats))
colnames(Porcentajes_juveniles)[2]="% Juveniles"

seebutsugakunodeta=cbind(Porcentajes_juveniles,stastic)
seebutsugakunodeta






-------------------------------------------------------------------------------------
  
#pesos
bio=read.xlsx("biometria calas.xlsx",1)

bio1=melt(bio[1:33])
Latitud=rep(bio$truncar,33)

tallas=as.numeric(str_replace_all(bio1$variable, "X", ""))

dfbio=data.frame(tallas, Latitud, bio1$value)
colnames(dfbio)=c("Tallas","Latitud","Frequencia")
dfbio=na.omit(dfbio)
  
lats=unique(dfbio$Latitud)
listo=list()
for(i in 1:length(lats)){
  
  dfbio_clase=dfbio%>%filter(Latitud==lats[i])
  n=sum(dfbio_clase$Frequencia)
  df_class=dfbio_clase%>%mutate(Porcentaje=trunc(Frequencia*100/n))
  
  listo[[i]]=df_class  }          

dfbio_0=bind_rows(listo)
dfbio_0=na.omit(dfbio_0)


sizes=rep(dfbio_0$Tallas,dfbio_0$Porcentaje)
latitud=rep(dfbio_0$Latitud,dfbio_0$Porcentaje)

dfbio2.0=data.frame(sizes,latitud)
dfbio2.0=dfbio2.0%>%filter(latitud>=-15 & latitud<=-4)

ggplot(dfbio2.0, aes(x = sizes, y = as.factor(latitud))) +
  geom_density_ridges(fill="#00AFBB")+
  scale_x_continuous(breaks=seq(0,18,2)) +
  scale_y_discrete(expand=c(0.0002, 0.002))+ labs(x="Tallas(cm)",y="Latitud")+theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 9, colour = 1),
        panel.grid = element_line(colour = NA))

ggplot(dfbio2.0, aes(x = sizes, y = as.factor(latitud))) +
  geom_density_ridges(fill="#00AFBB")+
  scale_x_continuous(breaks=seq(0,18,2)) +
  scale_y_discrete(expand=c(0.0002, 0.002), limits=rev)+ labs(x="Tallas(cm)",y="DC")+ theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 9, colour = 1),
        panel.grid = element_line(colour = NA))


#%Porcentajes de juveniles basado en el peso
yuudachi=NULL
list_rango=list()
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))}

lats=lats[!lats %in% -16]

for(i in 1:length(lats)){
  df_raa=dfbio_0%>%filter(Latitud==lats[i])
  
  #rango de tallas min y max
  min_max=df_raa[df_raa$Frequencia>0,]
  minimo=min(min_max$Tallas)
  maximo=max(min_max$Tallas)
  moda=mode(min_max$Tallas)
  rango=data.frame(minimo,maximo,moda)
  list_rango[[i]]=rango 
  
  #%juveniles
  n=sum(df_raa$Frequencia)
  chica=df_raa%>%filter(Tallas<=11.5)
  chicaa=sum(chica$Frequencia)
  yuudachi[i]=chicaa*100/n     }


yuudachi=round(yuudachi,2)
stastic=bind_rows(list_rango)

df_juveniles=data.frame(yuudachi,lats)
Porcentajes_juveniles=na.omit(df_juveniles)
Porcentajes_juveniles=Porcentajes_juveniles%>%arrange(desc(lats))
colnames(Porcentajes_juveniles)[1]="% Juveniles_peso"

seebutsugakunodeta=cbind(Porcentajes_juveniles,stastic)
seebutsugakunodeta






