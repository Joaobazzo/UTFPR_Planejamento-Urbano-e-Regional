# ---
#
# verify type of street related to the collision
#
# ---
library(sf)
library(sp)
rm(list=ls())
#setwd("")

bateu <- read.csv("E:/Documents/CICLO/Projetos/Analise de Colisoes/colisoes/sistema-bateu/dados/csv/edited/Import_coordinates_bicicleta.csv")
#bici <- read.csv("colisoes/sistema-bateu/dados/csv/Import_coordinates.csv")
#head(bateu)
road <- read_sf("E:/Documents/CICLO/Base Cartografica/bairros/DIVISA_DE_BAIRROS.shp")
cwb <- read_sf("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/mapas/shp/cwb.shp")
# ---
# manipulation ####
# ---
crs = st_crs(31982)
aux <- list()
for(i in 1:length(bateu$lat)){aux[[i]] <- st_point(c(bateu$UTM_x[i],bateu$UTM_y[i]))}
aux1 <- st_sfc(aux)
shp_bateu <- st_sf(bateu,geom=aux1,crs=crs)
# road
road <- st_transform(road,crs=crs)
# cwb
cwb <- st_transform(cwb,crs)
# ---
# intersection ####
# ---
road$colisao <- 0
for(i in 1:length(road$NOME)){
  aux <- st_covered_by(shp_bateu$geom,road$geometry[i])
  aux1 <- aux[-which(aux%in%"integer(0)")]
  
  road$colisao[i] <- length(aux1)
}
break()

road1 <- as(road,'Spatial')
shp_bateu1 <- as(shp_bateu,'Spatial')
# exporta
write_sf(shp_bateu,"E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/shp/colisao.shp")
shp_bateu$id <- 2
jpeg("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/colisao.jpg",
     width = 210,height = 297,units = "mm",res = 200)
spplot(road1,'colisao',main="Colisões")
dev.off()
# segundo
shp_bateu$ano <- str_sub(shp_bateu$Data,start = 7,end = 10)
plot(shp_bateu['ano'],pch=16)

plot(cwb$geometry,add=T)
points(shp_bateu$geom,col="black",pch=16)
print(p1,more=T)
print(p2)
jj <- list("sp.polygons", road)

spplot(shp_bateu, "geom", sp.layout = jj)
# at2
bb <- bbox(road1)
plot(road["colisao"],main="Número de colisões",xlim=as.vector(bb[1,]),ylim=as.vector(bb[2,]))
plot(shp_bateu1,add=T,col="black",pch=16,xlim=as.vector(bb[1,]),ylim=as.vector(bb[2,]))

plot(road['colisao'],xlim=as.vector(bb[1,]),ylim=as.vector(bb[2,]))
plot(road$geometry,add=T,xlim=as.vector(bb[1,]),ylim=as.vector(bb[2,]))
plot(shp_bateu1,add=T)
plot(road["colisao"],add=T)
a <- road$geometry; b <- shp_bateu
shp_bateu$name <- c()
shp_bateu$fclass <- c()
shp_bateu$oneway <- c()
shp_bateu$maxspeed <- c()
for(i in 1:length(shp_bateu$Tipo.Registro)){ # 
  d <- st_nearest_feature(x=b[i,],y=a)
  # road
  aux <- road[d,c("name","fclass","oneway","maxspeed")]
  aux <- as.data.frame(aux)[c(1:4)]
  shp_bateu$name[i] <- aux$name
  shp_bateu$fclass[i] <- aux$fclass
  shp_bateu$oneway[i] <- aux$oneway
  shp_bateu$maxspeed[i] <- aux$maxspeed
}
# --
# saving ####
# --
write_sf(shp_bateu,
         "geolocation_github-repo/geolocalizacao_colisoes/pre-processed_data/shp/bicycle_crash_osmdata-merged.shp")
break()