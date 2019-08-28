#
# export shp ####
#
library(sf)
ver <- read_sf("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/PVT_VISUM/shapefile/Modelo OD_Pico Manhã_20180530_link.SHP")
portao <- read_sf("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/shp/portao.shp")

crs = st_crs(31982)
ver <- st_transform(ver,crs)
portao <- st_transform(portao,crs)
ver_pt <- st_crop(ver,portao)
plot(ver_pt$geometry,col="red")
plot(portao$geometry)

ver_pt <- ver_pt[-which(ver_pt$V0PRT<20),]
write_sf(ver_pt,"E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/shp/fluxo.shp")

# plot
namefile <- "E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/fluxo_acum.jpg"
jpeg(namefile, width = 15,height = 10,units="cm",res=300)
plot.ecdf(ver_pt$`VOLVEHPR~5`,main="Fluxo HPM",xlab="Fluxo HPM",ylab="Frequência acumulada")
grid()
dev.off()
