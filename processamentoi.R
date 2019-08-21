
setwd("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/")
rm(list=ls())
require(ggplot2)
file <- read.csv("arquivos_saida/csv/estendida/deslocamentos_extrapolacao_tx_viagem_variavel.csv")

bairrosp <- c("GUAIRA","AGUA VERDE","VILA IZABEL","PORTAO",
             "PAROLIN","SEMINARIO","FAZENDINHA","SANTA QUITERIA")

#file_p <- file[which(file$bairro_res%in%bairros),]
#file_t <- file[-which(file$bairro_res%in%bairros),]

bairros <- as.character(unique(file$bairro_res))
dtf <- data.frame("bairro"=bairros,
                  "ape"=0,
                  "bici"=0,
                  "car"=0,
                  "bus"=0,
                  "others"=0)
for(i in 1:length(bairros)){
  aux <- file[file$bairro_res%in%bairros[i],]
  # modal
  dtf$ape[i] <- length(which(aux$COD_MEIO%in%14))/dim(aux)[1]
  dtf$bici[i] <- length(which(aux$COD_MEIO%in%13))/dim(aux)[1]
  dtf$car[i] <- length(which(aux$COD_MEIO%in%c(6,8)))/dim(aux)[1]
  dtf$bus[i] <- length(which(aux$COD_MEIO%in%c(1,3,5,4,2,9,10,11)))/dim(aux)[1]
  dtf$others[i] <- length(which(aux$COD_MEIO%in%c(7,0,15)))/dim(aux)[1]
  
}
dtf[,2:6] <- round(100*dtf[,2:6],2)
colnames(dtf) <- c("Bairro","A pé","Bici","Carro","TC","Outros")
head(dtf)
#
# ggplot2-1
#
dtf1 <- data.frame("bairro"=rep(bairros,5),
                   "Modo"=rep(colnames(dtf)[2:6],each=130),
                   "valor"=c(dtf$`A pé`,dtf$Bici,dtf$Carro,
                             dtf$TC,dtf$Outros))

ggplot(dtf1,aes(x=Modo,y=valor,color=Modo))+
  geom_boxplot()+ylab("% repartição modal por bairro")+
  xlab("Modo de transporte")+
  geom_jitter(shape=16, position=position_jitter(0.2))

ggsave("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/reparticao_bairro.jpg",
       width = 25,height = 10,units="cm")
#
# ggplot2-2
#
dtf2 <- data.frame("bairro"=rep(bairros,5),
                   "Modo"=rep(colnames(dtf)[2:6],each=130),
                   "valor"=c(dtf$`A pé`,dtf$Bici,dtf$Carro,
                             dtf$TC,dtf$Outros))
dtf2$`Regional Portão` <- dtf2$bairro%in%bairrosp
dtf2$`Regional Portão`[dtf2$`Regional Portão`%in%F] <- "Outras regionais"
dtf2$`Regional Portão`[dtf2$`Regional Portão`%in%T] <- "Reg. Portão"

ggplot(dtf2,aes(x=Modo,y=valor,color=`Regional Portão`))+
  geom_boxplot()+ylab("% repartição modal por bairro")+
  xlab("Modo de transporte")+
  geom_jitter(shape=16, position=position_jitter(0.2))

ggsave("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/reparticao_bairro_portao.jpg",
       width = 25,height = 10,units="cm")
#
# ggplot2-3
#
dtf1 <- data.frame("bairro"=rep(bairros,5),
                   "Modo"=rep(colnames(dtf)[2:6],each=130),
                   "valor"=c(dtf$`A pé`,dtf$Bici,dtf$Carro,
                             dtf$TC,dtf$Outros))
dtf1 <- dtf1[dtf1$Modo%in%"Bici",]
ggplot(dtf1,aes(x=Modo,y=valor,color=Modo))+
  geom_boxplot()+ylab("% repartição modal por bairro")+
  xlab("Modo de transporte")+
  geom_jitter(shape=16, position=position_jitter(0.2))

ggsave("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/reparticao_bairro_bici.jpg",
       width = 7.5,height = 10,units="cm")
#
# ggplot2-4
#
dtf2 <- data.frame("bairro"=rep(bairros,5),
                   "Modo"=rep(colnames(dtf)[2:6],each=130),
                   "valor"=c(dtf$`A pé`,dtf$Bici,dtf$Carro,
                             dtf$TC,dtf$Outros))
dtf2$`Regional Portão` <- dtf2$bairro%in%bairrosp

dtf2$`Regional Portão`[dtf2$`Regional Portão`%in%F] <- "Outras regionais"
dtf2$`Regional Portão`[dtf2$`Regional Portão`%in%T] <- "Reg. Portão"

dtf2 <- dtf2[dtf2$Modo%in%"Bici",]
ggplot(dtf2,aes(x=Modo,y=valor,color=`Regional Portão`))+
  geom_boxplot()+ylab("% repartição modal por bairro")+
  xlab("Modo de transporte")+
  geom_jitter(shape=16, position=position_jitter(0.2))

ggsave("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/reparticao_bairro_portao_bici.jpg",
       width = 15,height = 10,units="cm")
