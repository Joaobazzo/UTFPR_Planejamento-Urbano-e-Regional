#
# tempo viagem x genero
#
#
require(openxlsx)
require(ggplot2)
require(gridExtra)
require(extrafont)
#
#
rm(list=ls())
setwd("E:/Documents/CICLO/Apresentacoes/Genero_perfil-ciclista/")
#filenew <- read.xlsx("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/deslocamento_arq.xlsx",sheet = 1)


f1 <- function(x){return(7.6508*exp(3.789*x/100))}
e1 <- expression(italic(y)=="7.6508" %.% e^("0.03789"  %.% italic(x)))
e11 <- expression(~~italic(R)^2=="0.45")
ff1 <- function(x){return((0.1199*log(x)-0.0443)*100)}
ef1 <- expression(italic(y)=="0.1199"%.% ln(italic(x)) - "0.0443")
ef11 <- expression(~~italic(R)^2=="0.4542")
f2 <- function(x){return(11.125*x^(-0.804)/(100^(-0.804)))}
e2 <- expression(italic(y)=="451.1282" %.% italic(x)^("-0.804"))
e22 <- expression(italic(R)^2=="0.3")
f3 <- function(x){return(101352*(x)^(-2.47))}
e3 <- expression(italic(y)=="101352" %.% italic(x)^("-2.47"))
e33 <- expression(italic(R)^2=="0.4083")
f4 <- function(x){return(62.352*(x/100)^(1.4041))}
e4 <- expression(italic(y)=="62.352" %.% (italic(x)/"100")^("1.4041"))
e44 <- expression(italic(R)^2=="0.5336")
df_renda <- read.csv("dados/df_renda.csv")
df_renda$renda_media <- df_renda$renda_media*13/1000
df_renda$uso_car_fem <- df_renda$uso_car_fem*100
df_renda$uso_bus <- df_renda$uso_bus*100
df_renda$uso_car <- df_renda$uso_car*100
#plots
bairrosp <- c("Guaíra","Água Verde","Vila Izabel","Portão",
              "Parolin","Seminário","Fazendinha","Santa Quitéria")
df_renda$`Regional` <- df_renda$bairro%in%bairrosp
df_renda$`Regional`[df_renda$`Regional`%in%F] <- "Outras regionais"
df_renda$`Regional`[df_renda$`Regional`%in%T] <- "Reg. Portão"
break()
p11 <- ggplot(df_renda, aes(x=renda_media, y=uso_car_fem,color=Regional, group=1))+
  geom_point(size=1.75)+xlab("Participação feminina entre motoristas(%)")+
  ylab("Renda média anual do bairro (10³ R$)")+
  theme(text=element_text(family="Times New Roman"))+
  stat_function(fun=ff1)+
  annotate("text",family="Times New Roman",label=ef1, parse=TRUE, x=85, y=17, hjust=1.1, vjust=-.5)+
  annotate("text",family="Times New Roman",label=ef11, parse=TRUE, x=81, y=13, hjust=1.1, vjust=-.5)
ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/woman_participation_car_drivers.jpg",
       width = 17,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = p11)

p2 <- ggplot(df_renda, aes(y=renda_media, x=uso_bus,color=`Regional`, group=1))+
  geom_point(size=1.75)+xlab("Uso de TC (%)")+  ylab("Renda média anual do bairro (10³ R$)")+
  theme(text=element_text(family="Times New Roman"))+
  stat_function(fun=f2)+
  annotate("text",family="Times New Roman", label=e2, parse=TRUE,x = 47.0,y=70.0)+
  annotate("text",family="Times New Roman", label=e22, parse=TRUE,x = 43.0,y=66.0)
ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/TC_vs_Renda.jpg",
       width = 17,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = p2)

p3 <- ggplot(df_renda, aes(x=ave_time,y=renda_media,color=`Regional`,group=1))+
    geom_point(size=1.75)+xlab("Tempo médio de viagem (min)")+
  ylab("Renda média anual do bairro (10³ R$)")+
  theme(text=element_text(family="Times New Roman"))+
  stat_function(fun=f3,colour="black")+
  annotate("text",family="Times New Roman", label=e3, parse=TRUE, x=42, y=78, hjust=1.1, vjust=-.5)+
  annotate("text",family="Times New Roman", label=e33, parse=TRUE, x=39, y=74, hjust=1.1, vjust=-.5)

ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/Renda_vs_tempo-viagem.jpg",
       width = 17,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = p3)

p4 <- ggplot(df_renda, aes(y=renda_media, x=uso_car,color=`Regional`, group=1))+
  geom_point(size=1.75)+xlab("Uso do automóvel (%)")+
  ylab("Renda média anual do bairro (10³ R$)")+
  theme(text=element_text(family="Times New Roman"))+
  stat_function(fun=f4)+
  annotate("text",family="Times New Roman", label=e4, parse=TRUE, x=75, y=78, hjust=1.1, vjust=-.5)+
  annotate("text",family="Times New Roman", label=e44, parse=TRUE, x=85, y=74, hjust=1.1, vjust=-.5)

ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/graficos/Renda_vs_uso-auto.jpg",
       width = 17,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = p4)
#pf <- grid.arrange(p1,p2,p3,p4,ncol=2)

# salva
#ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Genero_perfil-ciclista/graficos/pesquisa_od.jpg",
#       width = 35,height = 25,units = "cm",scale = 0.6,dpi = 300,plot = pf)
ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Genero_perfil-ciclista/graficos/woman_participation_car_drivers.jpg",
              width = 17,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = p11)
pf1 <- grid.arrange(p22,p4,p3,ncol=3)
# salva
ggsave(filename = "E:/Documents/CICLO/Apresentacoes/Genero_perfil-ciclista/graficos/pesquisa_od1.jpg",
       width = 40,height = 10,units = "cm",scale = 0.75,dpi = 300,plot = pf1)
break()
#
plot(df_renda$renda_media,df_renda$uso_bus)
plot(df_renda$renda_media,df_renda$uso_bici)
plot(df_renda$renda_media,df_renda$ave_time)
plot(df_renda$renda_media,df_renda$ave_time_bici)
plot(df_renda$renda_media,df_renda$trips_per_pop)
