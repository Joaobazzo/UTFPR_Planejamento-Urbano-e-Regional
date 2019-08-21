# ----------------------------------------------------------------------------
#
#   PF - plots
#
# ---------------------------------------------------------------------------
require(openxlsx)
require(ggplot2)
require(gridExtra)
require(plyr)
library(plotly)
library(stringr)
require(extrafont)
setwd("E:/Documents/CICLO/Apresentacoes/Aula_Debora-Rocha/UTFPR_Planejamento-Urbano-e-Regional/perfil ciclista")
pc <- read.xlsx(xlsxFile = "E:/Documents/CICLO/Apresentacoes/Perfil_Ciclista_ppt/cut/PERFIL DO CICLISTA_v1.xlsx",
                sheet = 1,startRow = 2,colNames = T)
#folder <- c("geral/")
folder <- c("portao/")
#pb <- pc[1:56,]
#pv <- pc[373:397,]
#pr <- pc[c(213:217,219:246,329:353),] 
#pmal <- pc[c(57:91,93,293:328),]
#pciclovida <- pc[c(157:179,180:212,827:832,834:848),]
#fm <- pc[which(pc$Q13==2),]
#mm <- pc[which(pc$Q13==1),]
#pbairro <- pc[which(pc$Q7=="CRISTO REI"),]
# ponto de avaliacao
bairrosp <- c("GUAIRA","GUAÍRA","ÁGUA VERDE","AGUA VERDE","VILA IZABEL","PORTÃO",
              "PAROLIN","SEMINARIO","SEMINÁRIO","FAZENDINHA","SANTA QUITÉRIA","SANTA QUITERIA")
pc <- pc[pc$Q7%in%bairrosp,]

# --------------------------------------------------------------------
# QUESTAO 03 - tempo de uso
# --------------------------------------------------------------------
resposta_03 <- c("< 0.5","0.5 - 1","1 - 2","2 - 3",
                 "3 - 4","4 - 5","> 5")
nq_03 <- length(resposta_03)
pc_03 <- pc
w_03 <- c()
m_03 <- c()

for(g in (1:2)){
  pc_03n <- pc_03[which(pc_03$Q13==as.character(g)),]
  for (j in (1:nq_03)){
      w_03[7*(g-1)+j] <- round(100*length(which(pc_03n$Q3==j))/length(pc_03n$Q3),1)
  }
  }
df_03 <- data.frame("resposta"=resposta_03,"g"= c(rep("Masculino",7),rep("Feminino",7))
                    ,"w"=w_03)
df_03$resposta <- factor(df_03$resposta,resposta_03)

q03<- data.frame(idade=rep(paste0("f",1:7),2)[order(rep(paste0("f",1:7),2))],
                 resposta=rep(c("Masculino","Feminino"),7),
                 w=df_03$w[ order(rep(paste0("f",1:7),2))],
                 label=paste0(df_03$w[ order(rep(paste0("f",1:7),2))],"%"))
q03$resposta <- factor(q03$resposta,c("Masculino","Feminino"))
##
# sparados
#
ggplot(q03,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Tempo de uso (anos)") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.1,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:7),
                   labels=resposta_03)+
  ylim(0, 1.05*max(df_03$w))+
  geom_text(data=q03, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q03$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_03.jpeg"),width = 15,
       height = 9.3,dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 02 - finalidade de uso
# --------------------------------------------------------------------
w_2 <- c()
resposta_2 <- c("Trabalho","Estudo","Compras","Lazer/Social")
nq_02 <- length(resposta_2)
pc02 <- pc
for(g in (1:2)){
  pc02n <- pc02[which(pc02$Q13==as.character(g)),]
    w_2[4*(g-1)+1] <- length(which(pc02n$Q2a>0))/length(pc02n$Q2a)
    w_2[4*(g-1)+2] <- length(which(pc02n$Q2b>0))/length(pc02n$Q2b)
    w_2[4*(g-1)+3] <- length(which(pc02n$Q2c>0))/length(pc02n$Q2c)
    w_2[4*(g-1)+4] <- length(which(pc02n$Q2d>0))/length(pc02n$Q2d)
}
w_2 <- round(100*w_2,1)
df_02 <- data.frame("resposta"=resposta_2,"g"= c(rep("Masculino",4),rep("Feminino",4))
                    ,"w"=w_2)
df_02$resposta <- factor(df_02$resposta,resposta_2)

q02<- data.frame(idade=rep(paste0("f",1:4),2)[order(rep(paste0("f",1:4),2))],
                 resposta=rep(c("Masculino","Feminino"),4),
                 w=df_02$w[ order(rep(paste0("f",1:4),2))],
                 label=paste0(df_02$w[ order(rep(paste0("f",1:4),2))],"%"))
q02$resposta <- factor(q02$resposta,c("Masculino","Feminino"))
resposta_2 <- c("Trabalho","Estudo","Compras","Lazer/Social")
nq_2 <- 04
df_2 <- data.frame("resposta"=resposta_2,"w"=w_2,"label"=paste0(w_2,"%"))
# plot
ggplot(q02,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Destino") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.358,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:4),
                   labels=resposta_2)+
  ylim(0, 1.05*max(df_02$w))+
  geom_text(data=q02, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q02$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_02.jpeg"),width = 15,height = 9.3,
       dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 05 - principal problema
# --------------------------------------------------------------------
w_05 <- c()
resposta_05 <- c("Falta de \n segurança \n no trânsito","Falta de \n segurança \n pública",
                 "Falta de \n sinalização",
                 "Falta de \n infraestrutura adequada \n (ciclovias, bicicletários, etc.)",
                 "Outros")
nq_05 <- length(resposta_05)
pc05 <- pc
for(g in (1:2)){
  pc05n <- pc05[which(pc05$Q13==as.character(g)),]
  for (j in (1:nq_05)){
    w_05[5*(g-1)+j] <- round(100*length(which(pc05n$Q5==j))/length(pc05n$Q5),1)
  }
}
w_05 <- c(w_05[6:10],w_05[1:5])
df_05 <- data.frame("resposta"=resposta_05,"g"= c(rep("Feminino",5),rep("Masculino",5))
                    ,"w"=w_05)
df_05$resposta <- factor(df_05$resposta,resposta_05)

q05<- data.frame(idade=rep(paste0("f",1:5),2)[order(rep(paste0("f",1:5),2))],
                 resposta=rep(c("Feminino","Masculino"),5),
                 w=df_05$w[ order(rep(paste0("f",1:5),2))],
                 label=paste0(df_05$w[ order(rep(paste0("f",1:5),2))],"%"))
q05$resposta <- factor(q05$resposta,c("Feminino","Masculino"))
# plot
ggplot(q05,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Problemas") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.9,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:5),
                   labels=resposta_05)+
  ylim(0, 1.05*max(df_05$w))+
  geom_text(data=q05, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q05$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_05.jpeg"),width = 15,height = 9.3,
       dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 10 - escolaridade
# --------------------------------------------------------------------
w_10 <- c()
resposta_10 <-c("Sem instrução",
                        "Ensino Fundamental",
                        "Ensino Médio",
                        "Ensino Superior",
                        "Pós-Graduação")
nq_10 <- length(resposta_10)
pc10 <- pc
for(g in (1:2)){
  pc10n <- pc10[which(pc10$Q13==as.character(g)),]
  for (j in (1:nq_05)){
    w_10[5*(g-1)+j] <- round(100*length(which(pc10n$Q10==j))/length(pc10n$Q10),1)
  }
}
w_10 <- c(w_10[6:10],w_10[1:5])
df_10 <- data.frame("resposta"=resposta_10,
                    "g"= c(rep("Feminino",5),rep("Masculino",5))
                    ,"w"=w_10)
df_10$resposta <- factor(df_10$resposta,resposta_10)

q10<- data.frame(idade=rep(paste0("f",1:5),2)[order(rep(paste0("f",1:5),2))],
                 resposta=rep(c("Feminino","Masculino"),5),
                 w=df_10$w[order(rep(paste0("f",1:5),2))],
                 label=paste0(df_10$w[ order(rep(paste0("f",1:5),2))],"%"))
q10$resposta <- factor(q10$resposta,c("Feminino","Masculino"))
# plot
ggplot(q10,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Escolaridade (último nível completo)") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.9,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:5),
                   labels=resposta_10)+
  ylim(0, 1.05*max(df_10$w))+
  geom_text(data=q10, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q10$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_10.jpeg"),width = 15,height = 9.3,
       dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 15 - motivacao pra pedalar mais
# --------------------------------------------------------------------
w_15 <- c()
resposta_15 <-c("Mais \n segurança/educação \n no trânsito",
                "Mais segurança \n pública",
                "Mais sinalização",
                "Mais e melhores \n infraestruturas adequadas \n (ciclovias, bicicletários, etc.)",
                "Outros")
nq_15 <- length(resposta_15)
pc15 <- pc
for(g in (1:2)){
  pc15n <- pc15[which(pc15$Q13==as.character(g)),]
  for (j in (1:nq_05)){
    w_15[5*(g-1)+j] <- round(100*length(which(pc15n$Q15==j))/length(pc15n$Q15),1)
  }
}
w_15 <- c(w_15[6:10],w_15[1:5])
df_15 <- data.frame("resposta"=resposta_15,
                    "g"= c(rep("Feminino",5),rep("Masculino",5))
                    ,"w"=w_15)
df_15$resposta <- factor(df_15$resposta,resposta_15)

q15<- data.frame(idade=rep(paste0("f",1:5),2)[order(rep(paste0("f",1:5),2))],
                 resposta=rep(c("Feminino","Masculino"),5),
                 w=df_15$w[order(rep(paste0("f",1:5),2))],
                 label=paste0(df_15$w[ order(rep(paste0("f",1:5),2))],"%"))
q15$resposta <- factor(q15$resposta,c("Feminino","Masculino"))
# plot
ggplot(q15,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Motivação para pedalar mais") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.9,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:5),
                   labels=resposta_15)+
  ylim(0, 1.05*max(df_15$w))+
  geom_text(data=q15, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q15$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_15.jpeg"),width = 15,height = 9.3,
       dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 18 - RENDA MENSAL 
# --------------------------------------------------------------------
w_18 <- c()
resposta_18 <-c("Sem renda",
                "Até 1 S.M.",
                "1 - 2 S.M.",
                "2 - 5 S.M.",
                "5 - 10 S.M.",
                "Acima de \n 10 S.M.")
nq_18 <- length(resposta_18)
pc18 <- pc
sm <- 934
for(g in (1:2)){
  pc18n <- pc18[which(pc18$Q13==as.character(g)),]
  w_18[6*(g-1)+1] <- length(which(pc18n$Q18<=100))/length(pc18n$Q18)
  w_18[6*(g-1)+2] <- length(which(pc18n$Q18<=sm&pc18n$Q18>100))/length(pc18n$Q18)
  w_18[6*(g-1)+3] <- length(which(pc18n$Q18<=2*sm&pc18n$Q18>sm))/length(pc18n$Q18)
  w_18[6*(g-1)+4] <- length(which(pc18n$Q18<=5*sm&pc18n$Q18>2*sm))/length(pc18n$Q18)
  w_18[6*(g-1)+5] <- length(which(pc18n$Q18<=10*sm&pc18n$Q18>5*sm))/length(pc18n$Q18)
  w_18[6*(g-1)+6] <- length(which(pc18n$Q18>10*sm))/length(pc18n$Q18)
}
w_18 <- round(100*w_18,1)
w_18 <- c(w_18[7:12],w_18[1:6])
df_18 <- data.frame("resposta"=resposta_18,
                    "g"= c(rep("Feminino",6),rep("Masculino",6))
                    ,"w"=w_18)
df_18$resposta <- factor(df_18$resposta,resposta_18)

q18<- data.frame(idade=rep(paste0("f",1:6),2)[order(rep(paste0("f",1:6),2))],
                 resposta=rep(c("Feminino","Masculino"),6),
                 w=df_18$w[order(rep(paste0("f",1:6),2))],
                 label=paste0(df_18$w[ order(rep(paste0("f",1:6),2))],"%"))
q18$resposta <- factor(q18$resposta,c("Feminino","Masculino"))
# plot
ggplot(q18,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Renda") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.9,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:6),
                   labels=resposta_18)+
  ylim(0, 1.05*max(df_18$w))+
  geom_text(data=q18, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q18$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_18.jpeg"),width = 15,height = 9.3,
       dpi = 420,units = "cm")
# --------------------------------------------------------------------
# QUESTAO 16 - raca
# --------------------------------------------------------------------
w_16 <- c()
resposta_16 <-c("Branca",
                "Preta",
                "Amarela",
                "Parda",
                "Indígena")
nq_16 <- length(resposta_16)
pc16 <- pc
for(g in (1:2)){
  pc16n <- pc16[which(pc16$Q13==as.character(g)),]
  for (j in (1:nq_16)){
    w_16[5*(g-1)+j] <- round(100*length(which(pc16n$Q16==j))/length(pc16n$Q16),1)
  }
}
w_16 <- c(w_16[6:10],w_16[1:5])
df_16 <- data.frame("resposta"=resposta_16,
                    "g"= c(rep("Feminino",5),rep("Masculino",5))
                    ,"w"=w_16)
df_16$resposta <- factor(df_16$resposta,resposta_16)

q16<- data.frame(idade=rep(paste0("f",1:5),2)[order(rep(paste0("f",1:5),2))],
                 resposta=rep(c("Feminino","Masculino"),5),
                 w=df_16$w[order(rep(paste0("f",1:5),2))],
                 label=paste0(df_16$w[ order(rep(paste0("f",1:5),2))],"%"))
q16$resposta <- factor(q16$resposta,c("Feminino","Masculino"))
# plot
ggplot(q16,aes(x=factor(idade),y=w,fill=factor(resposta)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.65,
           size=0.3,colour="black")+
  scale_fill_brewer(palette="Pastel1")+
  xlab("Cor/Raça") + ylab("Percentual de \n Entrevistados(as)")+
  ggtitle("")+
  theme(legend.position=c(0.9,0.9),legend.background=element_blank(),
        text=element_text(family="Arial"),legend.key.size = unit(0.5,"cm"))+
  guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=paste0("f",1:5),
                   labels=resposta_16)+
  ylim(0, 1.05*max(df_16$w))+
  geom_text(data=q16, 
            aes(x=factor(idade),y=w, group=factor(resposta), label=q16$label), 
            position = position_dodge(width=0.75),vjust=-0.35,
            size=2.5)
ggsave(paste0(folder,"qj_16.jpeg"),width = 16,height = 9.3,
       dpi = 420,units = "cm")
