library(ggplot2)


dx <- data.frame(x=c(3,5.2,-5.2,0,0),
                 y=c(-2,0,0,5.2,-5.2),
                 label=c("Produit E2",
                         "Qualité Haut",
                         "Qualité bas",
                         "Prix haut",
                         "Prix bas"),
                 col=factor(c(1,0,0,0,0)))

ggplot(dx,aes(x=x,y=y,col=col)) +
  geom_label(aes(label=label)) +
  geom_segment(y=0,yend=0,x=-4.8,xend=4.8) +
  geom_segment(x=0,xend=0,y=-4.8,yend=4.8) +
 ggthemes::theme_economist() +
  coord_cartesian(xlim = c(-5.5, 5.5),ylim=c(-5.5,5.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +xlab("")+ylab("") +
  ggtitle("Positionnement Famille E2") +
  scale_color_manual(values=c("black","green"))



dx <- data.frame(x=c(0.7,-0.7,0.7,5.2,-5.2,0  ,0),
                 y=c(4,2,1.5,0  ,0   ,5.2,-5.2),
                 label=c("Javel Import",
                         "Javel Concurent",
                         "Javel Amin",
                         "Qualité Haut",
                         "Qualité bas",
                         "Prix haut",
                         "Prix bas"),
                 col=factor(c(1,2,3,0,0,0,0)))

ggplot(dx,aes(x=x,y=y,col=col)) +
  geom_label(aes(label=label)) +
  geom_segment(y=0,yend=0,x=-4.8,xend=4.8) +
  geom_segment(x=0,xend=0,y=-4.8,yend=4.8) +
  ggthemes::theme_economist() +
  coord_cartesian(xlim = c(-5.5, 5.5),ylim=c(-5.5,5.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +xlab("")+ylab("") +
  ggtitle("Positionnement Famille Javel") +
  scale_color_manual(values=c("black","red","blue","green"))



dx <- data.frame(x=c(0.7,-0.7,-0.7,5.2,-5.2,0  ,0),
                 y=c(4,2,1.2,0  ,0   ,5.2,-5.2),
                 label=c("Poudre Import",
                         "Poudre Concurent",
                         "Poudre Amin     ",
                         "Qualité Haut",
                         "Qualité bas",
                         "Prix haut",
                         "Prix bas"),
                 col=factor(c(1,2,3,0,0,0,0)))

ggplot(dx,aes(x=x,y=y,col=col)) +
  geom_label(aes(label=label)) +
  geom_segment(y=0,yend=0,x=-4.8,xend=4.8) +
  geom_segment(x=0,xend=0,y=-4.8,yend=4.8) +
  ggthemes::theme_economist() +
  coord_cartesian(xlim = c(-5.5, 5.5),ylim=c(-5.5,5.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +xlab("")+ylab("") +
  ggtitle("Positionnement Famille Poudre") +
  scale_color_manual(values=c("black","red","blue","green"))


dx <- data.frame(x=c(0.7,-0.7,0.8,5.2,-5.2,0  ,0),
                 y=c(4,2,1.2,0  ,0   ,5.2,-5.2),
                 label=c("Multi usage Import",
                         "Multi usage Concurent",
                         "Multi usage Amin",
                         "Qualité Haut",
                         "Qualité bas",
                         "Prix haut",
                         "Prix bas"),
                 col=factor(c(1,2,3,0,0,0,0)))

ggplot(dx,aes(x=x,y=y,col=col)) +
  geom_label(aes(label=label)) +
  geom_segment(y=0,yend=0,x=-4.8,xend=4.8) +
  geom_segment(x=0,xend=0,y=-4.8,yend=4.8) +
  ggthemes::theme_economist() +
  coord_cartesian(xlim = c(-5.5, 5.5),ylim=c(-5.5,5.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +xlab("")+ylab("") +
  ggtitle("Positionnement Famille Multi usage") +
  scale_color_manual(values=c("black","red","blue","green"))

dx <- data.frame(x=c(0.7,-0.7,0.8,5.2,-5.2,0  ,0),
                 y=c(4,2,1.2,0  ,0   ,5.2,-5.2),
                 label=c("Autres Import",
                         "Autres Concurent",
                         "Autres Amin",
                         "Qualité Haut",
                         "Qualité bas",
                         "Prix haut",
                         "Prix bas"),
                 col=factor(c(1,2,3,0,0,0,0)))

ggplot(dx,aes(x=x,y=y,col=col)) +
  geom_label(aes(label=label)) +
  geom_segment(y=0,yend=0,x=-4.8,xend=4.8) +
  geom_segment(x=0,xend=0,y=-4.8,yend=4.8) +
  ggthemes::theme_economist() +
  coord_cartesian(xlim = c(-5.5, 5.5),ylim=c(-5.5,5.5)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none") +xlab("")+ylab("") +
  ggtitle("Positionnement Autres produits de nettoyage") +
  scale_color_manual(values=c("black","red","blue","green"))
