# Correlation
cor(data$Puntaje_momento,data$Puntaje_cancion)
mod<-lm(Puntaje_cancion~Puntaje_momento,data=data)
summary(mod)

ggplot(data = data,aes(Puntaje_momento,Puntaje_cancion,colour=Mariana))+
  geom_point()+
  theme_bw()+
  xlab("Puntaje momento")+ylab("Puntaje canción")+
  scale_colour_manual(values=c("red","blue"))+
  labs(colour=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))+
  theme(legend.text = element_text(family = "Lato"))


