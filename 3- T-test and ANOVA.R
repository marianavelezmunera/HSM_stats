# T-test and ANOVAs
mariana<-c(rep("Mariana Lozada",30),rep("Mariana Vélez",30))
data$Mariana<-mariana


puntaje_total<-apply(data[,4:6],1,FUN=mean)
data$puntaje_total<-puntaje_total
cancion<-ggplot(data=data,aes(x=Pelicula,y=Puntaje_cancion,fill=Mariana))+
  geom_boxplot()+
  theme_bw()+
  xlab("Película")+ylab("Puntaje canción")+
  scale_fill_manual(values=c("red","blue"))+
  labs(fill=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))

momento<-ggplot(data=data,aes(x=Pelicula,y=Puntaje_momento,fill=Mariana))+
  geom_boxplot()+
  theme_bw()+
  xlab("Película")+ylab("Puntaje momento")+
  scale_fill_manual(values=c("red","blue"))+
  labs(fill=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))

letra<-ggplot(data=data,aes(x=Pelicula,y=Puntaje_letra,fill=Mariana))+
  geom_boxplot()+
  theme_bw()+
  xlab("Película")+ylab("Puntaje letra")+
  scale_fill_manual(values=c("red","blue"))+
  labs(fill=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))

total<-ggplot(data=data,aes(x=Pelicula,y=puntaje_total,fill=Mariana))+
  geom_boxplot()+
  theme_bw()+
  xlab("Película")+ylab("Puntaje total")+
  scale_fill_manual(values=c("red","blue"))+
  labs(fill=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))
completo<-cancion+momento+letra+total+plot_layout(guides = "collect")
completo

# T test
var.test(HSM_ML$Puntaje_cancion,HSM_MV$Puntaje_cancion)
qqPlot(HSM_ML$Puntaje_cancion)
qqPlot(HSM_MV$Puntaje_cancion)
t.test(HSM_ML$Puntaje_cancion,HSM_MV$Puntaje_cancion,var.equal = TRUE) 

#Tenemos las mismas opiniones


# ANOVA por película

anova<-aov(Puntaje_cancion~Pelicula,data=data)
summary(anova)

anova<-aov(Puntaje_cancion~Cantantes,data=data)
summary(anova)


ggplot(data=data,aes(x=Cantantes,y=puntaje_total,fill=Mariana))+
  geom_boxplot()+
  theme_bw()+
  xlab("Cantantes")+ylab("Puntaje letra")+
  scale_fill_manual(values=c("red","blue"))+
  labs(fill=NULL)+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))+
  theme(legend.text = element_text(family = "Lato"))+
  geom_label(label="N=14",y=7.8,x=1,label.size = 0.5,fill="white")+
  geom_label(label="N=6",y=6.5,x=2,label.size = 0.5,fill="white")+
  geom_label(label="N=2",y=9.8,x=3,label.size = 0.5,fill="white")+
  geom_label(label="N=2",y=9.8,x=4,label.size = 0.5,fill="white")+
  geom_label(label="N=8",y=5,x=5,label.size = 0.5,fill="white")+
  geom_label(label="N=2",y=6.4,x=6,label.size = 0.5,fill="white")+
  geom_label(label="N=8",y=5.9,x=7,label.size = 0.5,fill="white")+
  geom_label(label="N=2",y=5.5,x=8,label.size = 0.5,fill="white")+
  geom_label(label="N=16",y=8,x=9,label.size = 0.5,fill="white")
