skip_mariana<-c("ML","ML","MV","MV")
skip_sino<-c("SI","NO","SI","NO")
skip<-frec<-c(11,19,9,21)
skips<-data.frame(skip_mariana,skip_sino,skip)
colnames(skips)<-c("Mariana","Skip","Frec")

ggplot(data=skips,aes(x=Mariana,y=Frec,fill=Skip))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.5)+
  theme_bw()+
  ylab("Número de canciones")+
  labs(x=NULL)+
  scale_x_discrete(labels = c("Mariana Lozada","Mariana Vélez"))+
  scale_fill_discrete(labels=c("NO","SÍ"),name="Skip")+
  theme(axis.text.y = element_text(family = "Lato",size=12))+
  theme(axis.title.y = element_text(family = "Lato",size=16,face="bold"))+
  theme(axis.text.x = element_text(family = "Lato",size=12))+
  theme(axis.title.x = element_text(family = "Lato",size=16,face="bold"))+
  theme(legend.text = element_text(family = "Lato"))

chisq.test(table(data$Skip,data$Mariana))  #Fallamos en rechazar la nula, no hay asociación entre los skips y la persona que evaluó

