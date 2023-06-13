# General analysis

#General table

cancion_media<-aggregate(data$Puntaje_cancion,list(data$Pelicula),FUN=mean)
colnames(cancion_media)<-c("Pelicula","Puntaje canción")
momento_media<-aggregate(data$Puntaje_momento,list(data$Pelicula),FUN=mean)
colnames(momento_media)<-c("Pelicula","Puntaje momento")
letra_media<-aggregate(data$Puntaje_letra,list(data$Pelicula),FUN=mean)
colnames(letra_media)<-c("Pelicula","Puntaje letra")

tabla<-cbind(cancion_media,momento_media$`Puntaje momento`,letra_media$`Puntaje letra`)
colnames(tabla)<-c("Pelicula","Puntaje_cancion","Puntaje_momento","Puntaje_letra")

intento<-apply(tabla[,2:4],1,FUN=mean)

tabla$puntaje_total<-intento
tabla$Puntaje_cancion<-round(tabla$Puntaje_cancion,1)
tabla$Puntaje_momento<-round(tabla$Puntaje_momento,1)
tabla$Puntaje_letra<-round(tabla$Puntaje_letra,1)
tabla$puntaje_total<-round(tabla$puntaje_total,1)

url<-c("https://upload.wikimedia.org/wikipedia/commons/8/85/Logo_HSM_original.jpg","https://upload.wikimedia.org/wikipedia/commons/f/ff/High_School_Musical_2_Text_Logo.svg","https://upload.wikimedia.org/wikipedia/commons/8/8e/High_School_Musical_3_Logo.svg")

tabla$url<-url

tabla<-tabla %>% #reordering columns
  relocate(url,.before = Pelicula)

tabla_gt<-tabla[,-2] %>%
  gt() %>% #table
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(url)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = c(60,50,60) #image size
      )
    }) %>%
  cols_label(url="Película",Puntaje_cancion="Puntaje canción",Puntaje_momento="Puntaje momento",Puntaje_letra="Puntaje letra",puntaje_total="Puntaje total") 
tabla_gt

tabla_gt<-tabla_gt %>%
  opt_all_caps()%>%
  opt_table_font(font = list(google_font("Lato"),default_fonts()))

tabla_gt<-tabla_gt %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "all", weight = px(2)),
      #Make text bold
      cell_text(weight = "bold")
    ))
tabla_gt<-tabla_gt %>%
  tab_style(
    style = cell_borders(
      sides = c("left","right"),
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )
tabla_gt <-tabla_gt%>%
  tab_style(
    style = cell_borders(
      sides=c("bottom","top"),color="transparent"),
    locations = cells_body()
  )
tabla_gt <- tabla_gt %>%
  tab_options(table_body.border.bottom.color = "black") %>%
  tab_options(column_labels.font.size = px(15)) %>%
  cols_align("center")
tabla_gt
