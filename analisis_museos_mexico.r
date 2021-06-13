##Este breve ejercicio de an�lisis de estad�stica descriptiva y generaci�n de gr�ficas 
# fue hecho con el objetivo de completar la "Actividad 2" del Curso de Selecci�n del DGC

#Los Datos fueron obtenidos del SIC, 2021 ( https://sic.gob.mx/datos.php?table=museo&estado_id=9 )

#Y del INEGI, 2019 ( https://www.inegi.org.mx/contenidos/programas/museos/doc/museos_2019_nota_tecnica.pdf )

## Grafica tipo de  museos en México 

data <- read.csv("0_museo_directorio.csv")


##Nombre del directorio 
X0_museo_directorio

## Tipo de Museos más común en México 

names(sort(table(X0_museo_directorio$museo_tematica_n1), decreasing=TRUE)[1])

# Resultado: Historia 

## Gráfica de Barras, tipo de Museos 
library(RColorBrewer)## Para quitarle el gris feo a la gráfica.
coul <- brewer.pal(5, "Set2")
counts <- table(X0_museo_directorio$museo_tematica_n1)
barplot(counts, main = "Tipos de Museos en México",  horiz = TRUE, names = c("Antropología", "Arqueología", "Arte", "Arte Alternativo", "Ciencia y Tecnología", "Especializado", "Historia", "TND"), 
        las = 1, col= "darkslategray") + theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))


#Crear una imagen de la tabla
library(gridExtra)
grid.table(count_types)

#Pie chart segundo intento, Este sí Funciono

lbls <- c("Antropología", "Arqueología", "Arte", "Arte Alternativo", "Ciencia y tecnología", "Especializado", "Historia", "TND")
slices <- c(85, 244, 279, 1, 150, 64, 397, 36)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=c("lightseagreen","darkslategray4","darkslategray3", "darkslategray2", "turquoise4", "turquoise","darkslategrey","turquoise1"), main="Tipos de Museos en México") #Buscar como hacerlo 3d



## Museos por Estado y Media 

library(readr)
x2 <- read_csv("C:/Users/iamso/OneDrive/Escritorio/DGC/Curso de Evalución/2 Actividad/2_estados_museos_población.csv")
View(x2)

#Total de Museos en la República Mexicana 
sum(x2$num_museos)
#1404
media <- mean(x2$num_museos)
#43.874

#Gráfica de Barras Concentración de Museos por Estado # Si funcionó
data <- x2[ ,1:2] #Subset todas las filar y las columnas de 1 a 2.
Estados <- c(x2$Estado)
Museos <- c(x2$num_museos)
graf <- ggplot(data, aes(x = reorder(Estados, - Museos), y = Museos)) #Ordenado mayor a menor 
graf + geom_bar(stat = "identity", fill="darkslategray") +
  geom_text(aes(label=Museos), vjust=-0.3, size=3.5)+
  geom_hline(aes(yintercept = mean(Museos), na.rm = "Media"))+ #media, no pude cambiar el nombre de 
  theme_minimal()+ ggtitle("Total de Museos por Estado")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

##10 Municipios con más museos en México 
library(readr)
municipios_museos_poblacion <- read_csv("C:/Users/iamso/OneDrive/Escritorio/DGC/Curso de Evalución/2 Actividad/municipios_museos_poblacion.csv")

#Total de Museos en estos municipios 
x <- sum(municipios_museos_poblacion$cantidad_museos)
x
##284 
y <- sum(x2$num_museos)
y 
z <- x/y * 100 #Porcentaje 
z #El 20% de los Museos del país están concentrados en estos municipios 

# Grafica Municipios 
#library(ggplot2)El 

Municipios <- c(municipios_museos_poblacion$nom_mun)
Museos <- c(municipios_museos_poblacion$cantidad_museos)
grafica <- ggplot(data = municipios_museos_poblacion, aes(Municipios,Museos))
grafica + geom_bar(stat = "identity", fill="darkslategray") +
  geom_text(aes(label = Museos), vjust=-0.3, size=3.5)+
  theme_minimal()+ ggtitle("10 Municipios con más Museos en México")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

#Grafica Museos por 


## Gráfica Municipios con más Museos de México y la relación con los habitantes que tiene 

#Datos 
library(readr)
museosxhab <- read_csv("C:/Users/iamso/OneDrive/Escritorio/DGC/Curso de Evalución/2 Actividad/museosxhab.csv")
totalmuseos <- sum(museosxhab$cantidad_museos)
totalmuseos
totalhab <- sum(museosxhab$pobtot)
totalhab
museossobrehab <- totalmuseos / totalhab 
museossobrehab
porcentaje_museosxhab <- totalmuseos / totalhab *100  
porcentaje_museosxhab
habxmuseos <- totalhab / totalmuseos
habxmuseos
perhabxmuseos <- habxmuseos * 100 #esta no nos sirve de nada 
perhabxmuseos

#Gráfica: 10 Municipios con mayor Porcentaje de Museos con Relación a sus habitantes 
Porcentaje <- c(museosxhab$porcentaje_museosxhab)
Municipios <- c(museosxhab$nom_mun)

## Bueno pero sin datos ordenados 
grfmuseosenthab <- ggplot(data = museosxhab, aes(Minicipios, Porcentaje))
grfmuseosenthab + geom_bar(stat = "identity", fill="steelblue") +
  geom_text(aes(label = Porcetaje ), vjust=-0.3, size=3.5)+
  theme_minimal()+ ggtitle("10 Municipios con mayor Porcentaje de Museos con Relación a sus habitantes")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

# "10 Municipios con mayor Porcentaje de Museos con Relación a sus habitantes" Mayor a menor porcentaje 
grfmuseosenthab <- ggplot(data = museosxhab, aes(x = reorder(Municipios, - Porcentaje), y = Porcentaje))
grfmuseosenthab + geom_bar(stat = "identity", fill="darkslategray") +
  geom_text(aes(label = Porcentaje ), vjust=-0.3, size=3.5)+
  theme_minimal()+ ggtitle("10 Municipios con mayor Porcentaje de Museos con Relación a sus habitantes")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

# Gráfico de pastel costos de museos 

costos <- c(670.41, 259.74, 239.85) ## 
categorias <- c("Siempre es gratis", "Sólo algunos días es gratis", "Siempre tiene costo")

pctcostos <- round(costos/sum(costos)*100)
categorias <- paste(categorias, pctcostos) # add percents to labels
categorias <- paste(categorias,"%",sep="") # ad % to labels
pie(costos,labels = categorias, col=c("darkslategrey","darkslategray4","darkslategray3"), main="Museos según costos de entrada, 2019")

# Gráfica Principales Medios para enterarse de la existencia del museo 

medios <- c("Amigos o conocidos", "Maestro/a, compañero/a, libro de texto", "Internet", "Conoce desde siempre el museo", "Por casualidad", "Redes Sociales", "Oficina turística o viaje turístico", "Folleto o anuncio", "Televisión", "Periodico, revista o libro", "Radio", "Otro", "No especificado")
porcentaje_medios <- c(26.3, 17.5, 12.2, 11.9, 11.3, 7.4, 5.5, 3.9, 1.7, 1.0, 0.9, 0.2, 0.2)
total_medios <- porcentaje_medios * 186125/ 100 

library(ggplot2)

grfmedios <- ggplot(data = NULL, aes(x = reorder(medios, + porcentaje_medios), y = porcentaje_medios))
grfmedios + geom_bar(stat = "identity", fill="darkslategray4") + coord_flip() +
  geom_text(aes(label = porcentaje_medios), vjust=-0.5, size=3.5)+
  theme_minimal()+ ggtitle("Principales medios para enterarse de la existencia del museo, 2019")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

# Gráfico de pastes de frecuencia de visitas al museo. Datos del INEGI 2019 

num_visitas_prc <- c(77.9, 13.7, 8.4)
frecuencia <- c("Primera visita", "2 o 3 visitas", "4 o más visitas")
num_visitas <- num_visitas_prc * 186125/ 100 


frecuencia <- paste(frecuencia, num_visitas_prc) # add percents to labels
frecuencia <- paste(frecuencia,"%",sep="") # ad % to labels
pie(num_visitas,labels = frecuencia, col=c("darkslategrey","darkslategray4","darkslategray3"), main="Frecuencia de visitas al museo")

# Gr�fico de distribuci�n por edad y sexo de los visitantes. Datos del INEGI 2019 
load("C:/Users/iamso/OneDrive/Escritorio/DGC/Curso de Evaluci�n/2 Actividad/Museos_A2/.RData")
library(readr)
library(reshape2)
library(plyr)
library(ggplot2)

mujeres_visitas <- visitasxedadxsexo$Mujeres
hombres_visitas <- visitasxedadxsexo$Hombres
Edades <- visitasxedadxsexo$Edad
variables <- visitasxedadxsexo[,c("Edades", "Genero", "Visitas")]
visitasxedadxsexo

pyramid <- ggplot(visitasxedadxsexo, aes(x = Edades, y = mujeres_visitas, hombres_visitas)) +  
  geom_bar(data = subset(visitasxedadxsexo, Genero == "Mujeres"), stat= "identity") + 
  geom_bar(data = subset(visitasxedadxsexo, Genero == "Hombres"), stat = "identity")+
  scale_y_continuous(labels = Edad) + coord_flip()
pyramid
    
pyramid <- ggplot(visitasxedadxsexo, aes(x = Edades, y = mujeres_visitas, hombres_visitas)) +
  geom_text(aes(label = Edades), vjust=-0.5, size=3.5)+
  theme_minimal()+ ggtitle("Principales medios para enterarse de la existencia del museo, 2019")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))

# Intento 3 
install.packages("dplyr", dependencies = TRUE)
library(ggplot2)
library(readr)
visitas_edad_genero <- read_csv("C:/Users/iamso/OneDrive/Escritorio/DGC/Curso de Evaluci�n/2 Actividad/visitas_edad_genero.csv", 
                                  +     col_types = cols(X4 = col_skip(), X5 = col_skip(), 
                                                         +         X6 = col_skip()))

Visitas <- visitas_edad_genero$Visitas
Edades <- visitas_edad_genero$Edad

visitas_edad_genero
pyramid <- ggplot(visitas_edad_genero, aes(x = Edades, y = Visitas, fill = Genero)) +
  geom_col(data = subset(visitas_edad_genero, Genero == "Mujeres") %>%
            mutate(Visitas = - Visitas),
          width = 0.5, fill = "darkslategrey") +
  geom_col(data = subset(visitas_edad_genero, Genero == "Hombres"),
           width = 0.5, fill = "darkslategrey4") + 
  coord_flip() +
  theme_minimal()+ theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "Distrobuci�n por Edad y Sexo de los visitantes",
       x = "",
       y = "Mujeres                        Hombres",
       caption = "Gr�fica 12- Elaboraci�n propia con datos del INEGI 2019.") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

ggsave(filename= "ppMorelos.png", dpi = 300)

## No he podido 

## Gr�fica de escolaridad de los y la visitantes

Escolaridad <- c("Ninguno","Preescolar", "Primaria", "Secundaria", "Media Superior", "Superior", "No especificado" )
Porcentaje <-c(0.3, 0.1, 3.1, 10.9, 27.4, 57.5, 0.7)

frecuencia <- paste(Escolaridad, Porcentaje) # add percents to labels
frecuencia <- paste(Escolaridad,"%",sep="") # ad % to labels
pie(Porcentaje,labels = Escolaridad, col=c("darkslategrey","darkslategray4","darkslategray3", "darkslategray2", "turquoise4", "turquoise","turquoise1"), main="Frecuencia de visitas al museo")

# Segundo intento para mejor visualizaci�n 
library(ggplot2)
grfescolaridad <- ggplot(data = NULL, aes(x = reorder(Escolaridad, + Porcentaje), y = Porcentaje))
grfescolaridad + geom_bar(stat = "identity", fill="darkslategray4") + coord_flip() +
  geom_text(aes(label = Porcentaje), vjust=-0.5, size=3.5)+
  theme_minimal()+ ggtitle("Nivel de escolaridad de las y los visitantes")+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1))
ggsave(filename= "verde_escolaridad.png", dpi = 300)

# Lugar donde viven los visitantes respecto al museo. Dato del INEGI 2019 
lugar <- c("Misma entidad federativa", "Otro pa�s", "Otra entidad federativa")
porcentaje <- c(59.1, 6.1, 34.8)

frecuencia <- paste(porcentaje, lugar)
frecuencia <- paste(porcentaje, "%", sep = " ") # ad % to labels
pie(lugar,labels = porcentaje, col=c("darkslategrey","darkslategray4","darkslategray3"), main="Lugar donde viven las y los visitantes")

lugar_visitante <- paste(Porcentaje, l) # add percents to labels
lugar_visitante <- paste(Lugar,"%",sep ="") # ad % to labels
pie(Porcentaje,labels = Lugar, col=c("darkslategrey","turquoise4", "darkslategray4"), main="Lugar donde viven las y los visitantes")

lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=c("lightseagreen","darkslategray4","darkslategray3", "darkslategray2", "turquoise4", "turquoise","darkslategrey","turquoise1"), main="Tipos de Museos en México") #Buscar como hacerlo 3d

graplugar <- paste(porcentaje, lugar) # add percents to labels
graplugar <- paste(lugar,"%",sep="") # ad % to labels
pie(porcentaje,labels = lugar, col=c("darkslategrey","darkslategray4","darkslategray3"), main="Museos según costos de entrada, 2019")
