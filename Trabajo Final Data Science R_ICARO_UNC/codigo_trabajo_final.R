##CODIGO DEL TRABAJO FINAL: ANALISIS DE UN DATASET "EDUCACION SEXUAL"
##ICARO-INTRODUCCION A DATA SCIENCE CON R, PROF. LAYLA SCHELI
###1. poniendo a punto nuestro entorno de trabajo
getwd()
setwd("C:/Users/Christian/Desktop/curso R icaro_UNC/trabajo final del curso")
###2. cargando librerias a ocupar
library(ggplot2)
library(readxl)
library(plotly)
###3. visualizacion primaria de datos
Educacion_Sexual <- read_excel("Educacion Sexual.xlsx")
edsex<-Educacion_Sexual
View(edsex)
summary(edsex)
summarise(edsex)
###4. visualizacion grafica introductoria de datos
hist(x=edsex$num_hijos)
hist(edsex$edad)
hist(edsex$anios_educ)
hist(edsex$bajo_socioecon)
anios.f <- as.factor(edsex$anios_educ)
numhijos.f <- as.factor(edsex$num_hijos)
con_socioecon <- as.factor(edsex$bajo_socioecon)
#bajo nivel socioeconomico, 1 pertenece, 0 no pertenece
pareja <- as.factor(edsex$en_pareja)

###5. analisis grafico de los datos
#Grafico 1: relacion de la edad con los años de educacion 
ggplot(edsex, aes(x= edad, fill = anios.f)) + 
  geom_bar(color='black',show.legend = T) +
  labs(x='Edad',
       title='Gráfico 1: Relacion de la edad con años de escolaridad',
       subtitle = 'ICARO - Introducción a Data Science con R. Prof: Layla Scheli',
       caption = 'Alumno: Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x= edad, fill = anios.f)) + 
           geom_bar(color='black',show.legend = T))

#Grafico 2: relacion de la edad con la cantidad de hijos
ggplot(edsex, aes(x= edad, fill = numhijos.f)) + 
  geom_bar(color='black',show.legend = T)+
  labs(x='Edad',
       title='Gráfico 2: Relación de la edad con el número de hijos',
       subtitle = 'ICARO - Introducción a Data Science con R. Prof: Layla Scheli',
       caption = 'Alumno: Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x= edad, fill = numhijos.f)) + 
  geom_bar(color='black',show.legend = T))

#Grafico 3: relacion hijos con escolaridad
ggplot(edsex, aes(x= num_hijos, fill = anios.f)) + 
  geom_bar(color='black',show.legend = T)+
  labs(x='Número de hijos',
       title='Gráfico 3: Relación del número de hijos con años de escolaridad',
       subtitle = 'ICARO - Introducción a Data Science con R. Prof: Layla Scheli',
       caption = 'Alumno: Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x= num_hijos, fill = anios.f)) + 
           geom_bar(color='black',show.legend = T))

#Grafico 4: relacion hijos con condicion socio economica
ggplot(edsex, aes(x= num_hijos, fill = bajose.f)) + 
  geom_bar(show.legend = T) +
  labs(x='Número de hijos',
       title='Gráfico 4: Relación del número de hijos y condición socioeconómica',
       subtitle = 'ICARO - Introducción a Data Science con R. Prof: Layla Scheli',
       caption = 'Alumno: Christian Farnast Contardo')

#Grafico 5: relacion de la edad con la condicion socio economica
  ggplot(edsex, aes(x= edad, fill = con_socioecon)) + 
    geom_bar(color='black',show.legend = T)+
    labs(x='Edad',
         title='Gráfico 5: Relación de la edad y condición socioeconómica',
         subtitle = 'ICARO - Introducción a Data Science con R. Prof: Layla Scheli',
         caption = 'Alumno: Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x= edad, fill = con_socioecon)) + 
           geom_bar(color='black',show.legend = T))

#Grafico 6: relacion de la escolaridad con la condicion socioeconomica
ggplot(edsex, aes(x= anios_educ, fill = con_socioecon)) + 
  geom_bar(color='black',show.legend = T)+
  labs(x='Años de escolaridad',
       title='Grafico 6: Relación de escolaridad con condición socioeconómica',
       subtitle = 'ICARO - Intro a Data Science con R. Prof: Layla Scheli',
       caption = 'Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x=anios_educ, fill = con_socioecon)) + 
           geom_bar(color='black',show.legend = T))

#Grafico 7: relacion de la edad con relacion de pareja
ggplot(edsex, aes(x= edad, fill = pareja)) + 
  geom_bar(color='black',show.legend = T)+
  labs(x='Edad',
       title='Gráfico 7: Relacion de la edad con relaciones de pareja',
       subtitle = 'ICARO - Intro a Data Science con R. Prof: Layla Scheli',
       caption = 'Christian Farnast Contardo')

ggplotly(ggplot(edsex, aes(x= edad, fill = pareja)) + 
           geom_bar(color='black',show.legend = T))


###ALUMNO DEL CURSO: CHRISTIAN FARNAST CONTARDO###