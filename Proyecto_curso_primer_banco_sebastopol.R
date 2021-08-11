## PROYECTO CURSO INTRODUCCION A R PARA INTELIGENCIA Y ANALISIS DE NEGOCIOS ##
## INSTRUCTOR: CARLOS MARTINEZ, PHD ##
## ENLACE: https://www.udemy.com/course/introduccion-a-r-para-inteligencia-y-analitica-de-negocios/learn/lecture/22597750?start=15#overview
## ALUMNO: Q.F. CHRISTIAN FARNAST CONTARDO ##


# CARGANDO LIBRERIAS
library(ggplot2)
library(funModeling)
library(tidyverse)
library(corrr)

# CARGANDO DATASET
banco <- read.csv("primerbancodesebastopol.csv", sep = ';')
View(banco)

# ANALISIS DEL DATASET
df_status(banco)
#            variable  q_zeros p_zeros q_na  p_na q_inf p_inf     type unique
# 1                ID       0    0.00    0  0.00     0     0   integer  31634
# 2          Ganancia     222    0.70    0  0.00     0     0   integer   1634
# 3            Online   27780   87.82    0  0.00     0     0   integer      2
# 4     CategoriaEdad       0    0.00 8289 26.20     0     0   integer      7
# 5 CategoriaIngresos       0    0.00 8261 26.11     0     0   integer      9
# 6       Permanencia       0    0.00    0  0.00     0     0   numeric    491
# 7         Municipio       0    0.00    0  0.00     0     0 character      3

summary(banco)
str(banco)

# las variables Online, Categoriaedad, Categoriaingresos y Municipio son de 
# tipo numerico y caracter, por lo que para visualizaciones serán convertidas
# a factor
banco$Online <- as.factor(banco$Online)
banco$CategoriaEdad <- as.factor(banco$CategoriaEdad)
banco$CategoriaIngresos <- as.factor(banco$CategoriaIngresos)
banco$Municipio <- as.factor(banco$Municipio)

df_status(banco)

# GRAFICANDO VARIABLES CATEGORICAS
freq(banco)

# GRAFICANDO VARIABLES NUMERICAS
plot_num(banco)

###############################################################################

## PREGUNTAS GUIA ##

# 1.	Estime la rentabilidad promedio para los clientes con y 
#     sin banca en línea. ¿Son diferentes? 
#     ¿Es esta diferencia estadísticamente significativa?

# SEPARAR LOS GRUPOS

banco_sin_linea <- banco %>% 
  filter(Online == 0) %>% 
  mutate(rent_promedio = sum(Ganancia)/27780)

# RENTABILIDAD PROMEDIO CLIENTES SIN BANCA ONLINE : 110.7862 UNIDADES

banco_linea <- banco %>% 
  filter(Online == 1) %>% 
  mutate(rent_promedio = sum(Ganancia)/3854)

# RENTABILIDAD PROMEDIO CLIENTES CON BANCA ONLINE : 116.6668 UNIDADES


# 2.	Corra una regresión con Ganancia como variable dependiente y Online y CategoríaEdad 
#     como variables independientes. 
#     ¿Es el coeficiente Online estadísticamente significativo? 
#     ¿Por qué R no utilizó todos los datos disponibles? 
#     ¿Hay una diferencia estadísticamente significativa entre las ganancias 
#     de los clientes con y sin datos faltantes en CategoríaEdad? 
#     Si la hay, ¿puede utilizar sus resultados para sacar conclusiones?

# REGRESION PARA DATOS FALTANTES
regresion <- lm(Ganancia ~ CategoriaEdad + Online , data = banco )
summary(regresion)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -1.802     10.485  -0.172    0.864    
# CategoriaEdad2   54.425     11.401   4.774 1.82e-06 ***
# CategoriaEdad3  112.699     11.098  10.155  < 2e-16 ***
# CategoriaEdad4  133.820     11.103  12.053  < 2e-16 ***
# CategoriaEdad5  144.986     11.531  12.574  < 2e-16 ***
# CategoriaEdad6  160.844     11.965  13.443  < 2e-16 ***
# CategoriaEdad7  193.072     11.757  16.422  < 2e-16 ***
# Online1          27.246      5.519   4.937 8.01e-07 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 277.9 on 23337 degrees of freedom
# (8289 observations deleted due to missingness) = CategoriaEdad
# Multiple R-squared:  0.02494,	Adjusted R-squared:  0.02464 
# F-statistic: 85.26 on 7 and 23337 DF,  p-value: < 2.2e-16


# CREAR UNA REGRESION SIN DATOS FALTANTES => CREAR VARIABLE CON EDAD Y SIN EDAD

banco$conedad[is.na(banco$CategoriaEdad) == 1] = 0
banco$conedad[is.na(banco$CategoriaEdad) == 0] = 1

mean(banco$Ganancia[banco$conedad == 0]) #72.9648
mean(banco$Ganancia[banco$conedad == 1]) #125.187

View(banco)

regresion2 <- lm(Ganancia ~ conedad + Online, data = banco)
summary(regresion2)
# Coefficients:
#             Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept)   72.962      2.986   24.43   <2e-16 ***
# conedad       52.224      3.476   15.02   <2e-16 ***

# 3.	Cree nuevas categorías para los datos faltantes 
# tanto en CategoríaEdad como CategoríaIngresos y 
# corra una regresión incluyendo las variables de control. 
# ¿Cómo interpreta cada uno de los coeficientes? 
# Luego de controlar por todas las variables demográficas disponibles, 
# ¿es el coeficiente de Online estadísticamente significativo? 
# ¿Qué conclusión le dará a su jefe en torno a la rentabilidad 
# de los clientes de banca online? 

# ASIGNAR VALOR O A LAS VARIABLES FALTANTES DE CATEGORIA EDAD Y CATEGORIA INGRESOS
banco$CategoriaEdad <- as.numeric(banco$CategoriaEdad)
banco$CategoriaEdad[banco$CategoriaEdad %in% NA] = 0
banco$CategoriaEdad <- as.factor(banco$CategoriaEdad)
freq(banco$CategoriaEdad)

banco$CategoriaIngresos <- as.numeric(banco$CategoriaIngresos)
banco$CategoriaIngresos[banco$CategoriaIngresos %in% NA] = 0
banco$CategoriaIngresos <- as.factor(banco$CategoriaIngresos)
freq(banco$CategoriaIngresos)

regresion3 <- lm(Ganancia ~ Online + CategoriaEdad, data = banco)
summary(regresion3)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      70.922      2.997  23.665  < 2e-16 ***
# Online1          19.686      4.683   4.204 2.63e-05 ***
# CategoriaEdad1  -71.266     10.537  -6.764 1.37e-11 ***
# CategoriaEdad2  -16.672      5.374  -3.102  0.00192 ** 
# CategoriaEdad3   41.144      4.717   8.722  < 2e-16 ***
# CategoriaEdad4   62.107      4.717  13.166  < 2e-16 ***
# CategoriaEdad5   72.976      5.581  13.075  < 2e-16 ***
# CategoriaEdad6   88.499      6.361  13.912  < 2e-16 ***
# CategoriaEdad7  120.623      5.980  20.170  < 2e-16 ***

# NO TENIENDO NINGUN VALOR FALTANTE, CONSIDEREMOS LA GANANCIA VERSUS TODAS LAS VARIABLES

regresion4 <- lm(Ganancia ~ Online +CategoriaEdad+CategoriaIngresos, data = banco)
summary(regresion4)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         70.6202     3.0133  23.436  < 2e-16 ***
# Online1             11.4747     4.6411   2.472 0.013426 *  
# CategoriaEdad1     -80.5875    12.3961  -6.501 8.10e-11 ***
# CategoriaEdad2     -45.0803     9.1175  -4.944 7.68e-07 ***
# CategoriaEdad3       1.4297     8.8883   0.161 0.872211    
# CategoriaEdad4      16.9535     8.9912   1.886 0.059362 .  
# CategoriaEdad5      31.9343     9.4579   3.376 0.000735 ***
# CategoriaEdad6      61.3649     9.8879   6.206 5.50e-10 ***
# CategoriaEdad7     100.7095     9.6144  10.475  < 2e-16 ***
# CategoriaIngresos1 -17.9038     9.7742  -1.832 0.066999 .  
# CategoriaIngresos2 -11.8848    12.3314  -0.964 0.335161    
# CategoriaIngresos3  -1.1760     9.5720  -0.123 0.902216    
# CategoriaIngresos4  -0.5165     9.8534  -0.052 0.958193    
# CategoriaIngresos5   5.6373     9.8389   0.573 0.566674    
# CategoriaIngresos6  31.9140     8.9316   3.573 0.000353 ***
# CategoriaIngresos7  53.7214     9.4447   5.688 1.30e-08 ***
# CategoriaIngresos8  71.1105    10.3486   6.872 6.47e-12 ***
# CategoriaIngresos9 144.6013     9.6501  14.984  < 2e-16 ***

regresion5 <- lm(formula = Ganancia ~ Permanencia +Online + CategoriaEdad + CategoriaIngresos, 
                 data = banco)
summary(regresion5)

# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         31.9881     3.3628   9.512  < 2e-16 ***
# Permanencia          4.7751     0.1916  24.921  < 2e-16 ***
# Online1             14.2257     4.5976   3.094 0.001975 ** 
# CategoriaEdad1     -62.7963    12.2971  -5.107 3.30e-07 ***
# CategoriaEdad2     -31.8886     9.0449  -3.526 0.000423 ***
# CategoriaEdad3       4.7012     8.8034   0.534 0.593331    
# CategoriaEdad4       6.9428     8.9134   0.779 0.436033    
# CategoriaEdad5      10.4130     9.4062   1.107 0.268289    
# CategoriaEdad6      29.0209     9.8779   2.938 0.003306 ** 
# CategoriaEdad7      63.7838     9.6361   6.619 3.67e-11 ***
# CategoriaIngresos1 -15.6697     9.6801  -1.619 0.105511    
# CategoriaIngresos2 -12.5167    12.2122  -1.025 0.305403    
# CategoriaIngresos3  -2.4826     9.4796  -0.262 0.793410    
# CategoriaIngresos4  -5.6075     9.7603  -0.575 0.565620
# CategoriaIngresos5   1.0063     9.7456   0.103 0.917764    
# CategoriaIngresos6  27.0950     8.8474   3.062 0.002197 ** 
# CategoriaIngresos7  47.6641     9.3566   5.094 3.52e-07 ***
# CategoriaIngresos8  65.5752    10.2510   6.397 1.61e-10 ***
# CategoriaIngresos9 135.0702     9.5645  14.122  < 2e-16 ***

regresion6 <- lm(formula = Ganancia ~ Municipio + Permanencia + Online + CategoriaEdad + 
                   CategoriaIngresos, data = banco)
summary(regresion6)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           15.8836     5.6217   2.825 0.004725 ** 
# MunicipioMacumba       8.6630     6.2490   1.386 0.165665    
# MunicipioSalsipuedes  19.7067     5.1010   3.863 0.000112 ***
# Permanencia            4.7917     0.1916  25.009  < 2e-16 ***
# Online1               13.4983     4.5997   2.935 0.003342 ** 
# CategoriaEdad1       -63.5675    12.2956  -5.170 2.36e-07 ***
# CategoriaEdad2       -33.0700     9.0467  -3.655 0.000257 ***
# CategoriaEdad3         4.0186     8.8025   0.457 0.648010    
# CategoriaEdad4         6.6551     8.9113   0.747 0.455185    
# CategoriaEdad5         9.9292     9.4047   1.056 0.291081    
# CategoriaEdad6        28.7596     9.8759   2.912 0.003593 ** 
# CategoriaEdad7        63.4023     9.6342   6.581 4.75e-11 ***
# CategoriaIngresos1   -12.3684     9.7080  -1.274 0.202657
# CategoriaIngresos2   -11.0678    12.2163  -0.906 0.364952    
# CategoriaIngresos3    -2.4894     9.4772  -0.263 0.792803    
# CategoriaIngresos4    -3.1228     9.7751  -0.319 0.749376    
# CategoriaIngresos5     2.6210     9.7503   0.269 0.788079    
# CategoriaIngresos6    26.8388     8.8454   3.034 0.002414 ** 
# CategoriaIngresos7    46.8962     9.3559   5.012 5.40e-07 ***
# CategoriaIngresos8    64.2256    10.2531   6.264 3.80e-10 ***
# CategoriaIngresos9   132.7718     9.5773  13.863  < 2e-16 ***

# RESPUESTAS

# PREGUNTA 1: NO HAY UNA DIFERENCIA SIGNIFICATICA EN LAS RENTABILIDADES PROMEDIO
# DE QUIENES OCUPAN LA BANCA EN LINEA VERSUS QUIENES NO LA OCUPAN.
# LA DIFERENCIA ES DE : 5.8806 UNIDADES MONETARIAS

# PREGUNTA 2: AL HACER LA REGRESION DE GANANCIA V/S ONLINE Y CATEGORIAEDAD
# LA VARIABLE ONLINE MUESTRA UNA GANANCIA DE QUIENES LA OCUPAN DE 27.246
# UNIDADES MONETARIAS VERSUS QUIENES NO LA OCUPAN PARA TODAS LAS CATEGORIAS DE EDAD
# POR LO QUE ES ESTADISTICAMENTE SIGNIFICATIVO. PESE A LOS DATOS FALTANTES (8239)
# Y QUE EL SOFTWARE PERMITE OCUPAR DATOS FALTANTES PARA UNA REGRESION
# NO ES POSIBLE SACAR CONCLUSIONES PUES LOS RESULTADOS TIENEN UN EVIDENTE SESGO.
# LAS GANANCIAS SON SIGNIFICATIVAS ENTRE EL ARREGLO DE LOS DATOS FALTANTES VERSUS
# SI NO SE HUBIERA HECHO ARREGLO, ES DECIR DE 72.96248 UNIDADES DE QUIENES
# NO OCUPAN EL CANAL ONLINE, ESTE SUBE A 125.187 UNIDADES MONETARIAS PARA LOS QUE
# OCUPAN DICHA PLATAFORMA.

# PREGUNTA 3: AL ARREGLAR LOS DATOS FALTANTES DE LAS CATEGORIAS CATEGORIAEDAD Y
# CATEGORIAINGRESOS Y HACIENDO LA REGRESION DE GANANCIA VERSUS TODAS LAS VARIABLES,
# EL COEFICIENTE ONLINE MANTIENE SU ESTADISTICA SIGNIFICATIVA DANDO UN RESULTADO DE
# 13.4983 UNIDADES MONETARIAS. POR LO TANTO, SE SUGIERE DAR ENFASIS EN PROMOVER
# EL USO DE ESTA PLATAFORMA PARA GENERAR VALOR ADICIONAL A LA EMPRESA.