#------------------------------ ###### ---------------------------------|
#    -- ================================================================|
#    -- Author:		    <Luis Rodriguez>           									      |
#    -- Organization: <ITAM>                                            |
#    -- Create date:  <17/06/2020>										                  |
#    -- Last modification date:  <17/06/2020>							              |
#    -- Description:	<Proyecto Modulo Multivariable>                   |
#    -- Data Source:  <https://www.kaggle.com/rajyellow46/wine-quality> |
#    -- ================================================================|
#------------------------------ ###### ---------------------------------|

########                 Librerias, Descripción ########  

library(sf)              # Simple Features for R.
library(dplyr)           # Grammar of Data Manipulation.
library(tidyr)           # Easily Tidy Data with 'spread()' and 'gather()'. 
library(sp)              # Classes and Methods for Spatial Data.
library(raster)          # Geographic Data Analysis and Modeling.
library(rgeos)           # Interface to Geometry Engine.
library(rgbif)           # Interface to the Global 'Biodiversity' Information Facility API.
library(viridis)         # Default Color Maps from 'matplotlib'.
library(gridExtra)       # Miscellaneous Functions for "Grid" Graphics.
library(rasterVis)       # Visualization Methods for Raster Data.
library(caTools)         # Tools: moving window statistics, GIF, Base64, ROC. 
library(corrplot)        # Visualization of a Correlation Matrix.
library(dygraphs)        # Interactive Time Series.
library(data.table)	     # Data information handle.
library(directlabels)    # Direct Labels for Multicolor Plots.
library(forecast)        # Forecasting Functions for Time Series and Linear Models.
library(factoextra)      # Extract and Visualize the Results of Multivariate Data Analyses.
library(GGally)          # Extension to ggplot.
library(ggcorrplot)      # Visualization of Correlation Matrix using ggplot.
library(ggrepel)	       # Automatically Position Non-Overlapping Text Labels with 'ggplot2.
library(gmapsdistance)   # Distance and Travel Time Between Two Points from Google Maps.
library(geosphere)       # Compute various aspects of distance.
library(ggplot2)         # Create Elegant Data Visualisations Using the Grammar of Graphics.
library(ggfortify)       # Draw Some Popular Packages, Time Series.
library(lubridate)       # Dates and times made easy with lubridate.
library(plyr)            # The split-apply-combine paradigm for R.
library(psych)           # Toolbox for personality, psychometric theory and experimental psychology.
library(skimr)           # Provides an alternatuve to the default summary function within R.
library(xts)             # Test For Time-Series Object.
library(zoo)             # Infrastructure for Regular and Irregular Time Series.
library(ggthemes)        # This package contains extra themes for ggplot.
library(fpp2)            # Library for Book Forecasting: principles and practice.
library(RODBC)           # Open connections to ODBC databases.
library(sqldf)           # Manipulate R Data Frames Using SQL.
library(shiny)           # Web Application Framework for R
library(shinythemes)     # Themes for Shiny
library(leaflet)         # Create Interactive Web Maps with the JavaScript 'Leaflet'.
library(cluster)

################## DEFINIR RUTA DE ARCHIVOS ##################

DirectorioDatos <- "C:/Users/rodrigl2/OneDrive - Brambles/Desktop/Luis/ITAM/Multivariable"
setwd(DirectorioDatos)

################## EXPLICACIÓN DATOS ##################

# The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. 
# The reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) 
# and sensory (the output) variables are available (e.g. there is no data about grape types, 
#                                                   wine brand, wine selling price, etc.).

################## CARGAR ARCHIVOS DE DATOS ##################

VinosCalidad <- read.csv('VinosCalidad.csv')
VinosCalidad$Calidad <- factor(VinosCalidad$Calidad, levels = c("Excelente", "Muy Bueno", "Bueno", 
                                                                "Distinguido", "Aceptable", 
                                                                "Regular", "Deficiente"))
VinosCalidad <- VinosCalidad%>%
  dplyr::select(Tipo, Calidad, Calidad.Num, Acidez.Fija, Acidez.Volatil, Acido.Citrico, Azucar.Residual, 
                Cloruro, Dioxido.Sulfuro.Libre, Dioxido.Sulfuro.Total, Densidad, pH, Sulfato, Alcohol)

################## ANALISIS DE CUMULOS ##################
set.seed(22) #22 # 30

# Generación de muestreo

VinosSample <- VinosCalidad[sample(nrow(VinosCalidad), 100), ]

row.names(VinosSample) <- paste(substr(VinosSample$Tipo, 1,1), substr(VinosSample$Calidad, 1,4), c(1:100), sep = ".")

# View(VinosSample)

VinosDescribe <- VinosSample%>%
  dplyr::group_by(Tipo, Calidad)%>%
  dplyr::summarise(Num.Registros = n())

# View(VinosDescribe)

# 1.- Escalamiento de las variables cuantitativas.

VinosSample[,4:ncol(VinosSample)] <- scale(VinosSample[,4:ncol(VinosSample)], center = T, scale = T)

# View(VinosSample)

VinosCuantitativo <- VinosSample[,4:ncol(VinosSample)]

# View(VinosCuantitativo)

d <- cluster::daisy(VinosCuantitativo, metric = "gower")

# d

################## CONSTRUCCION DE GRUPOS ##################

# Vecino más cercano - single linkage
# Vecino mas lejano  - complete linkage
# Media grupal       - average

agrup_1 <- hclust(d, method = "single")
# View(agrup_1)
plot(agrup_1, main = "Vecino más cercano")

agrup_2 <- hclust(d, method = "complete")
# View(agrup_2)
plot(agrup_2, main = "Vecino más lejano")

agrup_3 <- hclust(d, method = "average")
# View(agrup_3)
plot(agrup_3, main = "Media Grupal")

#Paso 3 : Elegir el numero de grupos
#Paso 4 : Decribir los grupos

#Pregunta 5

ngrupos = 10

grupos <- cutree(agrup_3, k = ngrupos) #k es numero de grupos, h es altura

media.gpo <- lapply(1:ngrupos, function(nc)apply(VinosCuantitativo[grupos==nc,],2,mean))
media.gpo <- lapply(media.gpo,round,2)
media.gpo

sd.gpo    <- lapply(1:ngrupos, function(nc)apply(VinosCuantitativo[grupos==nc,],2,sd))
sd.gpo    <- lapply(sd.gpo,round,2)
sd.gpo

#nrow(VinosCuantitativo[grupos == ngrupos, ])
#unique(grupos)

IntegraGrupos <-as.data.frame(grupos)%>%
  dplyr::group_by(grupos)%>%
  dplyr::summarise(Total = n())%>%
  as.data.frame()

# View(IntegraGrupos)

# pairs(VinosCuantitativo, panel=function(x,y) text(x,y, grupos, col=grupos))

for (i in 1:ngrupos) {
  print(paste("Grupo", i, sep = " "))
  print(summary(VinosCuantitativo[grupos==i,]))
}

for (i in 1:ngrupos) {
  print(paste("Grupo", i, sep = " "))
  print(summary(VinosSample[grupos==i, 1]))
}

for (i in 1:ngrupos) {
  print(paste("Grupo", i, sep = " "))
  print(summary(VinosSample[grupos==i, 2]))
}

for (i in 1:ngrupos) {
  print(paste("Grupo", i, sep = " "))
  print(summary(VinosSample[grupos==i, 1:2]))
}

################## CUMULOS OPTIMIZACION ##################

# Empleo el conjunto de datos muestreado y estandarizado.

# View(VinosCuantitativo)

# Determinar el número de clusters k, mediante grafico.

wss <- numeric(0)

for (i in 1:20) {
  
  W <- kmeans(VinosCuantitativo, i)$tot.withinss
  wss <- c(wss, W)
}

plot(x = 1:20, y = wss, type = "l", xlab = "Num Clusters", ylab = "Suma de cuadrados")

result_k5 <- kmeans(VinosCuantitativo, 5)

result_k5$centers
result_k5$size

gruposV <- result_k5$cluster
pairs(VinosCuantitativo[,1:4], col = gruposV)
pairs(VinosCuantitativo[,5:8], col = gruposV)
pairs(VinosCuantitativo[,9:11], col = gruposV)
pairs(VinosCuantitativo[,c(4,8,9,11)], col = gruposV)

################## CUMULOS JERARQUICOS PARA DETERMINAR K ##################

agrupVJ <- hclust(dist(VinosCuantitativo), method = "complete")
plot(agrupVJ)

grupos.h  <- cutree(agrupVJ, k = 10)
media.gpo <- lapply(1:10, function(nc)apply(VinosCuantitativo[grupos.h==nc,],2,mean))
centros   <- matrix(unlist(media.gpo), ncol = 11, byrow = 1)
centros

# Correr k medias utilizando como particion inicial el resuktado del analisis jerarquico

result_k3_j <- kmeans(VinosCuantitativo, centers = centros)
grupos_j    <- result_k3_j$cluster
#pairs(VinosCuantitativo, col = grupos_j)
pairs(VinosCuantitativo[,c(4,8,9,11)], col = grupos_j)

#result_k3_j$centers
#dim(centros)

################## ESCALAMIENTO MULTIDIMENSIONAL ##################

# Calcular disimilitudes

delta <- 1 - cor(VinosCuantitativo)

# Realizar escalamiento multidimensional

result <- cmdscale(delta, k=2, eig=TRUE) #k es el nUmero de dimensiones

# Verificar que k=2 es un nUmero adecuado de dimensiones

#Alfa 1
result$GOF[1] #Goodness of fit
#Alfa 2
(result$eig[1]^2 + result$eig[2]^2)/sum(result$eig^2)

#Pregunta 5 
Dgraf <- dist(result$points) #distancia euclideana entre coordenadas
plot(as.dist(delta), Dgraf)
abline(0,1, col="red")

#Pregunta 6 Graficar el resultado del escalamiento multidimensional
plot(result$points[,1], result$points[,2], type="n", asp=1)
text(result$points[,1], result$points[,2], labels=colnames(VinosCuantitativo), asp=1)

################## ANALISIS DE CORRESPONDENCIA ##################

# Representar gráficamente las asociaciones recíprocas entre variables categóricas. 
# Uso el conjunto original de vinos

# View(VinosCalidad)
VinosCategoria <- VinosCalidad%>%
  dplyr::select(Tipo, Calidad, Alcohol)

VinosCategoria$CAlcohol <- cut(VinosCategoria$Alcohol, 
                   breaks=c(-Inf, 10.5, 12.5, Inf), 
                   labels=c("Bajo","Medio","Alto"))


# View(VinosCategoria)

VinosTable <- table(VinosCategoria$Calidad, VinosCategoria$CAlcohol)
# View(VinosTable)

# library(MASS)
chisq.test(VinosTable)

# Valor p < 2.2e-16, por lo que se rechaza H0

# Ya que si existe relación entre variables, se procede a realizar un análisis de correspondencia simple.

prop.table(as.matrix(VinosTable),1) # 1 - Filas
prop.table(as.matrix(VinosTable),2) # 2 - Columnas

# ncol(VinosTable)
# nrow(VinosTable)

#Pregunta 3 realizar anÃ¡lisis de correspondencias simple
#MÃ¡ximo podemos tener 1 dimension
min(ncol(VinosTable)-1, nrow(VinosTable)-1)

# library(ca)

result <- ca(VinosTable)
plot(result, main = "Correspondencia Calidad vs Grado Alcohol")

################## ANALISIS DISCRIMINANTE ##################

# library(MASS)

VinosSample2 <- VinosCalidad[sample(nrow(VinosCalidad), 1500), ]

row.names(VinosSample2) <- paste(substr(VinosSample2$Tipo, 1,1), substr(VinosSample2$Calidad, 1,4), c(1:1500), sep = ".")

# View(VinosSample)

# 1.- Escalamiento de las variables cuantitativas.

VinosSample2[,4:ncol(VinosSample2)] <- scale(VinosSample2[,4:ncol(VinosSample2)], center = T, scale = T)


VinosComplete <- VinosSample2[complete.cases(VinosSample2), ]


Vinos_ad <- lda(VinosComplete[,4:ncol(VinosComplete)], VinosComplete$Calidad)
# Vinos_ad

Vinos_ad$scaling

# Ejercicio 3 Obtener proporcion de la taza que representa cada funcion

Vinos_ad$svd

Vinos_ad$svd[1]^2/sum(Vinos_ad$svd^2)
Vinos_ad$svd[2]^2/sum(Vinos_ad$svd^2)
Vinos_ad$svd[3]^2/sum(Vinos_ad$svd^2)
Vinos_ad$svd[4]^2/sum(Vinos_ad$svd^2)
Vinos_ad$svd[5]^2/sum(Vinos_ad$svd^2)
Vinos_ad$svd[6]^2/sum(Vinos_ad$svd^2)

# Obtener la función discriminante
fn_discrV <- predict(Vinos_ad, VinosComplete[,4:ncol(VinosComplete)])
ldahist(data = fn_discrV$x[,1], g = VinosComplete$Calidad)

# Proceso de validación

Vinos_ad_cv <- lda(VinosComplete[,4:ncol(VinosComplete)], VinosComplete$Calidad, CV = T)
table(Vinos_ad_cv$class, VinosComplete$Calidad)


# Dividiendo la muestra

datos_calculo <- sample(1:nrow(VinosComplete), nrow(VinosComplete)*0.7, replace = F) # Tomamos el 70% de las observaciones.
Vinos_ad2     <- lda(VinosComplete[datos_calculo,4:ncol(VinosComplete)], VinosComplete$Calidad[datos_calculo])

fn_discr_b <- predict(Vinos_ad2, VinosComplete[-datos_calculo,4:ncol(VinosComplete)])
table(fn_discr_b$class, VinosComplete$Calidad[-datos_calculo])

# Graficamos las dos variables discriminatorias y las especies originales y las de CV
plot(fn_discrV$x[,1], fn_discrV$x[,2], col = VinosComplete$Calidad, pch = c(Vinos_ad_cv$class))
