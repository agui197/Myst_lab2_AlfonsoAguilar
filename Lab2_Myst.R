# Remover todos los objetos del "Environment"
rm(list = ls())

###Cargar librerias a utilizar
(library(plotly))  #graficas interactivas
(library(Quandl))  #Descargar precios
(library(PortfolioAnalytics))  #teoria moderna de portafolios
(library(ROI))  #optimizacion para portafolio
(library(knitr))  #opciones de documentacion + codigo
(library(kableExtra))  #tablas en HTML
(library(readxl))
# 
# tk <- as.data.frame(read.xlsx(file = "IAK.xlsx",
#                               sheetName = "Holdings",
#                               colIndex=1,
#                               startRow=10,
#                               endRow=73,header = FALSE))

read_xls("prueba.xls")