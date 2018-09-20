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
(library(plyr))
# 
# tk <- as.data.frame(read.xlsx(file = "IAK.xlsx",
#                               sheetName = "Holdings",
#                               colIndex=1,
#                               startRow=10,
#                               endRow=73,header = FALSE))



options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("dN9QssXxzTxndaqKUQ_i")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns,
                            ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

#### Para poder descargar los datos actualizados se necesitaria estar suscrito a la base de datos diaria

# Quandl.api_key("Us_v4rfs-m_kLT1skgsQ")
# temp <-list()
# for(i in 1:length(Datos)){
#   temp[[i]] <- Quandl(paste0("EOD/",tk[i]))
# }





ETF=read_xls("ETF.xls")
tk<-ETF$`14-Sep-2018`[9:length(ETF$`14-Sep-2018`)]


cs <- c("date", "adj_close")

# Fecha inicial y fecha final
fs <- c("2016-01-20", "2018-01-20")

# Descargar Precios
Datos <- list()

for(i in 1:length(tk)) {
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])
}
for(i in 1:length(tk)){
  Datos[[i]]<-Datos[[i]][order(Datos[[i]][,1]),]
}
names(Datos) <- tk


longitudes <- c()


for(i in 1:length(Datos)){
  longitudes[i] <- length(Datos[[i]]$date)
}
longs <- count(longitudes)
l<-longs(wich.max(longs(freq),1))

#maximo <- max(longitudes)
completos <- which(longitudes == l)

DatosN <- Datos[completos]


# Vector para almacenar columnas de interes
columnas <- c()
nuevos   <- c()

# Funci?n para repetir una funci?n por cada columna del data.frame
Precios <- do.call(cbind, DatosN)

# Crear vector con nombres de columnas de interes = "nombredeactivo.adj_close_r"
for(i in 1:length(tk)){
  nuevos[i] <- paste(tk[i], ".adj_close", sep="")
}

# Extraer 1 renglon para obtener los nombres de las columnas
nombres <- colnames(Precios[1,(names(Precios) %in% nuevos)])

# Elejir una columna Date y las dem?s columnas de rendimientos

Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date


##hacer correccion para voltear los precios
# temp<-Precios
# for(i in 1:(length(DatosN))){
#   temp[i]<-rev(Precios[i])
# }

# Reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos

# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

Historico <- data.frame("Date" = row.names(Precios),
                        "Precio" = Precios[,1], 
                        "R_Precio" = 0, 
                        "R_Activo" = 0,
                        "R_Cuenta" = 0, 
                        "Capital" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA, "Comisiones" = 0, "Mensaje" = NA)

# *Date*       : Fecha (Proviene desde los precios que bajaron).
# *Precio*     : Precio individual del activo.
# *R_Precio*   : Rendimiento diario del precio (dia a dia).
# *R_Activo*   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
# *Capital*    : El dinero no invertido (Equivalente a Efectivo).
# *Balance*    : El valor del portafolio (Precio diario X Titulos).
# *R_Cuenta*   : Balance + Capital (Cada dia respecto al capital inicial).
# *Titulos*    : Acciones que se tienen.
# *Titulos_a*  : Titulos acumulados.
# *Operacion*  : Indicativo de Compra (1), Mantener (0), Venta (-1).
# *Comisiones* : 0.0025 ? 0.25% por el valor de la transacci?n.
# *Mensaje*    : Un texto que indique alguna decisi?n o indicativo de que ocurri? algo.

Regla0_R <- -0.03  # Considerar una oportunidad de compra en un rendimiento de -6% o menor.
Regla1_I <- 0.20   # Porcentaje de capital para comprar titulos para posicion Inicial.
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra.
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio.
Regla4_C <- 0.0025 # Comisiones pagadas por compra.
Regla5_K <- 100000 # Capital Inicial.

# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Titulos[1]*Historico$Precio[1]

# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Balance[1]-Historico$Comisiones[1]

# -- Iniciamos con una postura de mantener.
Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

# -- Calcular R_Activo
for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- round((Historico$Precio[i]/Historico$Precio[1])-1,2)
}

# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
Historico$Titulos_a[1]<-Historico$Titulos[1]
for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R){ # Generar Se?al
    
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i] > 0){ # Si hay capital
      
      if(Historico$Capital[i]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        
        Historico$Operacion[i] <- "Compra"
        Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
        
        compra <- Historico$Precio[i]*Historico$Titulos[i]  
        Historico$Comisiones[i] <- compra*Regla4_C
        
        Historico$Titulos_a[i] <- Historico$Titulos[i-1]+Historico$Titulos[i]
        Historico$Capital[i]<-Historico$Capital[i]-Historico$Titulos[i]*Historico$Precio[i]-Historico$Comisiones[i]
        Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
        Historico$Balance[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
        Historico$Mensaje[i] <- "Se hizo una compra"
      }
      
    }
    else { # No hubo capital
      Historico$Mensaje[i] <- "P"
      Historico$Capital[i]<-Historico$Capital[i-1]
      Historico$Titulos[i] <-0
      Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
      Historico$Balance[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
       
      
      
    }
    
    
  }
  else { # Sin se?al
    Historico$Mensaje[i] <- "No hubo un rendimiento que activara la señal"
    Historico$Capital[i]<-Historico$Capital[i-1]
    Historico$Titulos[i] <-0 
    Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
    Historico$Balance[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
    
    
  }
  
}
Historico$Capital[i]+Historico$Balance[i]