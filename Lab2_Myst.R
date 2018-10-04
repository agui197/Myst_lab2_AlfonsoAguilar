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
l<-longs$x[[which.max(longs$freq)]]

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

trading_strategy <- function(ReglaR,ReglaI,ReglaP){
  Historico   <- c()
  for(j in 1:length(tk_completos)){
    Historico[[j]] <- data.frame("Date" = row.names(Precios),
                                 "Precio" = Precios[,j], 
                                 "R_Precio" = 0, 
                                 "R_Activo" = 0,
                                 "R_Cuenta" = 0, 
                                 "Capital" = 0,"Flotante" = 0, "Balance" = 0, "Titulos" = 0,
                                 "Titulos_a" = 0,
                                 "Operacion" = NA, "Comisiones" = 0,"Comisiones_a" = 0, "Mensaje" = NA)
    
    # *Date*       : Fecha (Proviene desde los precios que bajaron).
    # *Precio*     : Precio individual del activo.
    # *R_Precio*   : Rendimiento diario del precio (dia a dia).
    # *R_Activo*   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
    # *Capital*    : El dinero no invertido (Equivalente a Efectivo).
    # *Flotante     : Es el valor de los titulos acumulados que se tienen por el valor del activo
    # *Balance*    : Capital + Flotante
    
    # *R_Cuenta*   : Balance + Capital (Cada dia respecto al capital inicial).
    # *Titulos*    : Acciones que se tienen.
    # *Titulos_a*  : Titulos acumulados.
    # *Operacion*  : Indicativo de Compra (1), Mantener (0), Venta (-1).
    # *Comisiones* : 0.0025 ? 0.25% por el valor de la transacci?n.
    # *Comisiones_a: Comisiones acumuladas
    # *Mensaje*    : Un texto que indique alguna decisi?n o indicativo de que ocurri? algo.
    
    Regla0_R <- ReglaR  # Considerar una oportunidad de compra en un rendimiento de -6% o menor.
    Regla1_I <- ReglaI   # Porcentaje de capital para comprar titulos para posicion Inicial.
    Regla2_P <- ReglaP   # Se utiliza el P% del L capital restante en cada compra.
    Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio.
    Regla4_C <- 0.0025 # Comisiones pagadas por compra.
    Regla5_K <- 100000 # Capital Inicial.
    # -- ----------------------------------------------------------------------------------------- -- #
    # -- ----------------------------------------------------------------------------------------- -- #
    Regla6_V <- (.1)+2*(Regla4_C)
    
    # -- ----------------------------------------------------------------------------------------- -- #
    # -- ----------------------------------------------------------------------------------------- -- #
    # -- ----------------------------------------------------------------------------------------- -- #
    
    # -- Calcular los Titulos de posicion inicial
    Historico[[j]]$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico[[j]]$Precio[1]
    
    Historico[[j]]$Titulos_a[1]<-Historico[[j]]$Titulos[1]
    
    # -- Se calculan comisiones iniciales
    Historico[[j]]$Comisiones[1] <- Historico[[j]]$Titulos[1]*Historico[[j]]$Precio[1]*Regla4_C
    Historico[[j]]$Comisiones_a[1] <- Historico[[j]]$Comisiones[1]
    
    # --- Calcular el valor flotante de la posicion
    Historico[[j]]$Flotante[1] <- Historico[[j]]$Titulos_a[1]*Historico[[j]]$Precio[1]
    
    # -- Todo remanente se dejar? registrado en la cuenta de efectivo.
    Historico[[j]]$Capital[1] <- Regla5_K-Historico[[j]]$Flotante[1]-Historico[[j]]$Comisiones[1]
    
    # -- Calcular el Balance
    Historico[[j]]$Balance[1] <- Historico[[j]]$Flotante[1]+Historico[[j]]$Capital[1]
    
    # -- Iniciamos con una postura de mantener.
    Historico[[j]]$Operacion[1] <- "Posicion Inicial"
    
    # -- El rendimiento de capital en el tiempo 1 es 0
    Historico[[j]]$R_Cuenta[1] <- 0
    
    # -- Mensaje inicial
    Historico[[j]]$Mensaje[1] <- "Inicializacion de cartera"
    
    # -- Calcular R_Precio
    Historico[[j]]$R_Precio <- round(c(0, diff(log(Historico[[j]]$Precio))),4)
    # -- Calcular R_Activo
    #for(i in 1:length(Historico$Date)){
    # Historico[[j]]$R_Activo[i] <- round((Historico[[j]]$Precio[i]/Historico[[j]]$Precio[1])-1,2)
    #}
    # -- ------------------------------------ -- #
    # -- ------------------------------------ -- #
    # -- ------------------------------------ -- #
    Historico[[j]]$R_Cuenta[1]<-Historico[[j]]$Capital[1]+Historico[[j]]$Balance[1]
    for(i in 2:length(Historico[[j]]$Date)){
      Historico[[j]]$R_Activo[i] <- round((Historico[[j]]$Precio[i]/Historico[[j]]$Precio[1])-1,2)
      if(Historico[[j]]$R_Precio[i] <= Regla0_R){ # Generar Se?al
        # Establecer capital actual, inicialmente, igual al capital anterior
        Historico[[j]]$Capital[i] <- Historico[[j]]$Capital[i-1]
        if(Historico[[j]]$Capital[i] > 0){ # Si hay capital
          if(Historico[[j]]$Capital[i]*Regla2_P > Historico[[j]]$Precio[i]){ # Si Capital minimo
            Historico[[j]]$Operacion[i] <- "Compra"
            Historico[[j]]$Titulos[i]   <- (Historico[[j]]$Capital[i]*Regla2_P)%/%Historico[[j]]$Precio[i]
            compra <- Historico[[j]]$Precio[i]*Historico[[j]]$Titulos[i]  
            Historico[[j]]$Comisiones[i] <- compra*Regla4_C
            Historico[[j]]$Comisiones_a[i] <- Historico[[j]]$Comisiones_a[i-1]+Historico[[j]]$Comisiones[i]
            Historico[[j]]$Titulos_a[i] <- Historico[[j]]$Titulos[i-1]+Historico[[j]]$Titulos[i]
            Historico[[j]]$Capital[i]<-Historico[[j]]$Capital[i-1]-compra-Historico[[j]]$Comisiones[i]
            Historico[[j]]$Titulos_a[i]<-Historico[[j]]$Titulos[i]+Historico[[j]]$Titulos_a[i-1]
            Historico[[j]]$Flotante[i] <- Historico[[j]]$Titulos_a[i]*Historico[[j]]$Precio[i]
            Historico[[j]]$Balance[i] <- Historico[[j]]$Capital[i]+Historico[[j]]$Flotante[i]
            Historico[[j]]$Mensaje[i] <- "Se hizo una compra"
            Historico[[j]]$R_Cuenta[i]<-Historico[[j]]$Balance[i]/Regla5_K-1
          }
        }
        else { # No hubo capital
          Historico[[j]]$Mensaje[i] <- "P"
          Historico[[j]]$Capital[i]<-Historico[[j]]$Capital[i-1]
          Historico[[j]]$Titulos[i] <-0
          Historico[[j]]$Comisiones[i] <-0
          Historico[[j]]$Comisiones_a[i] <- Historico[[j]]$Comisiones_a[i-1]
          Historico[[j]]$Titulos_a[i]<-Historico[[j]]$Titulos[i]+Historico[[j]]$Titulos_a[i-1]
          Historico[[j]]$Flotante[i] <- Historico[[j]]$Titulos_a[i]*Historico[[j]]$Precio[i]
          Historico[[j]]$Balance[i] <- Historico[[j]]$Capital[i]+Historico[[j]]$Flotante[i]
          Historico[[j]]$Mensaje[i] <- "Capital insuficiente"
          Historico[[j]]$R_Cuenta[i]<-Historico[[j]]$Balance[i]/Regla5_K-1
        }
      }
      # #else if(Historico[[j]]$R_Precio[i] >= Regla6_V){ #aparece una señal de venta
      # else if(Historico[[j]]$Titulos_a[i-1]*Historico[[j]]$Precio[i]>=Historico[[j]]$Balance[i-1]*(1+Regla6_V)+Historico[[j]]$Titulos_a*Historico[[j]]$Precio*Regla4_C){
      #   if(Historico[[j]]$Titulos_a[i-1] > 0){ #Si hay acciones para vender
      #     Historico[[j]]$Operacion[i] <- "Venta"
      #     Historico[[j]]$Titulos[i] <-Historico[[j]]$Titulos_a[i-1]
      #     venta <- Historico[[j]]$Precio[i]*Historico[[j]]$Titulos[i]
      #     Historico[[j]]$Comisiones[i] <- venta*Regla4_C
      #     Historico[[j]]$Titulos_a[i] <-0
      #     Historico[[j]]$Capital[i]<-Historico[[j]]$Capital[i-1]+venta-Historico[[j]]$Comisiones[i]
      #     Historico[[j]]$Balance[i] <- Historico[[j]]$Titulos_a[i]*Historico[[j]]$Precio[i]
      #     Historico[[j]]$Mensaje[i] <- "Se hizo una venta"
      #     Historico[[j]]$R_Cuenta[i]<-Historico[[j]]$Capital[i]+Historico[[j]]$Balance[i]
      #   }
      #   else{
      #     Historico[[j]]$Mensaje[i] <- "Activos insuficientes"
      #     Historico[[j]]$Capital[i]<-Historico[[j]]$Capital[i-1]
      #     Historico[[j]]$Titulos[i] <-0
      #     Historico[[j]]$Titulos_a[i]<-Historico[[j]]$Titulos[i]+Historico[[j]]$Titulos_a[i-1]
      #     Historico[[j]]$Balance[i] <- Historico[[j]]$Titulos_a[i]*Historico[[j]]$Precio[i]
      #     Historico[[j]]$R_Cuenta[i]<-Historico[[j]]$Capital[i]+Historico[[j]]$Balance[i]
      #   }
      # }
      else { # Sin se?al
        Historico[[j]]$Mensaje[i] <- "No hubo un rendimiento que activara la señal"
        Historico[[j]]$Operacion[i] <- "N/A"
        Historico[[j]]$Capital[i]<-Historico[[j]]$Capital[i-1]
        Historico[[j]]$Titulos[i] <-0 
        Historico[[j]]$Comisiones[i]<-0
        Historico[[j]]$Comisiones_a[i] <- Historico[[j]]$Comisiones_a[i-1]
        Historico[[j]]$Titulos_a[i]<-Historico[[j]]$Titulos_a[i-1]
        Historico[[j]]$Flotante[i] <- Historico[[j]]$Titulos_a[i]*Historico[[j]]$Precio[i]
        Historico[[j]]$Balance[i] <- Historico[[j]]$Capital[i]+Historico[[j]]$Flotante[i]
        Historico[[j]]$R_Cuenta[i]<-Historico[[j]]$Balance[i]/Regla5_K-1
      }
    }
  }
  names(Historico)<-c(names(DatosN))
  win<-0
  for(i in 1:length(DatosN)) {
    if(rev(Historico[[i]]$R_Cuenta)[1]>rev(Historico[[i]]$R_Activo)[1]){
      win<-win+1
    }
  }
  results<-list()
  results[[1]]<-Historico
  results[[2]]<-win
  return(results)
}
results<-trading_strategy(-.03,.2,.25)
names(results)<-c("Historico","Wins")

########################
# BUSQUEDA DE PARAMETROS OPTIMOS
########################

np<-200; #Número de particulas
#inicialización
x1p<-list()
for(j in 1:length(seq(np))){
  x1p[[j]]<-runif(3, min=0, max=1)
} 
x1pg<-c(0,0,0)
vx1<-list()
for(j in 1:length(seq(np))){
  vx1[[j]]<-c(0,0,0)
}
x1pL<-x1p

fxpg<-1000 #desempeño valor inicial del mejor global
fxpL<-list()
for(j in 1:length(seq(np))){
  fxpL[[j]]<-c(fxpg) #desempeño delos mejores locales
}

c1<-0.3 #Velocidad de convergencia al  mejor global
c2<-0.3 #velocidad de convergencia al mejor local


#iteraciones
for(k in 1:length(seq(2000))){
  fx<-list()
  a<- -1000
  for(i in 1:length(seq(np))){
    fx[[i]]<- -(trading_strategy(x1p[[i]][1],x1p[[i]][2],x1p[[i]][2])+a*max(x1p[[i]][1],0)+a*max(-x1p[[i]][2],0)+a*max(x1p[[i]][2]-1,0)+a*max(-x1p[[i]][3],0)+a*max(x1p[[i]][3]-1,0))
  }
  ind<-which.min(fx)
  val<-fx[[ind]]
}
##continuar con el remplazo de los mejores globales y locales


