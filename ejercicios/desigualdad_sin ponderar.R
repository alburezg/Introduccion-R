###################################################
####### Ejercicio estimaciones desigualdad ########
#######                                    ########
####### ENEI 2010                          ########
#######                                    ########
####### SIN PONDERAR DATOS                 ########
#######                                    ########
###################################################

#========================
# Preambulo: cargar paquetes
# ==========================

# instalar solo si no estan instalados ya:

# install.packages("foreign") # para importar spss
# install.packages("Hmisc") # para analizar datos
# install.packages("stats") # analisis 
# install.packages("survey") # para aplicar pesos
# install.packages("ineq") # para analisis desigualdad

# cargar paquetes

library(foreign) 
library(Hmisc) 
library(stats) 
library(stats) 
library(ineq) 

options(digits = 10, scipen = 999) # de ser necesario, esto deshabilita notacion exponencial y muestra siemre 10 digitos

#========================
# Primero, importar datos
# =======================

# Para quienes si pudieron conectarse con Dropbox ==================================
user <- Sys.info()[["user"]]                                                     #
# para ubicacion de la carpeta Dropbox                                             #
carpeta.con.archivos <- paste0("C:/Users/",user,"/Dropbox/curso R")                #
archivo <- paste0(carpeta.con.archivos, "/data/enei.sav")                          #
# ==================================================================================

# Para quienes no pudieron conectarse a Dropbox ====================================
# carpeta.con.archivos <- # escribir aqui el directorio donde estan los archivos   #
# archivo <- paste0(carpeta.con.archivos, "/data/enei.sav")                        #
# ==================================================================================

# importar los datos (supresswarnings evita mostrar advertencias)
enei <- suppressWarnings(read.spss(archivo, to.data.frame = T)) 

#========================
# Editar datos
# =====================

# Conservar una unica observacion por hogar 
# -----------------------------------------

# El analisis de desigualdad de ingreso se hara a nivel de hogar.
# Cada columna en la base actual representa a un individuo
# La columna de interes es YPERCAPITA "Ingreso total per cápita de los miembros del hogar (anual)" 

# Conservar una unica observacion por hogar:
# este comando conserva solo filas sin valors duplicados en la columa enei$NUM_HOG:
enei <- enei[!duplicated(enei$NUM_HOG),] 

# Guardar la varible de ingreso anual per capita en un vector
#------------------------------------------------------------
ingresos <- enei$YPERCAPITA

#========================
# Explorar datos
# =======================
View(enei)

### Descripcion basica de los datos

describe(ingresos)

### Graficar
boxplot(ingresos)
boxplot(ingresos, outline = F) # sin valores extremos

### Generar cuantiles usando funcion 'quantile'

# Cuartiles
#----------
# de manera individual
quantile(ingresos, 0.25, na.rm = TRUE) # primer cuartil
quantile(ingresos, 0.5, na.rm = TRUE) # mediana
quantile(ingresos, 0.75, na.rm = TRUE) # tercer cuartil
# O, todos de una vez:
cuartiles <- quantile(ingresos, c(0.25, 0.50, 0.75), na.rm = TRUE) 
cuartiles <- as.numeric(cuartiles) # guardar como numero

# Deciles
# -------
valores <- seq(from = 0.1, to = 0.9, by = 0.1) # para definir 'cortes' en funcion 'quantile'
deciles <- quantile(ingresos, valores, na.rm = TRUE) 
deciles <- as.numeric(deciles)

# Centiles
#---------
valores <- seq(from = 0.01, to = 0.99, by = 0.01)
centiles <- quantile(ingresos, valores, na.rm = TRUE) 
centiles <- as.numeric(centiles)

# Analisis basado en cuantiles 
# =============================

# A. Dispersion de deciles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Cuantas veces más gana en promedio alguien del ultimo decil con respecto al primero? 

d1 <- deciles[1] # primer decil
d9 <- deciles[9] # noveno decil
d9/d1 # razon de un decil al otro

# 2. Cuantas veces gana mas en promedio alguien el percentil 5 mas rico que el mas pobre?

c5 <- centiles[5] # cuantil 5
c95 <- centiles[95] # cuantil 95
c95/c5 # razon de un cuantil al otro

# 3. Cuantas veces más gana en promedio alguien del ultimo percentil con respecto al primero?

c1 <- centiles[1]
c99 <- centiles[99]
c99/c1 # razon de un cuantil al otro

# B. Proporcion del ingreso
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Que proporcion del ingreso va al 10% mas pobre?

# Primero, vamos a eliminar los valores faltantes
ingresos <- ingresos[!is.na(ingresos)]

# Luego, estimar la suma de ingresos de los miembros del primer decil
d1.mem <- ingresos[ingresos <= d1] # guardar en un nuevo vector solo a miembros de primer decil
d1.ag <- sum(d1.mem) # Sumar ingresos de miembros de este vector
# Suma de todos los ingresos
ingresos.ag <- sum(ingresos)
# razon de agregado de ingresos
d1.ag/ingresos.ag 

# 2. Que proporcion del ingreso va al 50% mas pobre?

# Estimar la suma de ingresos de los miembros del cuantil 50
c50.mem <- ingresos[ingresos <= median(ingresos)] # guardar en un nuevo vector ingresos menores o iguales a la media
c50.ag <- sum(c50.mem) # Sumar ingresos de miembros de este vector
# razon de agregado de ingresos
c50.ag/ingresos.ag 

# 3. Que proporcion del ingreso va al 10% mas rico?

# Estimar la suma de ingresos de los miembros del cuantil 90
c90.mem <- ingresos[ingresos >= centiles[90]] # guardar en un nuevo vector ingresos menores o iguales a la media
c90.ag <- sum(c90.mem) # Sumar ingresos de miembros de este vector
# razon de agregado de ingresos
c90.ag/ingresos.ag

# Analisis usando indices de desigualdad
# ======================================

# A. Gini
# ~~~~~~~~~
Gini(ingresos)
# Graficar: La funcion Lc() para ver de Lorenz
# 'plot' es la funcion generica para graficar
plot(Lc(ingresos), xlab = "% acumulativo de la poblacion", ylab = "% acumulativo de ingreso") 

# B. Atkinson
# ~~~~~~~~~~~~
Atkinson(ingresos)

# C. Theil
# ~~~~~~~~
Theil(ingresos)
