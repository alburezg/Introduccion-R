###################################################
####### Ejercicio estimaciones desigualdad ########
#######                                    ########
####### ENEI 2010                          ########
#######                                    ########
####### PONDERANDO DATOS                   ########
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
# install.packages("convey") # para analisis de desigualdad para datos de encuestas

# cargar paquetes

library(foreign) 
library(Hmisc) 
library(stats) 
library(survey) 
library(convey) 

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

# Ponderar los datos
# -------------------

# En R no existe un equivalente a la sintaxis "Weigh by ..." de SPSS.
# Por lo general, se debe ponderar al momento de realizar el analisis,
# Esto resultara familiar para usuarios de Stata que hayan utilizado comandos como:
# regress y x1 x1 [aweight=wt]
# Ahora bien, en Stata tambien se puede (o se podia) hacer algo como esto:
# svyset enei [fweight = FACTOR]
# El paquete 'survey' en R ofrece una sintaxis similar. Sin embargo, las funcinens de este
# paquete crean un objeto de clase survey.design que puede trabajarse solo con funciones ad hoc
# El mismo paquete 'survey' incluye estas funciones.
# Otras funciones especificas para calcular medidas de desigualdad con datos ponderados vienen
# del paquete 'convey'

# Entonces, aplicamos los pesos:
enei <- svydesign(ids = ~1, data = enei, weights = enei$FACTOR) # crea un nuevo objeto
# ids permite identificar los cluster usados en multistage sampling. Si no aplica, usar ~1
class(enei.w) # ya no estamos trabajando con una data.frame

enei.index <- convey_prep(enei) # creamos este otro objecto que usaremos para estimar indices

#========================
# Explorar datos
# =======================

### Descripcion basica de los datos
options(digits = 10, scipen = 999) # de ser necesario, esto deshabilita notacion exponencial y muestra siemre 10 digitos

summary(enei)

# Media
# -----

svymean(~YPERCAPITA, enei, na.rm = T) # notar que es un comando distinto a 'mean()'
# na.rm indica que valores faltantes deben evitarse

### Generar cuantiles

# Cuartiles
#----------
# de manera individual
svyquantile(~YPERCAPITA, enei, 0.25, na.rm = TRUE) # primer cuartil
svyquantile(~YPERCAPITA, enei, 0.5, na.rm = TRUE) # segundo cuartil
svyquantile(~YPERCAPITA, enei, 0.75, na.rm = TRUE) # tercer cuartil
# O, todos de una vez:
cuartiles <- svyquantile(~YPERCAPITA, enei, c(0.25,0.5,0.75), na.rm = TRUE) 
cuartiles <- as.numeric(cuartiles) # guardar como numero

# Deciles
# -------
valores <- seq(from = 0.1, to = 0.9, by = 0.1) # para definir 'cortes' en funcion 'quantile'
deciles <- svyquantile(~YPERCAPITA, enei, valores, na.rm = TRUE)
deciles <- as.numeric(deciles)

# Centiles
#---------
valores <- seq(from = 0.01, to = 0.99, by = 0.01)
centiles <- svyquantile(~YPERCAPITA, enei, valores, na.rm = TRUE)
centiles <- as.numeric(centiles)

# Analisis basado en cuantiles 
# =============================

# A. Dispersion de deciles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Cuantas veces mas gana en promedio alguien del ultimo decil con respecto al primero? 

d1 <- deciles[1] # primer decil
d9 <- deciles[9] # noveno decil
d9/d1 # razon de un decil al otro

# 2. Cuantas veces gana mas en promedio alguien el percentil 5 mas rico que el mas pobre?

c5 <- centiles[5] # cuantil 5
c95 <- centiles[95] # cuantil 95
c95/c5 # razon de un cuantil al otro

# 3. Cuantas veces mas gana en promedio alguien del ultimo percentil con respecto al primero?

c1 <- centiles[1]
c99 <- centiles[99]
c99/c1 # razon de un cuantil al otro

# B. Proporcion del ingreso
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Que proporcion del ingreso va al 10% mas pobre?

# Estimar la suma de ingresos de los miembros del primer decil
d1.mem <- subset(enei, YPERCAPITA <= d1) # guardar en un nuevo vector solo a miembros de primer decil
d1.ag <- as.numeric(svytotal(~YPERCAPITA, d1.mem, na.rm=T)) # Sumar ingresos de miembros de este vector
# Suma de todos los ingresos
ingresos.ag <- as.numeric(svytotal(~YPERCAPITA,enei, na.rm=T))
# razon de agregado de ingresos
d1.ag/ingresos.ag 

# 2. Que proporcion del ingreso va al 50% mas pobre?

# Estimar la suma de ingresos de los miembros del cuantil 50
c50.mem <- subset(enei, YPERCAPITA <= centiles[50]) # guardar en un nuevo vector ingresos menores o iguales a la media
c50.ag <- as.numeric(svytotal(~YPERCAPITA, c50.mem, na.rm=T)) # Sumar ingresos de miembros de este vector
# razon de agregado de ingresos
c50.ag/ingresos.ag 

# 3. Que proporcion del ingreso va al 10% mas rico?

# Estimar la suma de ingresos de los miembros del cuantil 90
c90.mem <- subset(enei, YPERCAPITA <= centiles[90]) # guardar en un nuevo vector ingresos menores o iguales a la media
c90.ag <- as.numeric(svytotal(~YPERCAPITA, c90.mem, na.rm=T)) # Sumar ingresos de miembros de este vector
# razon de agregado de ingresos
c90.ag/ingresos.ag

# Analisis usando indices de desigualdad
# ======================================

# El paquete "convey" permite calcular indices de desigualdad con datos ponderados
# El paquete equivalente para datos no ponderados es 'ineq'
# para mas detalles https://cran.r-project.org/web/packages/convey/vignettes/convey-intro.html

# A. Gini
# ~~~~~~~~~

svygini(~YPERCAPITA, enei.index, na.rm=T) # notar que estamos usando enei.index

# La siguiente funcion grafica la curva de Lorenz
svylorenz(~YPERCAPITA, enei.index, na.rm=T)

# B. Atkinson
# ~~~~~~~~~~~~
svyatk(~YPERCAPITA, enei.index, na.rm=T) # notar que estamos usando enei.index