###################################################
####### Ejercicio 2                        ########
#######                                    ########
####### ENEI 2010                          ########
#######                                    ########
###################################################

# instalar solo si no estan instalados ya:

install.packages("foreign") # para importar spss
library(foreign) 

#========================
# Importar datos
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

# importar los datos 
enei <- read.spss(archivo, to.data.frame = T)
#enei <- suppressWarnings(read.spss(archivo, to.data.frame = T)) # (supresswarnings evita mostrar advertencias)

View(enei)
#========================
# Editar datos
# =====================
enei <- enei[!duplicated(enei$NUM_HOG),] 
# Guardar la varible de ingreso anual per capita en un vector
#------------------------------------------------------------
ingresos <- enei$YPERCAPITA

### Descripcion basica de los datos

describe(ingresos)

### EJERCICIOS

#1. Crear una nueva variable para quienes esten bajo la media y uno para los que esten sobre ella

media <- mean(ingresos, na.rm=T)
menor_media <- ifelse(ingresos<media,T,F)
enei$menor_media <- menor_media #opcional

#1.1 Crear variable para indicar cuartil

# Obtener cuartiles
#----------
# de manera individual
quantile(ingresos, 0.25, na.rm = TRUE) # primer cuartil
quantile(ingresos, 0.5, na.rm = TRUE) # mediana
quantile(ingresos, 0.75, na.rm = TRUE) # tercer cuartil
# O, todos de una vez:
cuartiles <- quantile(ingresos, c(0.25, 0.50, 0.75), na.rm = TRUE) 
cuartiles <- as.numeric(cuartiles) # guardar como numero

#recodificar
cuartil_ingreso <- NA # crear vector vacio
cuartil_ingreso[ingresos<=cuartiles[1]] <- 1
cuartil_ingreso[ingresos>cuartiles[1] & ingresos<=cuartiles[2]] <- 2
cuartil_ingreso[ingresos>cuartiles[2] & ingresos<=cuartiles[3]] <- 3
cuartil_ingreso[ingresos>cuartiles[3]] <- 4
enei$cuartil_ingreso <- cuartil_ingreso

#2.Usando un bucle, sumar el ingreso de todas las mujeres menores de 30 anios

ingreso_m <- 0
for (n in 1:nrow(enei)) {
  # definir que filas debe 'saltarse' el bucle
  if (is.na(enei$PPA03[n])) next # ignorar si sexo es NA
  if (is.na(enei$PPA04[n])) next # ignorar si edad es NA
  if (is.na(enei$YPERCAPITA[n])) next # ignorar si ingreso es NA
  # considerar solo mujeres menor de treinta
  if (enei$PPA03[n] == "Mujer" & enei$PPA04[n] < 30) {
      ingreso_temp <- enei$YPERCAPITA[n]
      ingreso_m <- ingreso_m + ingreso_temp
    }
}