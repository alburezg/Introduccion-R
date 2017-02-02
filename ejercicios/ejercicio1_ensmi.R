###########################################
###### Ejercicios de mnaipulcion de datos
###########################################

###########################################
######              ENSMI
###########################################

######################
# Importar datos
######################

## Opcion 1

# Usar menu a la derecha

## Opcion2

hogares <- read.csv("C:/Users/USUARIO/Documents/hogares.csv")

## Opcion 3

getwd()
setwd("C:/Users/USUARIO/Documents ")
hogares <- read.csv(" hogares.csv")

######################
# Explorar datos
######################

str(hogares)# resumen de variables
head(hogares[,1:5]) # Muestra primeras 6 lineas
head(hogares$mhp15) # agua para beber
head(hogares$mhogar) # id de hogar
tail(hogares[,1:5]) # ultimas 6 lineas
nrow(hogares) # numero de filas
ncol(hogares) # numero de columnas

# tabulaciones
# ============

# table() para tabular factores o variables categoricas:
#MHP16.- Que tipo de servicio sanitario tiene en su casa?
class(hogares$mhp16)
table(hogares$mhp16)
tabla_sanitario <- table(hogares$mhp16) # lo podemos guardar

# con el paquete descr
# ====================

install.packages("descr")
library(descr)

freq(hogares$mhp16) # para frecuencias de variables de factor

#tabulacion de region contra numero de visitas
CrossTable(hogares$mhreg, hogares$mhvisit, prop.r = F,prop.c = F, prop.t = F,
           prop.chisq = F, drop.levels = T, total.r = T, total.c = T, format = "SPSS")

# mostrando proporciones de fila
CrossTable(hogares$mhreg, hogares$mhvisit, prop.r = T,prop.c = F, prop.t = F,
           prop.chisq = F, drop.levels = T, total.r = T, total.c = T, format = "SPSS")

# mostrando proporciones de total
CrossTable(hogares$mhreg, hogares$mhvisit, prop.r = F,prop.c = F, prop.t = T,
           prop.chisq = F, drop.levels = T, total.r = T, total.c = T, format = "SPSS")

# idioma materno contra uso traductor

idioma_materno <- hogares$mhidic
freq(idioma_materno)
uso_traductor <- hogares$mhtra
freq(uso_traductor)

CrossTable(idioma_materno, uso_traductor, prop.r = F,prop.c = F, prop.t = F,
           prop.chisq = F, drop.levels = T, total.r = T, total.c = T, format = "SPSS")

# medidas de tendencia central
# ----------------------------

#mhaltitud registra altitud de encuesta
max(hogares$mhaltitud) # valor maximo
min(hogares$mhaltitud) # valor minimo
mean(hogares$mhaltitud) # media de columna
median(hogares$mhaltitud) # mediana de columna
sd(hogares$mhaltitud)

# No. mimebros el hogar:
mean(hogares$mhmembrs) # por que da NA?
mean(hogares$mhmembrs, na.rm=T)
median(hogares$mhmembrs, na.rm=T)
sd(hogares$mhmembrs, na.rm=T)

# funciones de descripcion de datos
describe(hogares$mhmembrs)
summary(hogares[,540:550])

# MHP17.-  Tiene:     A) Luz electrica
# MHP17.- B) Energia solar
# MHHP17.- C) Radio
# MHP17.- D) Telefono de linea
# MHP17.- E) Telefono celular
# MHP17.- F) Televisor
# MHP17.- G) Refrigeradora
# MHP17.- H) Lavadora
# MHP17.- I) Secadora de ropa
# MHP17.- J) Horno de micro ondas
# MHP17.- K) Computadora

# Depto, Region, Area:
summary(hogares[,689:691])

# otra alternativa:

install.packages("Hmisc")
library(Hmisc)
describe(hogares[,689:691])

# exploracion grafica elemental
# -----------------------------

hist(hogares$mhaltitud) # histograma
boxplot(hogares$mhaltitud) # boxplot

######################
# Edicion de datos
######################

# filtrar valores por fila
# =========================

# Nos interesas altitudes menores a 3000m
hogares <- hogares[hogares$mhaltitud < 3000,]
View(hogares) # abrir en pestania
hogares <- edit(hogares) # abrir para edicion

#  Recodificar variable
# ======================

# mhp15 Fuente de agua para beber.
class(hogares$mhp15)
levels(hogares$mhp15)
# crear variable binaria: agua pozo
pozo <-ifelse(hogares $mhp15 =="pozo mecánico/manual", 1, 0)
# crear variable binaria: rio o lluvia
prec <-ifelse(hogares$mhp15 =="Agua de lluvia" | hogares$mhp15 =="río/acequia/manantial",1, 0)
hogares$prec <- prec # opcional