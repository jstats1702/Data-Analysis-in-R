####################
# Introducción a R #
####################

#-------------------------------------------------------------------------------

# Por qué R?
# - Gratis
# - Intuitivo, facil de aprender
# - Source code, personalizar tareas
# - Gran numero de librerias
# - Popular en educación e industria
# - Big data

#-------------------------------------------------------------------------------

# '#' se utiliza para escribir "comentarios"

#-------------------------------------------------------------------------------

# Scrirt y Consola
# "Limpiar" la consola : Ctrl + L
# "Correr" codigo      : Ctrl + Intro

2 + 2

#-------------------------------------------------------------------------------

# Aritmetica basica
# + : suma
# - : resta
# / : division
# * : multiplicacion

(2 + 2) * (12 / 4)

#-------------------------------------------------------------------------------

# Lenguaje de programación orientado a objetos
# El "nombre" de los objetos es muy flexible
# - Usar nombres significativos
# - Letras mayusculas y minusculas representan caracteres diferentes
# - Es permitido usar puntos (.) y guiones bajos (_)
# - No usar nombres que inicien con numeros
# - No usar nombres ya definidos con anterioridad o y reservados
# - Por ejemplo, ya estan reervados: 
#   pi, exp, c, t, seq, sum, prod, mean, sd, plot, ...

# usar '=' o '<-' para definir objetos
# cuidado! Se recomienda no "sobre-escribir" objetos
x <- 13
val.1 = 2.7

#-------------------------------------------------------------------------------

# Concadenar valores
x <- c(3, 7, 1, 4, 2)

# secuencias de numeros
y <- 1:5

z <- seq(from = 2, to = 10, by = 2)

v <- rep(1, 5)

# clase y estructura de un objecto
class(x)

str(x)

# longitud de un objecto
length(x)

# operaciones
x + 2

x^2

exp(x)

x + y

y / z

# remover objetos
rm(val.1, x, y, z, v)

#-------------------------------------------------------------------------------

# clases de objetos: 
# numeric, character, logical, NA, matrix, data.frame, function, ...
ventas <- c(151.4, 134.2, 222.9, 102.5, 97.6, 115.7, 92.2, 113.1, 217.5, 249.9 )

ganancias <- c(42, 35, 24.1, 24.2, 21.9, 27.8, 16.6, 24.9, 45.2, 17.1)

activos <- c(3473.2, 3016.6, 620.9, 2513, 1943.4, 2816, 2196.8, 2611.5, 331.1, 412.5)

valor <- c(229.8, 200.5, 409.9, 306.6, 274.4, 149.2, 231.9, 141.3, 752, 171.9)

pais <- c("China", "China", "US", "US", "US", "China", "US", "China", "US", "Japan")

class(pais)

#-------------------------------------------------------------------------------

A <- matrix(data = c(ventas, ganancias), nrow = 10, ncol = 2, byrow = FALSE) 

B <- matrix(data = c(activos, valor), nrow = 10, ncol = 2) 

# clase de objeto
class(A)

# dimension
dim(A)

# numero de filas
nrow(A)

# numero de columnas
ncol(A)

# operaciones
A + 2

A + B

2 * A

A * B

A %*% t(B)

dim(A * B)

dim(A %*% t(B))

# unir A y B por columnas
FG10 <- cbind(A, B)

# unir A y B por filas
rbind(A, B)

# remover A y B
rm(A, B)

# obtener un elemento
FG10[2, 1]

# obtener filas
FG10[1, ]

FG10[c(1, 2, 3), ]

FG10[1:3, ]

# obtener columnas
FG10[ , 1]

FG10[ , c(1, 2)]

FG10[ , 1:2]

rm(FG10)

#-------------------------------------------------------------------------------

# bases de datos (admite variables cualitativas y cuantitativas)
# filas    : individuos
# columnas : variables
FG10 <- data.frame(pais, ventas, ganancias, activos, valor)

# asignar nombres a las variables
colnames(FG10) <- c("country", "sales", "profits", "assets", "value")

# clase de objeto
class(FG10)

# dimension de la base de datos
dim(FG10)

# numero de individuos (tamaño de la muestra)
nrow(FG10)

# numero de variables
ncol(FG10)

# obtener variables
FG10$sales

FG10[ , c(1, 2)]

FG10[ , c("country", "sales")]

# obtener individuos
FG10[1, ]

FG10[c(1, 2, 6, 8), ]

# hay datos faltantes?
is.na(FG10)

# toda la tabla esta completa?
any(is.na(FG10))

# reemplazar algunos valores
FG10[2, 3] <- NA

FG10[5, ] <- NA

is.na(FG10)

any(is.na(FG10))

# identificar registros completos
registros.completos <- complete.cases(FG10)

class(registros.completos)

as.numeric(registros.completos)

sum( as.numeric(registros.completos) )

# base de datos completa
FG10.full <- FG10[registros.completos, ]

# base de datos incompleta
FG10[!registros.completos, ]

any( is.na(FG10.full) )

# analisis descriptivo completo de las Forbes 2000 depues ...

rm(FG10, FG10.full, registros.completos)

#-------------------------------------------------------------------------------

#########################################################
# Caso: Crimenes violentos por Estado en Estados Unidos #
#########################################################

# bases de datos incorporadas en R
data() 

# ver de que se trata la base de datos
?USArrests

class(USArrests) 

str(USArrests)

# dimension de la base de datos
dim(USArrests)

# nombre de las variables
colnames(USArrests)

# ver los nombres de los individuos
rownames(USArrests)

# encabezado de la base de datos
head(USArrests)

# final de la base de datos
tail(USArrests)

# tamaño de la muestra y numero de variables
nrow(USArrests)

ncol(USArrests)

#-------------------------------------------------------------------------------

# adjuntar la base de datos
# ultil para acceder a todas las variables directamente
attach(USArrests)

USArrests$Murder

Murder

# datos faltantes?
any( is.na(USArrests) )

#-------------------------------------------------------------------------------

#########################
# manipulacion de datos #
#########################

# tasa de [arrestos de] homocidios en California: 9 (cada 100,000 habitantes)
USArrests["California", "Murder"]

#-------------------------------------------------------------------------------

# todos los datos de California
USArrests["California", ]

#-------------------------------------------------------------------------------

# estados con tasas de violacion inferiores a 10
USArrests[Rape < 10, ]

dim(USArrests[Rape < 10, ])

#-------------------------------------------------------------------------------

# estados con % poblacion urbana superior a 80%
USArrests[UrbanPop > 80, ]

#-------------------------------------------------------------------------------

# estados con tasas de violacion inferiores a 10 y de homicidios inferiores a 3
USArrests[(Rape < 10) & (Murder < 3), ]

#-------------------------------------------------------------------------------

# estados con tasas de violacion inferiores a 10 o de homicidios inferiores a 3
USArrests[(Rape < 10) | (Murder < 3) , ]

#-------------------------------------------------------------------------------

# tasa minima de asaltos
min(Assault)

# identificar el(los) estado(s) con la tasa minima de asaltos
USArrests[Assault == min(Assault), ]

# tasa maxima de violaciones (cada 100,000)
max(Rape)

# identificar el(los) estado(s) con la tasa maxima de violaciones
USArrests[Rape == max(Rape), ]

#-------------------------------------------------------------------------------

# construccion de una nueva variable
# porcentage de asesinatos con respecto a la totalidad de crimenes
murder.pct <- round( 100 * Murder / (Murder + Assault + Rape), 1)

# generar la nueva base de datos con una variable adicional
USArrests2 <- cbind(USArrests, murder.pct)

#USArrests2 <- data.frame(USArrests, murder.pct)

colnames(USArrests2) <- c(names(USArrests), "Murder.pct")

head(USArrests2)

#-------------------------------------------------------------------------------

################################
# Algunas medidas estadisticas #
################################

### medidas estadisticas de posicion

mean(Rape)   # promedio
# en los 50 estados, la tasa promedio de crimenes correspondientes a violaion es 
# 21.232 (cada 100,000)

# mediana: valor que acumula el 50% de los datos
# el 50% de los estados tiene tasas de violacion inferiores a 20.1 (cada 100,000)
median(Rape) # mediana

max(Rape)    # maximo

min(Rape)    # minimo

# el 25% de los estados tiene tasas de violacion inferiores a 15.1 (cada 100,000)
quantile(x = Rape, probs = 0.25)  # cuartil 1

quantile(x = Rape, probs = 0.50)  # mediana

quantile(x = Rape, probs = 0.75)  # cuartil 3

quantile(x = Rape, probs = 0.90)  # percentil 90 
# de los 50 estados, el 90% (10%) presenta tasas de arrestos correspondientes a 
# a violacion son inferiorres (superiores) a 32.4 (cada 100,000)

#-------------------------------------------------------------------------------

### medidas estadisticas de dispersion

# Rango
max(Rape) - min(Rape)

# Rango intercuartilico
as.numeric( quantile(x = Rape, probs = 0.75) - quantile(x = Rape, probs = 0.25) )

sum( Rape - mean(Rape) )

# varianza
var(Rape)   
# la varianza tiene unidades cuadraticas (dificil interpretacion)
# la varianza es proporcional a la dispersion de los datos
# cuanto mas "grande" es la variaza, hay mayor dipersion en los datos con 
# respecto al promedio

# desviacion estandar o desviacion tipica
sd(Rape)

sqrt(var(Rape))
# la desviacion estandar es define como la raiz cuadrada de la varianza
# la desviacion estadar tiene unidades lineales (unidades de la variable)

# coeficiente de variacion
sd(Rape) / mean(Rape)
# para establecer si la desv. estandar es "grande" o "pequena" se compara con el 
# promedio:
# 0%  < CV <=  5% : disperison baja
# 5%  < CV <= 15% : dispersion moderada 
# 15% < CV        : dispersion alta
 
# la dispersion de la tasa de crimenes correspondientes a violaciones (cada 100,000) 
# en los 50 estados es alta respecto al la tasa promedio, dado que e CV = 44.1%.

# otras medidas de dispersion: desviacion media, desviacion mediana

#-------------------------------------------------------------------------------

#############
# Funciones #
#############

### defincion de funciones

# un argumento, una salida

f <- function(x) {
     v1 <- x + 1
     return( v1 )
}

f(x = 2)

f(2)

f(-3)

class(f)

# 'v1' no hace parte del Enviroment, solamente 'f' lo es

#-------------------------------------------------------------------------------

# mas de un argumento, mas de una salida

f <- function(x, y) {
     v1 <- x + y
     v2 <- x * y
     return(list( suma = v1, producto = v2 ))
}

f(2, 3)

f(x = 2, y = 5)

a <- f(x = 2, y = 5)

class(a)

str(a)

a$suma

a$producto

#-------------------------------------------------------------------------------

# if - else

f <- function(x) {
     if (x > 0) {
          out <- x
     } else {
          out <- -x
     }
     return( out )
}

f(1)

f(-1)

#-------------------------------------------------------------------------------

# if

f <- function(x, y, cond) {
     if (cond == "sumar") {
          out <- x + y
     } 
     if (cond == "multiplicar") {
          out <- x * y
     }
     return( out )
}

f(x = 2, y = 5, cond = "sumar")

f(x = 2, y = 5, cond = "multiplicar")

# hay muchas mas estructuras de control: for, switch, repeat, ...

rm(f)

#-------------------------------------------------------------------------------

# resumen de seis numeros de la base de datos
summary(USArrests)

# funcion de resumen personalizada

mi.resumen.uni <- function(x, r) {
     
     prom <- round(mean(x), r)
     med  <- round(median(x), r)
     CV   <- round(100 * sd(x)/prom, r)
     
     cat("--- Resumen --- \n",
         "1. Promedio : ", prom,    "\n",
         "2. Mediana  : ", med,     "\n",
         "3. CV       : ", CV,    "% \n", sep = "")
     
     return(list( promedio = prom, mediana = med , CV = CV ))
}

resumen.Rape <- mi.resumen.uni(x = Rape, r = 1)

class(resumen.Rape)

str(resumen.Rape)

resumen.Rape$promedio

resumen.Rape$mediana

resumen.Rape$CV

resumen.Assault <- mi.resumen.uni(x = Assault, r = 2)


#-------------------------------------------------------------------------------

# "des-adjuntar" una base de datos
detach(USArrests)

# borrar todos los objetos del Enviroment
rm( list = ls() )

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
