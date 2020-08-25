#################################
# Caso: Forbes Global 2000 List #
#################################

#-------------------------------------------------------------------------------
# Descripcion de la base de datos:
# - Rank    : Ranking
# - Company : Nombre de la compañia
# - Sales   : Ventas
# - Profits : Ganancias
# - Assets  : Activos
# - Value   : Valor de la compañia en el mercado (abril 7, 2017)
# Nota: todas las cifras estan dadas en Billones (miles de millones) de dolares
# Fuente: https://www.forbes.com/global2000/list
#
# Mas info: "2017 Global 2000 Methodology: How We Crunch the Numbers"
#
# Objetivo: 
# - Describir numerica y graficamente las variables y sus relaciones.
# - Detectar patrones y anomalias.
# - Reducir (si es posible) la dimension de los datos.
#-------------------------------------------------------------------------------

#################
# base de datos #
#################

# importar la base de datos
FG <- read.csv2("C:/Users/Juan Camilo/Dropbox/UE/Finanzas Algoritmicas 35/sesion 1/FG2000_2017.csv")

# vizualizar la base de datos
View(FG)

dim(FG)
# 2000 registros
# 7 variables  

# variables
names(FG)

n <- nrow(FG)  # numero de registros
p <- ncol(FG)  # numero de variables

# hay datos faltantes?
any( is.na(FG) )

FG[!complete.cases(FG), ] # espacio en blanco, sin filtro
# ver la fuente!

# pocentaje de datos faltantes
100 * sum( !complete.cases(FG) )/n

#-------------------------------------------------------------------------------

# remover datos faltantes
FG <- FG[complete.cases(FG), ]

dim(FG)
# 1996 registros
# 7    variables

n <- nrow(FG)  # numero de registros

# adjuntar base de datos
attach(FG) # fija la base y deja buscar dentro de ella

#-------------------------------------------------------------------------------

#####################
# algunas consultas #
#####################

# compañias mas "valiosas"
# datos de las compañias con activos y valor superiores al percentil 99
p99.value <- quantile(x = Value, probs = 0.99)

p99.assets <- quantile(x = Assets, probs = 0.99)

FG[(Assets > p99.assets) & (Value > p99.value), ]

FG[(Assets > p99.assets) | (Value > p99.value), ]

FG[(Assets > p99.assets) & (Value > p99.value), c("Company", "Country", "Assets", "Value")]

#-------------------------------------------------------------------------------

# datos de las compañias colombianas
FG[Country == "Colombia", ]

FG[Country == "Colombia", c("Company","Assets")]

tail(x = FG, n = 5)

head(x = FG, n = 5)

FG[1:5, ]

FG[1991:n, ]

#-------------------------------------------------------------------------------

# datos de las compañias de sudamerica
paises.sura <- c("Argentina" , "Bolivia" , "Brazil"   , "Chile" , "Colombia", 
                 "Ecuador"   , "Guyana"  , "Paraguay" , "Peru"  , "Surinam" , 
                 "Uruguay", "Venezuela")

FG.sura <- FG[Country %in% paises.sura, ]

# ordenar (descendentemente) por "Value"
FG.sura[order(FG.sura$Value, decreasing = TRUE), c("Country", "Value")]

# visualizar datos de sudamerica
View(FG.sura)

#-------------------------------------------------------------------------------

##########################
# variables cualitativas #
##########################

# tabla de frecuencias (absolutas)
tabla <- table(FG$Country)

length(tabla)
# 61 paises en la lista Forbes Globe

# tabla ordenada descendentemente de acuerdo a la frecuencia
tabla <- tabla[order(tabla, decreasing = TRUE)]

#-------------------------------------------------------------------------------

# posicion de colombia de acuerdo a la cantidad de compañias
which(names(tabla) == "Colombia")

which(names(tabla) == "Venezuela")

which(names(tabla) == "Brazil")

which(names(tabla) == "Mexico")

which(names(tabla) == "Peru")

head(tabla, n = 10)

tail(tabla, n = 5)

#-------------------------------------------------------------------------------

# top 5 de acuerdo a la cantidad de compañias
tabla.top5 <- tabla[1:5]

# top 5 con frecuencias relativas (proporciones)
tabla.top5.rel <- round(x = tabla.top5 / n, digits = 3)

# mas de la mitad de las FG2000 pertenecen a solo 5 paises
sum( tabla.top5.rel )

#-------------------------------------------------------------------------------

# grafico de barras
windows() # quartz()
barplot(height = tabla.top5.rel)

#-------------------------------------------------------------------------------

windows()
bp <- barplot(height = 100*tabla.top5.rel, names.arg = c("US", "JP", "CN", "UK", "SK"),
              col = "lightblue", border = "blue", xlab = "País", ylab = "Porcentaje", 
              main = "Países Top 5 Forbes Globe 2000", font.main = 4, ylim = c(0,30))
text(x = bp, y = 100*tabla.top5.rel, labels = paste0(round(100*tabla.top5.rel, 1), "%"), 
     cex = 1.5, pos = 3, col = "blue", font = 2) 

#-------------------------------------------------------------------------------

# mas informacion
?barplot

#-------------------------------------------------------------------------------

###########################
# variables cuantitativas #
###########################

# base de datos solo con variables cuantitativas
FG.cuanti <- FG[ , c("Sales", "Profits", "Assets", "Value")]

View(FG.cuanti)

#-------------------------------------------------------------------------------

# resumen multivariado
summary(FG.cuanti)

# varianza
var(Sales)

# desviacion estandar
sd(Sales)

sqrt(var(Sales))

# coeficiente de variacion
sd(Sales) / mean(Sales)

# coeficiente de variacion 
apply(X = FG.cuanti, MARGIN = 2, FUN = function(x) sd(x)/mean(x)*100)

#-------------------------------------------------------------------------------

# graficos: examinar la distribucion de una variable
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# diagramas de caja
b <- boxplot(x = Profits, horizontal = TRUE, boxwex = 0.5, cex = 0.8, 
             border = "blue", col = "lightblue", xlab = "Ganancias")
# histograma
h <- hist(x = Profits, freq = FALSE, nclass = 50, border = "blue", col = "lightblue", 
          xlab = "Ganancias", ylab = "Densidad", main = " ")
# titulo
title(main = "Ganancias Compañías Forbes Globe 2000", font.main = 4, outer = TRUE, line = -2)


# diagrama de caja
b$stats

# histograma
h$breaks

h$counts

h$density

#-------------------------------------------------------------------------------

# rango intercuartilico profits
q3.profits <- quantile(x = Profits, probs = 0.75)

q1.profits <- quantile(x = Profits, probs = 0.25)

RI.profits <- as.numeric( q3.profits - q1.profits )

#-------------------------------------------------------------------------------

# cantidad de datos atipicos superiores     
sum( as.numeric(Profits > q3.profits + 1.5 * RI.profits) )

round(100*208/n, 2)

#-------------------------------------------------------------------------------

# cantidad de datos extremos superiores
sum( as.numeric(Profits > q3.profits + 3.0 * RI.profits) )

round(100*125/n, 2)

#-------------------------------------------------------------------------------

# datos de compañias extremas superiores 
FG.extrem.sup <- FG[Profits > q3.profits + 3.0 * RI.profits, c("Company", "Country", "Profits")]

FG.extrem.sup[order(FG.extrem.sup$Profits, decreasing = TRUE), ]

View(FG.extrem.sup)

#-------------------------------------------------------------------------------

# base de datos sudamerica solo con variables cuantitativas
FG.cuanti.sura <- FG[Country %in% paises.sura , c("Sales", "Profits", "Assets", "Value")]

head(FG.cuanti.sura)

dim(FG.cuanti.sura)

#-------------------------------------------------------------------------------

# diagramas de caja compañias suramerica
windows()
boxplot(FG.cuanti.sura, horizontal = TRUE, boxwex = 0.5, ylim=c(0,100), cex = 0.8,
        border = c("darkgreen", "blue", "red", "purple4"),
        col = c("lavender", "lightblue", "mistyrose", "linen"), 
        xlab = "Billones de dolares", main = "Compañías Forbes Globe 2000")

#-------------------------------------------------------------------------------

# dispersograma o nube de puntos
# examinar la relacion entre variable cuantitativas
windows()
plot(x = Sales, y = Profits)


pen <- sum( ( Sales - mean(Sales) )*( Profits - mean(Profits)) ) / sum( (Sales - mean(Sales))^2 )

corte <- mean(Profits) - pen * mean(Sales)

windows()
plot(x = Sales, y = Profits, cex = 1.2, pch = 16, col = "darkgreen", 
     ylab = "Ganancias (billones US)", xlab = "Ventas (billones US)",
     main = "Ganancias frente a Ventas")
grid()
abline(a = corte, b = pen, col = "red", lty = 3, lwd = 2)

FG[Sales > 400, ]

#-------------------------------------------------------------------------------

# prisma de dispersogramas
windows()
pairs(FG.cuanti)


windows()
pairs(FG.cuanti, pch = 16, cex = 0.8, gap = 0, xaxt = "n", yaxt = "n", 
      col = "blue", labels = c("Ventas", "Ganancias", "Activos", "Valor")) 

#-------------------------------------------------------------------------------

library(threejs)

windows()
scatterplot3js(x = FG$Sales, 
               y = FG$Profits, 
               z = FG$Assets, size = 0.5)

#-------------------------------------------------------------------------------

# covarianza
# el signo de la covarianza indica el tipo relacion entre las variables
# positivo: directa
# negativo: inversa
# unidades mixtas
round(cov(Sales, Profits), 1)

# matriz de covarianza
matriz.cov <- round(cov(FG.cuanti), 1)

class(matriz.cov)
isSymmetric(matriz.cov)

#-------------------------------------------------------------------------------

# coeficiente de correlacion de Pearson
# cuantifica el grado de relacion lineal entre dos variables cuantitativas
# siempre toma valores entre -1 y 1
# entre mas cercano a  1 la relacion es directa y fuerte
# entre mas cercano a -1 la relacion es inversa y fuerte
# entre mas cercano a  0 la relacion es mas debil
# es adimensional (no tiene unidades)
round(cor(Sales, Profits), 2)

# matriz de correlacion
round(100*cor(FG.cuanti), 2)


#install.packages("corrplot")
library(corrplot)

M     <- cor(FG.cuanti)
p.mat <- cor.mtest(FG.cuanti)
col   <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

windows()
corrplot(M, method="color", col=col(200), type="upper", tl.cex = 0.9, diag = FALSE, addCoef.col = 1, number.cex = 0.9)

windows()
corrplot(M, type="upper", diag = FALSE)


#-------------------------------------------------------------------------------
# medidas de sesgo (forma)

# indice de Yule - Bowley
q1 <- quantile(Assets, probs = 0.25)  # 10.9
q2 <- quantile(Assets, probs = 0.50)  # 22.9 
q3 <- quantile(Assets, probs = 0.75)  # 52.4

( (q3 - q2) - (q2 - q1) ) / ( q3 - q1 )
# 0.4216867

# coeficiente de asimetria
sum( ( Assets - mean(Assets) )^3 ) / (n * sd(Assets)^3)
# 7.567657

sum( ( Profits - mean(Profits) )^3 ) / (n * sd(Profits)^3)

sum( ( Sales - mean(Sales) )^3 ) / (n * sd(Sales)^3)

## FUNCIONES ##

juan <- function(x) {
  y <- x+1
  return(y)
}

juan(1)
juan(2)

medidas.sesgo <- function(x, tipo = "AF", ancho = 0.5, color = "black", grafico = TRUE) 
{
  if (grafico == TRUE) {
    windows()
    boxplot(x = x, horizontal = TRUE, boxwex = ancho, col = color, border = color, cex = 0.5)
  }
  if (tipo == "YB") {
    q1 <- quantile(x, probs = 0.25)
    q2 <- quantile(x, probs = 0.50) 
    q3 <- quantile(x, probs = 0.75)
    y <- ( (q3 - q2) - (q2 - q1) ) / ( q3 - q1 )
    return (as.numeric(y))
  }
  if (tipo == "AF") {
    n <- length(x)  
    y <- sum( ( x - mean(x) )^3 ) / (n * sd(x)^3)
    return (as.numeric(y))
  }
  if ( tipo != "YB" & tipo != "AF") {
    cat("Medida no Disponible")
  }
}

medidas.sesgo(x = Assets, color = "red")

f <- function(x) {
  windows()
  boxplot(x = x, horizontal = TRUE)
}

f(x = Assets)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

##############
# Fin sesion #
##############