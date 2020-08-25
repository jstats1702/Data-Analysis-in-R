####################################
# Caso: Microestablecimientos 2016 #
####################################

#-------------------------------------------------------------------------------
# Información de los establecimientos con 9 o menos personas ocupadas en 
# comercio al por mayor, comercio al por menor y venta de motocicletas y sus 
# accesorios, y los talleres de mantenimiento y reparación de vehículos 
# automotores; todos los establecimientos de servicios, sin incluir los 
# financieros, el transporte, la educación pública y los establecimientos del 
# orden gubernamental (administración pública); y toda la microindustria según 
# la CIIU Rev. 4 A.C. Si estas actividades se desarrollan dentro de los hogares 
# también son objeto de medición. No se incluyen los puestos móviles.
#
# Información sobre las variables de actividad económica, personal ocupado,
# producción (industria), ventas (comercio) o ingresos (servicios), 
# organización jurídica, tiempo de funcionamiento, entre otras.
#
# Ver el archivo "Microestablecimientos.pdf" para más detalles.
#
# Fuente: http://microdatos.dane.gov.co/index.php/catalog/560/get_microdata
#-------------------------------------------------------------------------------

#################
# Base de datos #
#################
micro <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/Finanzas Algoritmicas 35/sesion 3/Microestablecimientos_2016.csv")
dim(micro)
# 33013 microestablecimientos
# 104 variables

#-------------------------------------------------------------------------------

################################################################
# Inferencia sobre la proporcion de microempresas de servicios #
################################################################

# el interes recae sobre la proporcion (porcentaje) de indiviuos que tienen
# una caracteristica objeto de estudio

sector <- micro$SECTOR
# 1 = industria
# 2 = comercio
# 3 = servicios

# hay datos faltantes?
any( is.na(sector) )
# para el sector de los microestablecimientos no hay datos faltantes

# tamaño de la muestra
n <- length(sector)
# 33013

# definir la variable de estudio
x <- rep(NA, n)
x[sector == 3] <- 1
x[sector != 3] <- 0

# estimar y hacer inferencia sobre la proporcion poblacional de microestablecimientos
# cuyo sector corresponde a servicios

#-------------------------------------------------------------------------------

### estadistica descriptiva

table(x)

tab.frec <- round( 100 * table(x)/n, 2)

windows()
barplot(height = tab.frec, names.arg = c("Otro","Servicios"), ylab = "Porcentaje", 
        col = "mistyrose", border = "red", main = "Microempresas de servicios")
text(x = c(0.7, 1.9), y = tab.frec -3, labels = tab.frec, cex = 1.25)

#-------------------------------------------------------------------------------

### inferencia

# estimacion puntual
theta.hat <- mean(x)
# 0.292824 : estimacion puntual de la proporcion de microestablecimientos de servicios

# intervalo de confianza
z.975 <- qnorm(p = 0.975, mean = 0, sd = 1)
# 1.959964

# margen de error
ME <- z.975 * sqrt( theta.hat * (1 - theta.hat) / n )
# 0.00490877 es decir 0.5% aproximadamente 

# intervalo de confianza
round( 100 * c(theta.hat - ME, theta.hat + ME), 2)

# INTERPRETACION
# con una confiabilidad del 95%, el porcentaje de microestablecimientos de servicios 
# se encuentra entre 28.79% y 29.77%.

# Usando confiabilidad del 99%
z.995 <- qnorm(p = 0.995)

ME <- z.995 * sqrt( theta.hat * (1 - theta.hat) / n )

round( 100 * c(theta.hat - ME, theta.hat + ME), 2)

#-------------------------------------------------------------------------------

###########################################################################
# Inferencia sobre la media de los ingresos de microempresas de servicios #
###########################################################################

ingresos <- micro$P958

# hay datos faltantes?
any( is.na(ingresos) )
# no hay ingresos faltantes

# definir la variable de estudio
x <- ingresos[sector == 3]

n <- length(x)

#-------------------------------------------------------------------------------

### estadistica descriptiva

summary(x)
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 15000   1800000   3900000   6777070   8000000 300000000 

# coeficiente de variacion
100 * sd(x) / mean(x) 
# dado que el CV es 171.534%, se tiene que la variabilidad de los ingresos de los
# microestablecimientos es muy grande respecto al ingreso promedio.

windows()
boxplot(x, horizontal = TRUE, col = "mistyrose", border = "red", cex = 0.5, 
        boxwex = 0.5, xlab = "Ingresos", ylab = " ", 
        main = "Distribución ingresos microempresas de servicios")

#-------------------------------------------------------------------------------

### inferencia

# estimacion puntual
xbar <- mean(x)  # 6777070 valor promedio muestral de los ingresos
s    <- sd(x)    # 11624979 valor de la desv estandar muestral de los ingresos

# se estima que el ingreso promedio de los microestablecimientos es de $6,777,070

# intervalo de confianza
z.975 <- qnorm(p = 0.975)
# 1.959964

# margen de error
ME <- z.975 * s / sqrt( n )
# 231736.5
# el margen de error de la estimacion es de $231,736.5 con una confiabilidad del 95%

# intervalo de confianza
round( c( xbar - ME, xbar + ME), 2)
# ( $6,545,334, $7,008,806 )
# con una confiabilidad del 95%, se concluye que el ingreso medio de los 
# microestablecimientos de servicios se encuentra entre $6,545,334 y $7,008,806.

# Confiabilidad del 99%

z.995 <- qnorm(p = 0.995)

ME <- z.995 * s / sqrt( n )

round( c( xbar - ME, xbar + ME), 2)



#-------------------------------------------------------------------------------

#########################################################################################################
# Inferencia sobre la diferencia de medias de los ingresos de microempresas de servicios e industriales #
#########################################################################################################

ingresos <- micro$P958

# hay datos faltantes?
any( is.na( ingresos ) )

# definir las variables de estudio
x  <- ingresos[sector == 3]  # microestablecimientos de servicios
y  <- ingresos[sector == 1]  # microestablecimientos industruales
n1 <- length(x)  # 9667 microestablecimientos de servicios
n2 <- length(y)  # 3260 microestablecimientos de industria

#-------------------------------------------------------------------------------

### estadistica descriptiva

summary(x)
summary(y)

# coeficientes de variacion
100 * sd(x) / mean(x) # 171.534%
100 * sd(y) / mean(y) # 186.8542%
# alta variabilidad de los ingresos de los microestablecimientos en ambos sectores

windows()
boxplot(formula = ingresos ~ sector, horizontal = TRUE, boxwex = 0.3, cex = 0.5,
        main = " ", xlab = "Ingresos", ylab = "Sector",
        col = c("lightblue","lightgreen","lightgray"), 
        border = c("darkblue","darkgreen","gray"), 
        names = c("Industria","Comercio","Servicios") )
legend("topright", legend = c("Industria","Comercio","Servicios"), bg = "white",
       fill = c("lightblue","lightgreen","lightgray"), 
       border = c("darkblue","darkgreen","gray"))


#-------------------------------------------------------------------------------

### inferencia

# estimacion puntual
xbar <- mean(x)
ybar <- mean(y)
s1   <- sd(x) 
s2   <- sd(y) 

xbar - ybar
# en promedio, se estima que los ingrsos de los microestablecimientos de industria
# sobrepasan en $869,099.3 los ingresos de los microestablecimientos del sector de servicios


# intervalo de confianza
z.975 <- qnorm(p = 0.975)

# margen de error
ME <- z.975 * sqrt( s1^2/n1 + s2^2/n2 )
# el margen de error es de 542,432.9

# intervalo de confianza par ala diferencia de medias 
round( c( xbar - ybar - ME, xbar - ybar + ME), 2)
# con una confiabilidad del 95%, se concluye que el ingreso medio de los microestablecimientos
# de industria es significativamente mayor que el ingreso medio de los microestablecimientos
# de servicios

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------