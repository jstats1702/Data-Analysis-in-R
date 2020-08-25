#################################
# Caso : Stock Market Liquidity #
#################################

#-------------------------------------------------------------------------------
# La decisión de un inversor de comprar un stock generalmente se toma teniendo 
# en cuenta una serie de criterios:
# - Rendimiento esperado. 
# - Riesgo del stock (variabilidad de los returns). 
# - Duración promedio de la inversión. 
# - Posibilidad de vender el stock (liquidez del stock). 
#       * Cuanto más "líquido" es el stock, más fácil es venderlo. 
#       * Para medir la liquidez, es posible emplear el número de shares (participaciones) 
#         negociadas durante un período específico de tiempo (VOLUMEN). 
#
# Nota:
#   A "share" indicates a portion of ownership in a particular company. Stocks are 
#   divided into shares: a share is the smallest denomination of a company's stock. 
#   Each unit of stock is a share in a company. So each share of stock is equal to 
#   a piece of one particular company's ownership.
# 
# Objetivo: 
# Evaluar el impacto de algunas características financieras sobre el VOLUMEN.
#
# Descripcion de la base de datos:
# 
# Trading activity variables:
# - VOLUME  Número de shares negociadas en un periodo de tres meses (en millones de shares).
# - AVGT    Tiempo promedio entre transacciones (en minutos).
# - NTRAN   Número total de transacciones en un periodo de tres meses.
#
# Firm size variables:
# - PRICE   Precio del stock en el momento de la apertura (en dólares).
# - SHARE   Cantidad de acciones en circulación (en millones de shares).
# - VALUE   Valor en el mercado, se obtiene como el producto de PRICE y SHARE.
#
# Financial leverage varaibles:
# - DEBEQ   Proporción de deuda a capital (apalancamiento financiero).
#           Cantidad de deuda que una compañía está utilizando para financiar 
#           sus activos en relación con el valor del capital de los accionistas.
#
# Otras varaibles:
# - TIC     Símbolo de la compañía.
# - COMPANY Nombre de la comañia.
#
# Fuente: Regression Modeling with Actuarial and Financial Applications (Frees, 2009)
#         
#-------------------------------------------------------------------------------

# importar el conjunto de datos
liquid <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Finanzas Algoritmicas 35/sesion 4/liquidity.csv")

attach(liquid)

# variables
X <- liquid[ , c("AVGT","NTRAN","PRICE","SHARE","VALUE","DEBEQ", "VOLUME")]

# resumen
summary(X)

# matriz de correlacion
round(cor(X), 2)

# prisma de dispersogramas   
windows()
pairs(VOLUME ~ AVGT + NTRAN + PRICE + SHARE + VALUE + DEBEQ, cex = 0.8, gap = 0, col = "blue")

# - no hay una relacion aparente entre el apalancamiento financiero y el resto de
#   las variables
# - relacion fuerte entre VOLUME y el tamaño de la firma (SHARE y VALUE)
# - todas las varaibles de actimidad comercial (VOLUME, AVGT, NTRAN) tienen
#   una relacion fuerte.
# - AVGT tiene una relacion inversa con VOL
# - NTRAN tiene una relacion inversa con AVGT

# MODELO 1
model1 <- lm(VOLUME ~ NTRAN, data = liquid)
summary(model1)

liquid$residuals.model1 <- residuals(model1)
cor(liquid[,c("residuals.model1","AVGT","DEBEQ","PRICE","SHARE","VALUE")])[1,]

# reiduales: valores de VOL habiendo controlado por el efecto de NTRAN
# posiblemente hay relacion de AVGT en los residuales

# MODELO 2
model2 <- lm(VOLUME ~ NTRAN + AVGT, data=liquid)
summary(model2)

# la desv estandar del erro disminuyo
# R2 aumento

liquid$residuals.model2 <- residuals(model2)
cor(liquid[,c("residuals.model2","DEBEQ","PRICE","SHARE","VALUE")])[1,]

# no parece haber mas informacio en otras variables

layout(matrix(c(1,2,3,4,5,6,7,8,9,10),byrow=TRUE,ncol=5))
par("oma"=c(3,3,3,3),"mai"=c(0,0,0.1,0))
plot.new()
hist(liquid$PRICE,breaks=18,main="PRICE",xaxt="n",yaxt="n")
hist(liquid$SHARE,breaks=12,main="SHARE",xaxt="n",yaxt="n")
hist(liquid$VALUE,breaks=12,main="VALUE",xaxt="n",yaxt="n")
hist(liquid$DEBEQ,breaks=12,main="DEBEQ",xaxt="n",yaxt="n")
hist(liquid$residuals.model2,breaks=20,main="Residuals",xaxt="n",yaxt="n")

plot(liquid$PRICE,liquid$residuals.model2,xaxt="n",yaxt="n",xlab="",ylab="")
plot(liquid$SHARE,liquid$residuals.model2,xaxt="n",yaxt="n",xlab="",ylab="")
plot(liquid$VALUE,liquid$residuals.model2,xaxt="n",yaxt="n",xlab="",ylab="")
plot(liquid$DEBEQ,liquid$residuals.model2,xaxt="n",yaxt="n",xlab="",ylab="")


# diagnosticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(model2)
