##################################################
# Ejemplo 1, Analisis de Componentes Principales #
##################################################

#-------------------------------------------------------------------------------
# Descripcion de la base de datos:

# Investment recomendation data (S&P Dow Jones, 2011).
# Porcentaje de inversion recomendada en:
# - USst : stocks en US
# - Fst  : stocks en paises desarrollados fuera de US
# - Dst  : stocks en paises en desarrollo
# - USb  : bonds en US
# - Fb   : bonds en paises desarrollados fuera de US
# - Db   : bonds en paises en desarrollo
# - Alt  : inversiones alternativas
# - Cash : inversiones en efectivo
#
# Conjunto de porcentajes recomendados en cada uno de ocho de diferentes tipos 
# de inversi�n (Stocks [acciones]/Bonds [bonos]/Alternatives [Alternativas])
# por parte de empresas importantes de gesti�n financiera a principios de 2011.
#
# Cuando una empresa emite Stocks, est� vendiendo una parte de s� misma a 
# cambio de efectivo. Cuando una entidad emite un bono, est� adquiriendo una 
# deuda con el acuerdo de pagar intereses por el uso del dinero.
#
# Las inversiones alternativas incluyen arrendamientos, asociaciones de petr�leo 
# y gas, propiedad inmobiliaria, metales preciosos e inversiones similares.
#
# El efectivo incluye inversiones a corto plazo, como el mercado monetario,
# dep�sitos bancarios y certificados de dep�sito.
#
# Decidir sobre la combinaci�n adecuada de acciones y bonos en su cartera es una 
# funci�n de su horizonte temporal, tolerancia al riesgo y objetivos de inversi�n.
#
# Para mass informaci�n ver Zelterman, p. 11.
# Zelterman, D. (2015). Applied Multivariate Statistics with R. Springer.
#-------------------------------------------------------------------------------

# importar data
invest <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/Finanzas Algoritmicas 35/sesion 3/investment.txt", row.names=1)
# visualizar data
View(invest)

n <- nrow(invest)  # numero de individuos, 27

p <- ncol(invest)  # numero de variables, 8

#############################
### Analisis exploratorio ###
#############################

# estadisticas univariadas
round( colMeans(invest), 2)

round( apply(X = invest, MARGIN = 2, FUN = function(x) 100 * sd(x)/mean(x)), 2)

# matriz de correlacion
round(cor(invest), 2)

# dispersogramas
windows()
pairs(x = invest, lwd = 1, pch = 16, gap = 0, xaxt = "n", yaxt = "n", col = "blue")

################################################################################
#################### Analisis de Componentes Principales #######################
################################################################################

#-------------------------------------------------------------------------------

# calculo de componentes principales
pc <- princomp(x = invest, cor = FALSE, scores = TRUE)

# resumen
summary(pc)

#-------------------------------------------------------------------------------

# coeficientes (cargas o pesos)
pc$loadings

#-------------------------------------------------------------------------------

# scree plot
windows()
screeplot(pc, col = "blue", pch = 16, type = "lines", cex = 2, lwd = 2, 
          cex.axis = 0.8, cex.lab = 0.8, main = " ")

#-------------------------------------------------------------------------------

# scores
head( pc$scores )

#-------------------------------------------------------------------------------

# biplot
windows()
biplot(pc, col = c(1,2), cex = c(1.5, 1.5),
       xlab = "Primera componente",
       ylab = "Segunda componente",
       main = "Biplot de asignaciones de inversi�n")
grid()

#-------------------------------------------------------------------------------

# Diferencia entre USst y Alt frente a la primera componente
windows()
plot(pc$scores[ , 1], invest[ , "USst"] - invest[ , "Alt"],
     pch = 16, col = "blue", cex = 1.1,
     ylab = "Diferencia entre USst y Alt",
     xlab = "Primera componente",
     main = "")
grid()

# correlacion
round(cor(pc$scores[ , 1], invest$USst - invest$Alt), 3)

#-------------------------------------------------------------------------------

# correlacion entre variables y componentes
cor.vars.comps <- round( cor( cbind(invest, pc$scores) ), 2 )

cor.vars.comps[1:8, 9:10]

#-------------------------------------------------------------------------------
# INTERPRETACION:
# - La primera componente representa la diferencia entre recomendaciones de 
#   inversion en stocks de US y las alternativas. 
# - La segunda componente representa principalmente el reciproco de los bonos de EU. 
# - Las cargas para los otros tipos de inversiones (efectivo o acciones y bonos 
#   extranjeros) no juegan un papel determinante en el an�lisis.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------