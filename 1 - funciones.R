################################################################################
############################### Funciones ######################################
################################################################################

# defincion de una funcion

f <- function(juan) {
        g <- juan + 1 # sumar uno
        return(g) 
}

class(f)

f(juan = 2)

f(2)

f(-3)

# observe que g no hace parte del Global Enviroment, solamente 'f' lo es

# grafico de f
windows() # quartz
curve(expr = f, from = -3, to = 3, col = "blue", xlab = "Variable independiente", 
      ylab = "Variable dependiente", main = "Gráfico de f", lwd = 2, lty = 2)
grid()

# simular un conjunto de datos 
set.seed(1234)
x <- rnorm(n = 1000, mean = 5, sd = 1)

# histograma
windows()
hist(x, density = 10, col = "red")

# opcion 1
f <- function(datos, color, d) 
{
        # datos : conjunto datos
        # color : color del grafico
        # d     : cifras decimales
        # 1. calcular valores
	mediana  <- round(median(datos), d)
	promedio <- round(mean(datos), d)
	minimo   <- round(min(datos), d)
	maximo   <- round(max(datos), d)
	# 2. hacer el grafico
	windows()
	hist(datos, col = color, density = 10)
	# 3. retorno
	return(c(mediana, promedio, minimo, maximo))
}

# opcion 2
f <- function(juan) 
{
	mediana  <- median(juan)
	promedio <- mean(juan)
	minimo   <- min(juan)
	maximo   <- max(juan)
 	salida   <- c(mediana, promedio, minimo, maximo)
	names(salida) <- c("Mediana", "Promedio", "Mínimo", "Máximo")
	return(salida)
}

# opcion 3
f <- function(x) 
{
	mediana  <- median(x)
	promedio <- mean(x)
	minimo   <- min(x)
	maximo   <- max(x)
	cat("El promedio es:", promedio, "\n")
	cat("La mediana  es:", mediana,  "\n")
	cat("El mínimo   es:", minimo,   "\n")
	cat("El máximo   es:", maximo,   "\n")
	
	
	
}

#-------------------------------------------------------------------------------

# Ejemplo

f <- function(juan) {
        g <- juan + 1 # sumar uno
        return(g) 
}

f2 <- function(x) {
        v1 <- x^2
        return(v1)
}

f3 <- function(x) {
        v1 <- x^3
        return(v1)
}

# grafico de f y f2
windows()
curve(expr = f2, from = -3, to = 3, col = "red", lty = 1, ylim = c(-4, 4), lwd = 2,
      xlab = "Eje x", ylab = "Eje y", main = "Gráfico de f y f2")
curve(expr = f, col = "blue", lty = 2, lwd = 2, add = TRUE)
curve(expr = f3, col = "purple", lty = 3, lwd = 2, add = TRUE)
grid()


windows()
curve(expr = f2, from = -3, to = 3, ylim = c(-3, 3), col = "red", lty = 1,
      xlab = "Eje x", ylab = "Eje y", main = "Gráfico de f y f2")
curve(expr = f,  col = "blue", lty = 2, add = TRUE)
grid()
abline(h = 0, v = 0, col = "black")


# optimizacion
optimise(f = f2, interval = c(-3, 3), maximum = FALSE)

#-------------------------------------------------------------------------------

# Ejemplo

g <- function(a, b) {
        v1 <- a^2 + b^2
        return(v1)
}

class(g)

g(0, 0)

g(-1, 2)

g(b = 2, a = -1)

# grafico 3D
x <- seq(from = -10, to = 10, length = 25)
y <- seq(from = -10, to = 10, length = 25)
z <- outer(x, y, g)


windows()
persp(x = x, y = y, z = z, theta = 45, phi = 45, expand = 1)

windows()
persp(x = x, y = y, z = z, theta = 30, phi = 45, expand = 1, 
      col = "mistyrose")


library(lattice)

windows()
wireframe(x = z)


windows()
wireframe(x = z, drape = TRUE, col.regions = rainbow(100), 
          xlab = "Eje x", ylab = "Eje y")

# grafico de contorno

windows()
contour(z)

windows()
contour(z, nlevels = 20, col = "blue", xlab = "Eje x", ylab = "Eje y", main = "Gráfico de contorno")

#-------------------------------------------------------------------------------

# Ejemplo

h <- function(x, y) {
        v1 <- x + y
        v2 <- x * y
        salida <- c(v1, v2)
        names(salida) <- c("Suma", "Producto")
        return( salida )
}

class(f)

f(x = 2, y = 5)

f(2, 3)

#-------------------------------------------------------------------------------