################################################################
# CAP IIb - Datos cuantitativos: Tendencia central
# Objetivo: media, mediana, cuartiles y percentiles de Y (Nm/rad)
# Fuente: trunk-control.csv (seleccionar con file.choose())
# Autor: Dante Baldeon Molleda | Fecha: 25/08/2025
################################################################

# Carga de datos
datos.control <- read.csv(file.choose())
cat("Dimensiones (filas x columnas):", paste(dim(datos.control), collapse = "x"), "\n")
head(datos.control)

# Ensayo 1
datos.0 <- datos.control[datos.control$time == 1, ]
y <- datos.0$Y  # si fuese necesario: y <- na.omit(y)

# Vector y media
cat("\nVector Y:\n") 
y

cat("\nMedia de Y:\n")
mean(y)

# Ordenamiento y mediana
cat("\nY ordenado (ascendente):\n")
y[order(y, decreasing = FALSE)]

cat("\nMediana de Y:\n")
median(y)

# Cuartiles y percentiles
cat("\nCuartiles (Q1, Q3):\n")
quantile(y, probs = c(0.25, 0.75))

cat("\nPercentiles 10 y 90:\n")
quantile(y, probs = c(0.10, 0.90))

cat("\nPercentiles 10, 25, 75 y 90:\n")
quantile(y, probs = c(0.10, 0.25, 0.75, 0.90))

