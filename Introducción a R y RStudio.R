############################################################
# Introducción a R y RStudio
# Objetivo: Comenzar a trabajar en RStudio
# Autor: Dante Baldeon Molleda | Fecha: 20/08/2025
############################################################

# Directorio de trabajo (WD)
getwd()                            # Verificar WD actual
# setwd("C:/ruta/a/tu/proyecto")   # Windows
# setwd("~/ruta/a/tu/proyecto")    # macOS/Linux


# Objetos y operaciones
x = 1:5
y = c(2, 4, 6, 8, 10)
mean(x)
sd(x) 
sum(y)


# Data frame mínimo
id = 1:10
sexo = c("F","M","F","M","F","M","F","M","F","M")
edad = c(18, 23, 20, 22, 24, 26, 22, 26, 21, 28)
df = data.frame(id, sexo, edad)
df
# Inspección
str(df); summary(df); head(df, 2)


# Guardar un CSV de ejemplo en /data
write.csv(df, "E:/PUCP/Estadistica/ejemplo_df.csv")


# Leer un CSV desde /data
df2 = read.csv("E:/PUCP/Estadistica/ejemplo_df.csv")

# Comprobaciones
dim(df2); names(df2); summary(df2)


tabla_resumen = aggregate(edad ~ sexo, data = df, FUN = mean)
print(tabla_resumen)

# Guardar la tabla como CSV en /output
write.csv(tabla_resumen,
          "E:/PUCP/Estadistica/tabla_resumen.csv",
          row.names = FALSE)


# Histograma y boxplot
hist(df2$edad, main = "Distribución de la edad", xlab = "Edad")
boxplot(edad ~ sexo, data = df2,
        main = "Edad por sexo", xlab = "Sexo", ylab = "Edad")


# Guardar un gráfico a archivo PNG (en /figs)
png("E:/PUCP/Estadistica/figs/hist_edad.png", width = 900, height = 600, res = 120)
hist(df2$edad, main = "Distribución de la edad", xlab = "Edad")


# Instalar (una sola vez) y cargar paquete(s)
# dplyr: manipulación y transformación de datos
install.packages("dplyr")   
library(dplyr)


# Listar y limpiar objetos (con precaución)
ls()
#rm(list = ls())   # Borra todo: usar solo si es necesario
