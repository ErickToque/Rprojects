############################################################
# Cap. IIa - Datos cualitativos (Estado civil)
# Objetivo: frecuencias, proporciones y gráficos de barras
# Fuente: SUSALUD (CUESTIONARIO 02 - CAPITULOS.sav)
# Autor: Dante Baldeon Molleda | Fecha: 20/08/2025
############################################################

## 0) Paquetes (solo lo necesario)
#install.packages("haven") # si aún no lo tienes
library(haven)  # leer .sav manteniendo etiquetas


## 1) Carpetas de salida
getwd()
dir.create("output",  showWarnings = FALSE, recursive = TRUE)
dir.create("figuras", showWarnings = FALSE, recursive = TRUE)


## 2) Cargar base
url_susalud = "http://portal.susalud.gob.pe/wp-content/uploads/archivo/base-de-datos/2015/CUESTIONARIO%2002%20-%20CAPITULOS.sav"
salud.personal = read_sav(url_susalud)

## 3) Explorar etiquetas clave (útil para clase)
attr(salud.personal$C2P1, "label")   # pregunta ocupación
attr(salud.personal$C2P1, "labels")  # niveles ocupación
attr(salud.personal$C2P5, "label")   # pregunta estado civil
attr(salud.personal$C2P5, "labels")  # niveles estado civil


## 4) Filtrar solo médicos (como en el PPT)
salud.medicos = salud.personal[salud.personal$C2P1 == 1, ]

## 5) Variable de interés: Estado civil (categórica nominal)
# NOTA: no consideramos NA; la base está completa.
est_civil = as_factor(salud.medicos$C2P5, levels = "labels")
est_civil


## 6) Tablas: frecuencia, proporción y porcentaje
freq = table(est_civil)                 # frecuencia por categoría
prop = prop.table(freq)                 # proporción (suma = 1)
pct  = 100 * prop                       # porcentaje
# Guardar tabla limpia para el informe/Beamer
tabla_ec = data.frame(
  Estado_civil = names(freq),
  Frecuencia   = as.integer(freq),
  Proporcion   = as.numeric(round(prop, 4)),
  Porcentaje   = sprintf("%.1f", round(pct, 1)))
write.csv(tabla_ec, "output/tabla_estado_civil_medicos.csv", row.names = FALSE)


## ========================
## 7) FIGURAS (base R)
## ========================

# Vector de nombres (etiquetas limpias para el eje X)
nombres_ec = names(freq)

# --- Figura 1: barras con proporción (eje 0–1)
png("figuras/Figura1.png", width = 1200, height = 800, res = 140)
tab = prop
barplot(tab,
        ylim = c(0, 1),
        names.arg = nombres_ec,
        ylab = "Proporción",
        xlab = "Estado civil",
        main = "Distribución de estado civil en médicos",
        col = "grey80", border = "grey35")
dev.off()


# --- Figura 2: barras con conteo encima (proporción en eje)
png("figuras/Figura2.png", width = 1200, height = 800, res = 140)
tab = prop
bar.x = barplot(tab,
                ylim = c(0, 1),
                names.arg = nombres_ec,
                ylab = "Proporción",
                xlab = "Estado civil",
                main = "Distribución de estado civil en médicos",
                col = "grey80", border = "grey35")
text(x = bar.x, y = tab + 0.03, labels = as.integer(freq), cex = 0.8)
dev.off()

# --- Figura 3: barras sin etiquetas en eje X + leyenda por color
png("figuras/Figura3.png", width = 1200, height = 800, res = 140)
tab = prop
bar.x = barplot(tab,
                ylim = c(0, 1),
                names.arg = rep("", length(tab)),
                ylab = "Proporción",
                main = "Distribución de estado civil en médicos",
                col = 1:length(tab), border = "grey35")
text(x = bar.x, y = tab + 0.03, labels = as.integer(freq), cex = 0.8)
legend("topright",
       title  = "Estado civil",
       legend = nombres_ec,
       col    = 1:length(tab),
       pch    = 15, bty = "n")
dev.off()

# --- Figura 4: barras con porcentaje en eje (0–100) y conteo encima
png("figuras/Figura4.png", width = 1200, height = 800, res = 140)
tab = 100 * prop
bar.x = barplot(tab,
                ylim = c(0, 100),
                names.arg = nombres_ec,
                ylab = "Porcentaje (%)",
                xlab = "Estado civil",
                main = "Distribución de estado civil en médicos",
                col = "grey80", border = "grey35")
text(x = bar.x, y = tab + 3, labels = as.integer(freq), cex = 0.8)
dev.off()
