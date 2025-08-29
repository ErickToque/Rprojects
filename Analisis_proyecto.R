#Cargar dataset y libreria
load(file.choose())
#install.packages('ggplot2')
#install.packages("moments")
library(moments)
library(dplyr)
library(ggplot2)
##################################################
#Analisis general de dataset
##################################################
head(datos)
str(datos)      # estructura
summary(datos)  # resumen estadístico
names(datos)    # nombres de columnas

##################################################
#Variables numéricas en tu dataset
##################################################
#P314 → Ingreso mensual
#P210_A → Edad
#P601 → Cantidad de libros en el hogar
#P418x → Gasto en libros

##################################################
# Estadisticas Variables numéricas
##################################################

# Variables numéricas
num_vars <- c("P314", "P210_A", "P601", "P418x")

# Función para resumen estadístico extendido
summary_numeric_ext <- function(var){
  x <- datos[[var]]
  x <- x[!is.na(x)]  # eliminar NA
  data.frame(
    Variable = var,
    Media = mean(x),
    Mediana = median(x),
    SD = sd(x),
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75),
    Max = max(x),
    Rango = max(x) - min(x),
    IQR = IQR(x),
    Asimetria = skewness(x),
    Curtosis = kurtosis(x),
    Coef_Variacion = sd(x)/mean(x)
  )
}

# Aplicar a todas las variables numéricas
tabla_num_ext <- do.call(rbind, lapply(num_vars, summary_numeric_ext))
tabla_num_ext
##################################################
#Graficas de variables numericas
##################################################
# --- Ingreso con histograma
ggplot(datos, aes(x = P314)) +
  geom_histogram(binwidth = 500, fill="skyblue", color="black") +
  labs(title="Distribución de Ingresos", 
       x="Ingreso mensual", y="Frecuencia")
# --- Ingreso con boxplot
ggplot(datos, aes(y = P314)) +
  geom_boxplot(fill="tomato") +
  labs(title="Boxplot de Ingresos", y="Ingreso mensual")

# --- Edad
ggplot(datos, aes(x = P210_A)) +
  geom_histogram(binwidth = 5, fill="lightgreen", color="black") +
  labs(title="Distribución de la Edad", 
       x="Edad", y="Frecuencia")

# --- Cantidad de Libros en el hogar
ggplot(datos, aes(x = P601)) +
  geom_histogram(binwidth = 20, fill="orange", color="black") +
  labs(title="Distribución de Libros en el Hogar", 
       x="Número de libros", y="Frecuencia")

# --- Gasto en libros
ggplot(datos, aes(x = P418x)) +
  geom_histogram(binwidth = 50, fill="purple", color="black") +
  labs(title="Distribución del Gasto en Libros", 
       x="Gasto en libros (S/.)", y="Frecuencia")
##################################################
#Variables categóricas en tu dataset
##################################################

#ESTRATOSOCIO → Estrato socioeconómico
#P602_10 → Internet en el hogar (Sí/No)
#P209 → Sexo
#P212 → Estado civil
#P306 → Nivel educativo
#P415 → Consiguió libros (Sí/No)
#P435 → Asistió a ferias/festivales (Sí/No)
##################################################
#Estadistica de variables categóricas
##################################################
# Variables categóricas
cat_vars <- c("ESTRATOSOCIO","P602_10","P209","P212","P306","P415","P435")

# Función para tabla de frecuencia
tabla_categorica <- function(var){
  freq <- table(datos[[var]])
  prop <- round(prop.table(freq)*100,2)
  data.frame(
    Variable = var,
    Categoria = names(freq),
    Frecuencia = as.vector(freq),
    Porcentaje = as.vector(prop)
  )
}

# Aplicar a todas las variables categóricas
tabla_cat <- do.call(rbind, lapply(cat_vars, tabla_categorica))
tabla_cat
##################################################
#Graficas de variables categóricas
##################################################
# --- Estrato socioeconómico
ggplot(datos, aes(x = as_factor(ESTRATOSOCIO))) +
  geom_bar(fill="skyblue") +
  labs(title="Distribución por Estrato Socioeconómico", 
       x="Estrato", y="Frecuencia")

# --- Internet en el hogar
ggplot(datos, aes(x = as_factor(P602_10))) +
  geom_bar(fill="lightgreen") +
  labs(title="Conexión a Internet en el Hogar", 
       x="Respuesta", y="Frecuencia")

# --- Sexo
ggplot(datos, aes(x = as_factor(P209))) +
  geom_bar(fill="orange") +
  labs(title="Distribución por Sexo", 
       x="Sexo", y="Frecuencia")

# --- Estado civil
ggplot(datos, aes(x = as_factor(P212))) +
  geom_bar(fill="violet") +
  labs(title="Estado Civil", 
       x="Estado civil", y="Frecuencia")

# --- Nivel educativo
ggplot(datos, aes(x = as_factor(P306))) +
  geom_bar(fill="tomato") +
  labs(title="Nivel Educativo", 
       x="Educación", y="Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Nivel educativo con leyenda
niveles <- levels(as_factor(datos$P306))
niveles
ggplot(datos, aes(x = as_factor(P306), fill = as_factor(P306))) +
  geom_bar(show.legend = TRUE) +
  labs(title="Nivel Educativo", x="Categoría (número)", y="Frecuencia", fill="Nivel educativo") +
  scale_x_discrete(labels = seq_along(niveles)) +  # Solo muestra 1, 2, 3...
  theme_minimal()

# --- Consiguió libros
ggplot(datos, aes(x = as_factor(P415))) +
  geom_bar(fill="cyan") +
  labs(title="Consiguió libros en los últimos 12 meses", 
       x="Respuesta", y="Frecuencia")

# --- Asistencia a ferias/festivales
ggplot(datos, aes(x = as_factor(P435))) +
  geom_bar(fill="brown") +
  labs(title="Asistencia a ferias o festivales", 
       x="Respuesta", y="Frecuencia")



##################################################
#Preparación de variables
##################################################

# Nivel educativo simplificado: completo vs incompleto
datos <- datos %>%
  mutate(
    NivelEduSimpl = case_when(
      P306 %in% c(1,2,4,6,7,9) ~ "Incompleto",
      P306 %in% c(3,5,8,10,11) ~ "Completo",
      TRUE ~ NA_character_
    ),
    # Estado civil simplificado
    EstadoCivilSimpl = case_when(
      P212 %in% c(1,3) ~ "Conviviente/Casado",
      P212 %in% c(2,4,5) ~ "Separado/Viudo/Divorciado",
      P212 == 6 ~ "Soltero/a",
      TRUE ~ NA_character_
    ),
    # Gasto en libros binario (0 = no gasto, 1 = gasto)
    GastoLibrosBin = ifelse(P418x > 0, 1, 0)
  )


##################################################
#Bivariado: variables sociodemográficas vs ingreso
##################################################

#Ingreso vs Estrato socioeconómico
ggplot(datos, aes(x = as_factor(ESTRATOSOCIO), y = P314)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Ingreso mensual por Estrato Socioeconómico",
       x = "Estrato", y = "Ingreso mensual (S/.)")

#Ingreso vs Nivel educativo simplificado
ggplot(datos, aes(x = NivelEduSimpl, y = P314)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Ingreso mensual según nivel educativo",
       x = "Nivel educativo", y = "Ingreso mensual (S/.)")
#Ingreso vs Sexo
ggplot(datos, aes(x = as_factor(P209), y = P314)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Ingreso mensual por Sexo",
       x = "Sexo", y = "Ingreso mensual (S/.)")
#Ingreso vs Edad (numérica)
ggplot(datos, aes(x = P210_A, y = P314)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Ingreso mensual vs Edad",
       x = "Edad", y = "Ingreso mensual (S/.)")
##################################################
#Bivariado: variables relacionadas con lectura
##################################################
#Ingreso vs Número de libros en el hogar
ggplot(datos, aes(x = P601, y = P314)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Ingreso vs Número de libros en el hogar",
       x = "Número de libros", y = "Ingreso mensual")
#Ingreso vs Gasto en libros
ggplot(datos, aes(x = P418x, y = P314)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Ingreso vs Gasto en libros",
       x = "Gasto en libros (S/.)", y = "Ingreso mensual")
#Ingreso vs Asistencia a ferias
ggplot(datos, aes(x = as_factor(P435), y = P314)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Ingreso vs Asistencia a ferias/festivales",
       x = "Asistió", y = "Ingreso mensual")

