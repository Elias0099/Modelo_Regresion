Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.setenv(LANG = "spa")
library(tidyverse)
library(PerformanceAnalytics)
library(equatiomatic)
datos_idh <- read.csv("~/DataR/Mexico/idh_mpio_2000_2005.csv", header = TRUE) #ruta de acceso a los datos, 'header = TRUE' en caso de que el archivo cuente con nombres de las variables.
str(datos_idh)

##1 EXPLORACION DESCRIPTIVA DE DATOS

# Para el año 2000
idh_reg_2000 <- datos_idh %>%
  select(tasa_moralidad_infantil_2000,
         tasa_alfabetizacion_2000,
         tasa_asistencia_escolar_2000,
         usd_ppc_2000)

# Para el año 2005
idh_reg_2005 <- datos_idh %>%
  select(tasa_mortalidad_infantil_2005,
         tasa_alfabetizacion_2005,
         tasa_asistencia_escolar_2005,
         usd_ppc_2005)

# Correlación para el año 2000
chart.Correlation(idh_reg_2000,
                  histogram = TRUE,
                  method = "pearson")

# Correlación para el año 2005
chart.Correlation(idh_reg_2005,
                  histogram = TRUE,
                  method = "pearson")



if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
}

# Calcular la matriz de correlación para el año 2000
matriz_correlacion_2000 <- cor(idh_reg_2000, method = "pearson")

# Crear gráfico de la matriz de correlación para el año 2000
corrplot(matriz_correlacion_2000, method = "number", tl.col = "black", title = "Matriz de correlación (Año 2000)")

# Calcular la matriz de correlación para el año 2005
matriz_correlacion_2005 <- cor(idh_reg_2005, method = "pearson")

# Crear gráfico de la matriz de correlación para el año 2005
corrplot(matriz_correlacion_2005, method = "number", tl.col = "black", title = "Matriz de correlación (Año 2005)")


## 2. AJUSTE DEL MODELO

# Para el año 2005
m1_2005 <- lm(tasa_mortalidad_infantil_2005 ~  
                usd_ppc_2005 +                   
                tasa_asistencia_escolar_2005 +   
                tasa_alfabetizacion_2005,        
              data = datos_idh)

# Para el año 2000
m1_2000 <- lm(tasa_moralidad_infantil_2000 ~  
                usd_ppc_2000 +                   
                tasa_asistencia_escolar_2000 +   
                tasa_alfabetizacion_2000,        
              data = datos_idh)

## 3. COMPROBACIÓN DE LOS SUPUESTOS DEL MODELO

# Histograma y prueba de normalidad de los residuos para el modelo del año 2005
hist(resid(m1_2005))
shapiro.test(resid(m1_2005))

# Histograma y prueba de normalidad de los residuos para el modelo del año 2000
hist(resid(m1_2000))
shapiro.test(resid(m1_2000))

# Gráfico de los residuos frente a los valores ajustados para el modelo del año 2005
plot(fitted.values(m1_2005), resid(m1_2005))
abline(h = 0, lty = 2)

# Gráfico de los residuos frente a los valores ajustados para el modelo del año 2000
plot(fitted.values(m1_2000), resid(m1_2000))
abline(h = 0, lty = 2)

## 4. INTERPRETACIÓN DE LOS MODELOS

# Resumen del modelo para el año 2005
options(scipen = 999)
summary(m1_2005)

# Resumen del modelo para el año 2000
options(scipen = 999)
summary(m1_2000)

# Instalar las librerías necesarias si no están instaladas
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")

# Cargar las librerías
library(broom)
library(dplyr)

# Extraer los coeficientes del modelo usando broom
model_summary <- tidy(m1_2005)

# Crear una función para formatear la ecuación
create_equation <- function(model_summary) {
  intercept <- model_summary$estimate[model_summary$term == "(Intercept)"]
  terms <- model_summary %>% filter(term != "(Intercept)")
  
  # Inicializar la ecuación con el término independiente
  equation <- paste0("tasa_mortalidad_infantil_2005 = ", sprintf("%.4f", intercept))
  
  for (i in 1:nrow(terms)) {
    term <- terms$term[i]
    estimate <- terms$estimate[i]
    
    # Añadir los términos de las variables con sus coeficientes
    if (estimate >= 0) {
      equation <- paste0(equation, " + ", sprintf("%.8f", estimate), "(", term, ")")
    } else {
      equation <- paste0(equation, " - ", sprintf("%.8f", abs(estimate)), "(", term, ")")
    }
  }
  
  return(equation)
}

# Generar la ecuación formateada
equation <- create_equation(model_summary)
cat(equation)
summary(m1_2005)

# Paso 2: Seleccionar el 20% de las filas aleatoriamente
set.seed(123)  # Fijar semilla para reproducibilidad
sample_size <- floor(0.2 * nrow(datos_idh))  # Calcular el tamaño de la muestra (20% del total)
sample_indices <- sample(seq_len(nrow(datos_idh)), size = sample_size)  # Obtener índices de la muestra

# Crear un subconjunto de datos con los índices seleccionados
datos_submuestra <- datos_idh[sample_indices, ]

# Paso 3: Entrenar el modelo con el subconjunto de datos
ejemplo20 <- lm(tasa_mortalidad_infantil_2005 ~  
                usd_ppc_2005 +                   
                tasa_asistencia_escolar_2005 +   
                tasa_alfabetizacion_2005,        
              data = datos_submuestra)

# Mostrar el resumen del modelo
summary(ejemplo20)

# Paso 1: Cargar los datos (esto es solo un ejemplo, asegúrate de que tus datos ya están cargados)
# datos_idh <- read.csv("ruta/a/tus/datos.csv")

# Paso 2: Seleccionar el 80% de las filas aleatoriamente
set.seed(123)  # Fijar semilla para reproducibilidad
sample_size2 <- floor(0.8 * nrow(datos_idh))  # Calcular el tamaño de la muestra (80% del total)
sample_indices2 <- sample(seq_len(nrow(datos_idh)), size = sample_size2)  # Obtener índices de la muestra

# Crear un subconjunto de datos con los índices seleccionados
datos_submuestra2 <- datos_idh[sample_indices2, ]

# Paso 3: Entrenar el modelo con el subconjunto de datos
ejemplo80 <- lm(tasa_mortalidad_infantil_2005 ~  
                usd_ppc_2005 +                   
                tasa_asistencia_escolar_2005 +   
                tasa_alfabetizacion_2005,        
              data = datos_submuestra2)

# Mostrar el resumen del modelo
summary(ejemplo80)



##################
calcular_variable_dependiente <- function(model_summary, x1, x2, x3) {
  coeficientes <- model_summary$coefficients
  beta0 <- coeficientes[1, 1]
  beta1 <- coeficientes[2, 1]
  beta2 <- coeficientes[3, 1]
  beta3 <- coeficientes[4, 1]
  
  y_estimado <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  return(y_estimado)
}

# Utilizar la función con el resumen del modelo y valores de las variables predictoras
modelo_summary <- summary(m1_2005)
modelo_summary_20 <- summary(ejemplo20)
modelo_summary_80 <- summary(ejemplo80)

# Valores de las variables predictoras
x1 <- 20
x2 <- 30
x3 <- 40


# Calcular la variable dependiente estimada DEL 100%
y_estimado <- calcular_variable_dependiente(modelo_summary, x1, x2, x3)
print(y_estimado)

# Calcular la variable dependiente estimada para el 20%
y_estimado_20 <- calcular_variable_dependiente(modelo_summary_20, x1, x2, x3)
print(y_estimado_20)

# Calcular la variable dependiente estimada para el 80%
y_estimado_80 <- calcular_variable_dependiente(modelo_summary_80, x1, x2, x3)
print(y_estimado_80)






























