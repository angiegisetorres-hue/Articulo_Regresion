datos <- read.csv("datos.csv", header = TRUE, sep = ",", check.names = FALSE, encoding = "UTF-8")
# =====================================================================
# Script: anal_descrip.R
# Autor: Angie Gisela Palacios Torres
# Descripción: Análisis descriptivo de la base de datos del artículo 
#              sobre uso de celular, actividad física y rendimiento académico.
# =====================================================================

# 1. Leer la base de datos "datos" desde el repositorio
# Nota: 'check.names = FALSE' preserva los nombres originales de las columnas con tildes o símbolos.
datos <- read.csv("datos.csv", header = TRUE, sep = ",", check.names = FALSE, encoding = "UTF-8")

# Verificar la carga correcta de la base de datos
cat("=== DIMENSIÓN DE LA BASE DE DATOS ===\n")
print(dim(datos))

cat("\n=== PRIMERAS 6 OBSERVACIONES ===\n")
print(head(datos))

cat("\n=== ESTRUCTURA GENERAL DE LAS VARIABLES ===\n")
str(datos)

# 2. Resumen estadístico general
cat("\n=== RESUMEN ESTADÍSTICO GENERAL ===\n")
print(summary(datos))

# 3. Análisis descriptivo detallado de variables cuantitativas
# Función para calcular media, desviación estándar, mediana, mínimo y máximo
calc_estadisticos <- function(x) {
  c(
    Media = mean(x, na.rm = TRUE),
    Desv_Est = sd(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Minimo = min(x, na.rm = TRUE),
    Maximo = max(x, na.rm = TRUE)
  )
}

cat("\n=== ESTADÍSTICOS DESCRIPTIVOS - VARIABLES CUANTITATIVAS ===\n")
vars_cuant <- datos[, c("Edad", "Horas Sueño", "Pasos/día", "Calorías (kcal)", 
                        "Distancia (km)", "Tiempo Activo (min)", 
                        "Uso Tel. (min)", "Prom. Notas (1-7)")]
print(sapply(vars_cuant, calc_estadisticos))

# 4. Tablas de frecuencia para variables cualitativas
cat("\n=== TABLA DE FRECUENCIA: SEXO ===\n")
print(table(datos$Sexo))
print(prop.table(table(datos$Sexo)) * 100)

cat("\n=== TABLA DE FRECUENCIA: RESIDENCIA ===\n")
print(table(datos$Residencia))

cat("\n=== TABLA DE FRECUENCIA: ACTIVIDAD FÍSICA ===\n")
print(table(datos$`¿Hace AF?`))

# 5. Gráficos descriptivos básicos
# Histograma del promedio de notas (con el espacio exacto: "Prom. Notas (1-7)")
hist(datos$`Prom. Notas (1-7)`, 
     main = "Distribución del Promedio de Notas", 
     xlab = "Promedio (1.0 - 7.0)", 
     col = "steelblue", border = "black")

# Boxplot comparativo de uso diario de celular según el sexo
boxplot(`Uso Tel. (min)` ~ Sexo, data = datos,
        main = "Tiempo Diario de Uso de Celular por Sexo",
        xlab = "Sexo", ylab = "Minutos / día",
        col = c("lightpink", "lightblue"))