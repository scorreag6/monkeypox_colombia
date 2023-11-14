### Entrega 2 ###
library(tidyverse)
database <- read_delim("Biostatistics project/database.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(database)

### Item 2 ###
# Histogramas para Variables Numéricas
ggplot(database, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Edades",
       x = "Edad",
       y = "Frecuencia")

ggplot(database, aes(x = epidemiological_week)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Semanas Epidemiológicas",
       x = "Semana Epidemiológica",
       y = "Frecuencia")

# Gráficos de Barras para Datos Cualitativos
ggplot(database, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Distribución por Género",
       x = "Género",
       y = "Frecuencia")

ggplot(database, aes(x = social_security_type, fill = social_security_type)) +
  geom_bar() +
  labs(title = "Distribución por Tipo de Seguro",
       x = "Tipo de Seguro",
       y = "Frecuencia")

# Gráficos de Círculo para Proporciones
database %>%
  group_by(hospitalization) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = factor(hospitalization))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proporción de Hospitalizaciones",
       fill = "Hospitalización",
       x = NULL,
       y = NULL) +
  theme_void()

# Box Plots para Identificar Outliers
filtered_data <- database %>%
  filter(stratum != 999)
ggplot(filtered_data, aes(x = as.factor(stratum), y = age, fill = as.factor(stratum))) +
  geom_boxplot() +
  labs(title = "Distribución de Edades por Estrato Social (Excluyendo 999)",
       x = "Estrato Social",
       y = "Edad") +
  theme(legend.position = "none")

# Scatter plot para la relación entre edad y semana epidemiológica
ggplot(database, aes(x = age, y = epidemiological_week, color = gender)) +
  geom_point() +
  labs(title = "Relación entre Edad y Semana Epidemiológica",
       x = "Edad",
       y = "Semana Epidemiológica",
       color = "Género")

### Item 3 ###
# Convertir la columna 'epidemiological_week' a formato de fecha para facilitar el análisis temporal
database$epidemiological_week <- as.Date(paste0(database$epidemiological_year, "-W", database$epidemiological_week, "-1"), format="%Y-W%U-%u")

# Gráfico de línea para mostrar la tendencia a lo largo de las semanas epidemiológicas
ggplot(database, aes(x = epidemiological_week, y = ..count.., group = 1)) +
  geom_line(stat = "count", color = "skyblue") +
  labs(title = "Tendencia de Casos a lo Largo de las Semanas Epidemiológicas",
       x = "Semana Epidemiológica",
       y = "Número de Casos") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de línea para mostrar la tendencia a lo largo de los años epidemiológicos
ggplot(database, aes(x = epidemiological_year, y = ..count.., group = 1)) +
  geom_line(stat = "count", color = "skyblue") +
  labs(title = "Tendencia de Casos a lo Largo de los Años Epidemiológicos",
       x = "Año Epidemiológico",
       y = "Número de Casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras apiladas para mostrar la distribución de casos por año
ggplot(database, aes(x = as.factor(epidemiological_year), fill = as.factor(gender))) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Casos por Año",
       x = "Año Epidemiológico",
       y = "Número de Casos",
       fill = "Género") +
  theme(legend.position = "top")

# Gráfico de densidad para mostrar la distribución de casos a lo largo de las semanas epidemiológicas
ggplot(database, aes(x = epidemiological_week, fill = gender)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribución de Casos por Semana Epidemiológica",
       x = "Semana Epidemiológica",
       y = "Densidad") +
  theme_minimal()

# Calcular el recuento de casos por semana epidemiológica y género
count_data <- database %>%
  group_by(epidemiological_week, gender) %>%
  summarise(case_count = n())
# Gráfico de líneas múltiples para comparar la tendencia de casos por género a lo largo de las semanas epidemiológicas
ggplot(count_data, aes(x = epidemiological_week, y = case_count, color = gender, group = gender)) +
  geom_line() +
  labs(title = "Tendencia de Casos por Género a lo Largo de las Semanas Epidemiológicas",
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       color = "Género") +
  theme_minimal()

# Calcular el recuento de casos por semana epidemiológica y año epidemiológico
count_data <- database %>%
  group_by(epidemiological_year, epidemiological_week) %>%
  summarise(case_count = n())
# Crear un heatmap
ggplot(count_data, aes(x = epidemiological_week, y = as.factor(epidemiological_year), fill = case_count)) +
  geom_tile() +
  labs(title = "Variación en el Número de Casos por Semana Epidemiológica y Año",
       x = "Semana Epidemiológica",
       y = "Año Epidemiológico",
       fill = "Número de Casos") +
  scale_fill_viridis_c() +  # Puedes ajustar el esquema de colores según tus preferencias
  theme_minimal()

# Calcular el recuento de casos confirmados y hospitalizaciones por semana epidemiológica
count_data <- database %>%
  group_by(epidemiological_week) %>%
  summarise(confirmed_cases = sum(final_condition == 1, na.rm = TRUE),
            hospitalizations = sum(hospitalization == 1, na.rm = TRUE))
# Crear un gráfico de líneas múltiples
ggplot(count_data, aes(x = epidemiological_week)) +
  geom_line(aes(y = confirmed_cases, color = "Casos Confirmados"), linewidth = 1.5) +
  geom_line(aes(y = hospitalizations, color = "Hospitalizaciones"), linewidth = 1.5) +
  labs(title = "Tendencia de Casos Confirmados y Hospitalizaciones",
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       color = "Variable") +
  scale_color_manual(values = c("Casos Confirmados" = "blue", "Hospitalizaciones" = "red")) +
  theme_minimal()

# Agrupar datos por semana epidemiológica y edad y calcular el número total de casos confirmados
age_data <- database %>%
  group_by(epidemiological_week, age) %>%
  summarise(total_cases = sum(final_condition == 1, na.rm = TRUE))
# Crear un gráfico de área apilada para mostrar la composición de casos confirmados por grupos de edad
ggplot(age_data, aes(x = epidemiological_week, y = total_cases, fill = as.factor(age))) +
  geom_area(position = "stack") +
  labs(title = "Composición de Casos Confirmados por Grupos de Edad",
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       fill = "Grupo de Edad") +
  theme_minimal()

# Crear un gráfico de líneas suavizadas para mostrar la tendencia de casos confirmados por semana epidemiológica
ggplot(database, aes(x = epidemiological_week, y = ifelse(final_condition == 1, 1, 0))) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1.5) +
  labs(title = "Tendencia de Casos Confirmados a lo Largo de las Semanas Epidemiológicas",
       x = "Semana Epidemiológica",
       y = "Proporción de Casos Confirmados") +
  theme_minimal()

# Agrupar datos por semana epidemiológica y calcular el número total de casos confirmados y hospitalizaciones
weekly_data <- database %>%
  group_by(epidemiological_week) %>%
  summarise(total_cases = sum(final_condition == 1, na.rm = TRUE),
            total_hospitalizations = sum(hospitalization == 1, na.rm = TRUE))
# Crear un gráfico de barras agrupadas para comparar casos confirmados y hospitalizaciones por semana epidemiológica
ggplot(weekly_data, aes(x = as.factor(epidemiological_week))) +
  geom_bar(aes(y = total_cases, fill = "Casos Confirmados"), 
           position = "dodge", stat = "identity", width = 0.5) +
  geom_bar(aes(y = total_hospitalizations, fill = "Hospitalizaciones"), 
           position = "dodge", stat = "identity", width = 0.5) +
  labs(title = "Comparación de Casos Confirmados y Hospitalizaciones por Semana Epidemiológica",
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       fill = "Variable") +
  scale_fill_manual(values = c("Casos Confirmados" = "blue", "Hospitalizaciones" = "red")) +
  theme_minimal()

# Agrupar datos por semana epidemiológica y calcular el número total de casos confirmados, hospitalizaciones y casos con conexión epidemiológica
weekly_data <- database %>%
  group_by(epidemiological_week) %>%
  summarise(total_cases = sum(final_condition == 1, na.rm = TRUE),
            total_hospitalizations = sum(hospitalization == 1, na.rm = TRUE),
            total_epidemiological_connections = sum(!is.na(epidemiological_connection)))
# Crear un gráfico de líneas múltiples con puntos para comparar las tendencias
ggplot(weekly_data, aes(x = as.factor(epidemiological_week))) +
  geom_line(aes(y = total_cases, color = "Casos Confirmados"), size = 1.5) +
  geom_point(aes(y = total_cases, color = "Casos Confirmados"), size = 3) +
  geom_line(aes(y = total_hospitalizations, color = "Hospitalizaciones"), size = 1.5) +
  geom_point(aes(y = total_hospitalizations, color = "Hospitalizaciones"), size = 3) +
  geom_line(aes(y = total_epidemiological_connections, color = "Casos con Conexión Epidemiológica"), size = 1.5) +
  geom_point(aes(y = total_epidemiological_connections, color = "Casos con Conexión Epidemiológica"), size = 3) +
  labs(title = "Tendencias de Casos, Hospitalizaciones y Conexiones",
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       color = "Variable") +
  theme_minimal()

### Item 4 ###
# Crear un gráfico de barras apiladas para la distribución por edad y género
ggplot(database, aes(x = age, fill = gender)) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Casos Confirmados por Edad y Género",
       x = "Edad",
       y = "Número de Casos",
       fill = "Género") +
  theme_minimal()

# Filtrar el valor 999 del estrato socioeconómico y asignar colores
database_filtered <- database %>%
  filter(stratum != 999)
# Crear un gráfico de barras para la distribución por estrato socioeconómico con colores asignados
ggplot(database_filtered, aes(x = as.factor(stratum), fill = as.factor(stratum))) +
  geom_bar() +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  labs(title = "Distribución de Casos Confirmados por Estrato",
       x = "Estrato Socioeconómico",
       y = "Número de Casos") +
  theme_minimal()

# Crear un gráfico de barras agrupadas para la comparación por tipo de seguridad social
ggplot(database, aes(x = social_security_type, fill = as.factor(final_condition))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparación de Casos Confirmados por Tipo de Seguridad Social",
       x = "Tipo de Seguridad Social",
       y = "Número de Casos",
       fill = "Condición Final") +
  theme_minimal()

# Calcular el número de casos por cada combinación de edad y estrato socioeconómico, excluyendo estrato 999
cases_count <- database %>%
  filter(stratum != 999) %>%
  group_by(age, stratum) %>%
  summarise(total_cases = n())
# Crear un gráfico de dispersión con facetas para analizar la relación entre edad y número de casos confirmados, desglosado por estrato socioeconómico
ggplot(cases_count, aes(x = age, y = total_cases, color = as.factor(stratum), size = total_cases)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~as.factor(stratum), scales = "free_y", ncol = 2) +
  labs(title = "Relación entre Edad y Número de Casos Confirmados, Desglosado por Estrato Socioeconómico",
       x = "Edad",
       y = "Número de Casos",
       color = "Estrato Socioeconómico",
       size = "Número de Casos") +
  theme_minimal()

# Crear un gráfico de violín para visualizar la distribución de edades por estrato socioeconómico
ggplot(database_filtered, aes(x = as.factor(stratum), y = age, fill = as.factor(stratum))) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribución de Edades de Casos Confirmados por Estrato Socioeconómico",
       x = "Estrato Socioeconómico",
       y = "Edad",
       fill = "Estrato Socioeconómico") +
  theme_minimal()

# Crear un gráfico de densidad combinado con un gráfico de dispersión para analizar la relación entre edad y estrato socioeconómico
ggplot(database_filtered, aes(x = age, fill = as.factor(stratum))) +
  geom_density(alpha = 0.5) +
  geom_point(aes(y = 0), position = "jitter", color = "blue", alpha = 0.5) +
  labs(title = "Distribución de Edades y Puntos de Casos Confirmados por Estrato Socioeconómico",
       x = "Edad",
       y = "Densidad") +
  theme_minimal()

# Calcular el número de casos por cada combinación de edad, estrato socioeconómico y género
cases_count <- database %>%
  filter(stratum != 999) %>%  # Filtrar el valor 999 del estrato socioeconómico
  group_by(age, stratum, gender) %>%
  summarise(total_cases = n())
# Crear un gráfico de dispersión condicional para analizar la relación entre edad y número de casos confirmados, desglosado por género
ggplot(cases_count, aes(x = age, y = total_cases, color = gender)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relación entre Edad y Número de Casos Confirmados, Desglosado por Género",
       x = "Edad",
       y = "Número de Casos",
       color = "Género") +
  theme_minimal()

# Crear un gráfico de dispersión con línea de tendencia para analizar la relación entre dos variables numéricas
ggplot(database_filtered, aes(x = age, y = department)) +
  geom_point(alpha = 0.7) +                  # Puntos de dispersión
  geom_smooth(method = "lm", color = "red") + # Línea de tendencia (regresión lineal)
  labs(title = "Relación entre Edad y Departamento",
       x = "Edad",
       y = "Departamento") +
  theme_minimal()

# Crear un box plot desglosado por una variable categórica
ggplot(database_filtered, aes(x = department, y = age, fill = department)) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Departamento",
       x = "Departamento",
       y = "Edad") +
  theme_minimal()

# Crear un gráfico de violín para visualizar la distribución de una variable numérica en diferentes categorías
ggplot(database_filtered, aes(x = ethnicity, y = age, fill = social_security_type)) +
  geom_violin() +
  labs(title = "Distribución de Valor en Diferentes Categorías",
       x = "Etnia",
       y = "Edad") +
  theme_minimal()

### Item 5 ###
# Gráfico de dispersión para analizar relación entre tipo de seguro y edad
ggplot(database, aes(x = age, y = social_security_type, color = social_security_type)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relación entre Edad y Tipo de Seguro Social",
       x = "Edad",
       y = "Tipo de Seguro Social") +
  theme_minimal()

# Diagrama de dispersión para analizar la relación entre edad y semana epidemiológica
ggplot(database, aes(x = age, y = epidemiological_week, color = infection_source)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relación entre Edad y Semana Epidemiológica por Fuente de Infección",
       x = "Edad",
       y = "Semana Epidemiológica",
       color = "Fuente de Infección") +
  theme_minimal()

# Crear un mapa de calor para visualizar la relación entre semana epidemiológica, edad y fuente de infección
ggplot(database, aes(x = as.factor(epidemiological_week), y = as.factor(age), fill = infection_source)) +
  geom_tile() +
  labs(title = "Mapa de Calor: Relación entre Semana Epidemiológica, Edad y Fuente de Infección",
       x = "Semana Epidemiológica",
       y = "Edad",
       fill = "Fuente de Infección") +
  theme_minimal()

# Gráfico de caja y bigotes para comparar la distribución de edades por fuente de infección
ggplot(database, aes(x = infection_source, y = age, fill = infection_source)) +
  geom_boxplot() +
  labs(title = "Distribución de Edades por Fuente de Infección",
       x = "Fuente de Infección",
       y = "Edad",
       fill = "Fuente de Infección") +
  theme_minimal()

# Gráfico de barras apiladas para comparar tipos de seguro social por género
ggplot(database, aes(x = social_security_type, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Tipos de Seguro Social por Género",
       x = "Tipo de Seguro Social",
       y = "Proporción",
       fill = "Género") +
  theme_minimal()

# Filtrar los valores 999 en la variable stratum
database_filtered <- subset(database, stratum != 999)
# Gráfico de barras apiladas para comparar hospitalizaciones por estrato socioeconómico y género (filtrando 999)
ggplot(database_filtered, aes(x = as.factor(stratum), fill = as.factor(hospitalization), color = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Comparación de Hospitalizaciones por Estrato Socioeconómico y Género (Filtrando 999)",
       x = "Estrato Socioeconómico",
       y = "Proporción",
       fill = "Hospitalización",
       color = "Género") +
  theme_minimal()

# Gráfico de barras para comparar contagios por departamento
ggplot(database, aes(x = department)) +
  geom_bar() +
  labs(title = "Número de Contagios por Departamento",
       x = "Departamento",
       y = "Número de Contagios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de torta para comparar fuentes de infección
ggplot(database, aes(x = "", fill = infection_source)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(title = "Proporción de Fuentes de Infección") +
  theme_void()

### Item 6 ###
# Modelo de regresión lineal
modelo_regresion_lineal <- lm(age ~ epidemiological_week, data = database)
# Gráfico del modelo de regresión lineal
ggplot(database, aes(x = epidemiological_week, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Modelo de Regresión Lineal: Edad vs. Semana Epidemiológica",
       x = "Semana Epidemiológica",
       y = "Edad") +
  theme_minimal()

# Modelo de regresión lineal
modelo_regresion_lineal <- lm(age ~ epidemiological_week + gender, data = database)
# Gráfico del modelo de regresión lineal
ggplot(database, aes(x = epidemiological_week, y = age, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Modelo de Regresión Lineal: Edad vs. Semana Epidemiológica y Género",
       x = "Semana Epidemiológica",
       y = "Edad",
       color = "Género") +
  theme_minimal()

# Convertir la columna infection_source a factor
database$infection_source <- as.factor(database$infection_source)
# Modelo de regresión logística
modelo_regresion_logistica <- glm(hospitalization ~ age + infection_source, data = database, family = "binomial")
# Gráfico del modelo de regresión logística
ggplot(database, aes(x = age, y = as.numeric(hospitalization), color = infection_source)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Modelo de Regresión Logística: Hospitalización vs. Edad y Fuente de Infección",
       x = "Edad",
       y = "Probabilidad de Hospitalización",
       color = "Fuente de Infección") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Sí")) +  # Etiquetas en el eje y
  theme_minimal()

# Filtrar los datos para eliminar los valores 999 de los estratos
database_filtered <- database[database$stratum != 999,]
# Modelo de regresión lineal
modelo_regresion_lineal <- lm(age ~ stratum, data = database_filtered)
# Gráfico del modelo de regresión lineal
ggplot(database_filtered, aes(x = stratum, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Modelo de Regresión Lineal: Edad vs. Estrato Socioeconómico",
       x = "Estrato Socioeconómico",
       y = "Edad") +
  theme_minimal()

# Filtrar los datos para eliminar los valores 999 de los estratos
database_filtered <- database[database$stratum != 999,]
# Modelo de regresión lineal
modelo_regresion_lineal <- lm(age ~ department, data = database_filtered)
# Gráfico del modelo de regresión lineal
ggplot(database_filtered, aes(x = department, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Modelo de Regresión Lineal: Edad vs. Departamento",
       x = "Departamento",
       y = "Edad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir la columna ethnicity a factor
database$ethnicity <- as.factor(database$ethnicity)
# Modelo de regresión logística
modelo_regresion_logistica <- glm(infection_source ~ gender + ethnicity, data = database, family = "binomial")
# Gráfico del modelo de regresión logística
ggplot(database, aes(x = ethnicity, y = as.numeric(infection_source), color = gender)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Modelo de Regresión Logística: Fuente de Infección vs. Género y Etnia",
       x = "Etnia",
       y = "Probabilidad de Infección",
       color = "Género") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Sí")) +  # Etiquetas en el eje y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filtrar los datos para eliminar los valores 999 de los estratos
database_filtered <- database[database$stratum != 999,]
# Modelo de regresión lineal
modelo_regresion_lineal <- lm(age ~ stratum + social_security_type, data = database_filtered)
# Gráfico del modelo de regresión lineal
ggplot(database_filtered, aes(x = social_security_type, y = age, color = stratum)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Modelo de Regresión Lineal: Edad vs. Estrato Socioeconómico y Tipo de Seguro",
       x = "Tipo de Seguro",
       y = "Edad",
       color = "Estrato Socioeconómico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir la columna gender a factor
database$gender <- as.factor(database$gender)
# Filtrar los datos para eliminar los valores 999 de los estratos
database_filtered <- database[database$stratum != 999,]
# Modelo de regresión logística
modelo_regresion_logistica <- glm(gender ~ stratum + as.Date(diagnosis_date, format = "%d/%m/%y"), data = database_filtered, family = "binomial")
# Gráfico del modelo de regresión logística
ggplot(database_filtered, aes(x = as.Date(diagnosis_date, format = "%d/%m/%y"), y = as.numeric(gender), color = stratum)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Modelo de Regresión Logística: Género vs. Estrato Socioeconómico y Fecha de Diagnóstico",
       x = "Fecha de Diagnóstico",
       y = "Probabilidad de Género Femenino",
       color = "Estrato Socioeconómico") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Masculino", "Femenino")) +  # Etiquetas en el eje y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filtrar los datos para eliminar los valores 999 de los estratos
database_filtered <- database[database$stratum != 999,]
# Modelo de regresión polinómica de segundo grado
modelo_regresion_polinomica <- lm(age ~ poly(stratum, 2), data = database_filtered)
# Gráfico del modelo de regresión polinómica
ggplot(database_filtered, aes(x = stratum, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  labs(title = "Regresión Polinómica: Edad vs. Estrato Socioeconómico",
       x = "Estrato Socioeconómico",
       y = "Edad") +
  theme_minimal()

###Fin del codigo ###