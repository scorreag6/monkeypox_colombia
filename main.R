################### Inicio del codigo de la primera entrega #############
# Cargar el paquete
library(tidyverse)

# Fuente (tipo de texto)
windowsFonts(Palatino = windowsFont("Palatino Linotype"))

# Importar base de datos
database <- read_delim("database.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

####################ENTREGA1#################

# Sumario de los datos
s1 <- summary(database)
s1


# Histograma para semana epidemiológica
ggplot(database, aes(x = epidemiological_week)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  labs(title = "Distribución de Semanas Epidemiológicas", x = "Semana Epidemiológica", y = "Frecuencia") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

#### Años epidemiologicos no se realiza debido a que es solo 2022 y un muy bajo rango 2023

# Grafico circular para ciudades afectadas
data_counts_municipality <- database %>%
  count(municipality, name = "Record_Count") %>%
  arrange(desc(Record_Count)) %>%
  top_n(5)
total_records_municipality <- sum(data_counts_municipality$Record_Count)
data_counts_municipality <- data_counts_municipality %>%
  mutate(Percentage = (Record_Count / total_records_municipality) * 100)
ggplot(data_counts_municipality, aes(x = "", y = Record_Count, fill = municipality)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Principales Ciudades Afectadas",
       fill = "Ciudad Afectada") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Grafico circular para Departamentos afectados
data_counts_department <- database %>%
  count(department, name = "Record_Count") %>%
  arrange(desc(Record_Count)) %>%
  top_n(5)
total_records_department <- sum(data_counts_department$Record_Count)
data_counts_department <- data_counts_department %>%
  mutate(Percentage = (Record_Count / total_records_department) * 100)
ggplot(data_counts_department, aes(x = "", y = Record_Count, fill = department)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Principales Departamentos Afectados",
       fill = "Departamento Afectado") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Histograma para género
ggplot(database, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Distribución de Género", x = "Género", y = "Frecuencia")

# Histograma para edad
ggplot(database, aes(x = age)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia")

# Gráfico de ojiva para la Edad 
data_ogive <- database %>%
  group_by(age) %>%
  summarise(Frequency = n()) %>%
  arrange(age) %>%
  mutate(CumulativeFrequency = cumsum(Frequency))
ggplot(data_ogive, aes(x = age, y = CumulativeFrequency)) +
  geom_step(color = "blue") +
  labs(title = "Ojiva de Distribución de Edades", x = "Edad", y = "Frecuencia Acumulada") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

### No se realiza grafico para unidad de medida ya que todos usaron la medicion de edad por años 

# Grafico circular para Hospitalizacion
data_counts_hospitalization <- database %>%
  count(hospitalization, name = "Record_Count")
total_records_hospitalization <- sum(data_counts_hospitalization$Record_Count)
data_counts_hospitalization <- data_counts_hospitalization %>%
  mutate(Percentage = (Record_Count / total_records_hospitalization) * 100,
         hospitalization_label = ifelse(hospitalization == 1, "Hospitalizado", "No Hospitalizado"))
ggplot(data_counts_hospitalization, aes(x = "", y = Record_Count, fill = hospitalization_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Hospitalización", fill = "Estado de Hospitalización") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Grafico circular para condiciones finales
data_counts_condition <- database %>%
  count(final_condition, name = "Record_Count")
total_records_condition <- sum(data_counts_condition$Record_Count)
data_counts_condition <- data_counts_condition %>%
  mutate(Percentage = (Record_Count / total_records_condition) * 100)
data_counts_condition$final_condition <- factor(data_counts_condition$final_condition,
                                                levels = c("1", "2", "3", "4", "5"),
                                                labels = c("Recuperado", "Fallecido", "Hospitalizado", "UCI", "Desconocido"))
ggplot(data_counts_condition, aes(x = "", y = Record_Count, fill = final_condition)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Condición Final de los Infectados", fill = "Condición Final") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Gráfico de dispersión para semana epidemiológica y edad con color por género
ggplot(database, aes(x = epidemiological_week, y = age, color = gender)) +
  geom_point() +
  labs(title = "Dispersión de Semana Epidemiológica vs. Edad", x = "Semana Epidemiológica", y = "Edad") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

# Gráfico de barras para grupo étnico
ggplot(database, aes(x = ethnicity, fill = ethnicity)) +
  geom_bar() +
  labs(title = "Distribución de Grupo Étnico", x = "Grupo Étnico", y = "Frecuencia") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

# Grafico circular para grupo etnico
data_counts_ethnicity <- database %>%
  count(ethnicity, name = "Record_Count")
total_records_ethnicity <- sum(data_counts_ethnicity$Record_Count)
data_counts_ethnicity <- data_counts_ethnicity %>%
  mutate(Percentage = (Record_Count / total_records_ethnicity) * 100)
data_counts_ethnicity$ethnicity <- factor(data_counts_ethnicity$ethnicity,
                                          levels = c("1", "2", "3", "4", "5", "99"),
                                          labels = c("Indígena", "Afrodescendiente", "Mestizo", "Blanco", "Otro", "Desconocido"))
ggplot(data_counts_ethnicity, aes(x = "", y = Record_Count, fill = ethnicity)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribución de Grupo Étnico", fill = "Grupo Étnico") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Gráfico de círculo para fuente de infección
data_counts_infection <- database %>%
  count(infection_source, name = "Record_Count") %>%
  arrange(desc(Record_Count)) %>%
  top_n(5)
total_records_infection <- sum(data_counts_infection$Record_Count)
data_counts_infection <- data_counts_infection %>%
  mutate(Percentage = (Record_Count / total_records_infection) * 100)
ggplot(data_counts_infection, aes(x = "", y = Record_Count, fill = infection_source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Fuente de Infección Top 5",
       fill = "Fuente de Infección") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Gráfico de barras para tipo de seguro social
ggplot(database, aes(x = social_security_type, fill = social_security_type)) +
  geom_bar() +
  labs(title = "Distribución de Tipo de Seguro Social", x = "Tipo de Seguro Social", y = "Frecuencia") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

# Gráfico de círculo para tipos de seguro social
filtered_database <- database %>%
  filter(!is.na(social_security_type))  # Filtrar filas con tipo de seguro social válido
data_counts_social_security <- filtered_database %>%
  count(social_security_type, name = "Record_Count")
total_records_social_security <- sum(data_counts_social_security$Record_Count)
data_counts_social_security <- data_counts_social_security %>%
  mutate(Percentage = (Record_Count / total_records_social_security) * 100)
ggplot(data_counts_social_security, aes(x = "", y = Record_Count, fill = social_security_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribución de Tipo de Seguro Social", fill = "Tipo de Seguro Social") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Gráfico de círculo para viajes a otro país
data_counts_traveled <- database %>%
  count(traveled, name = "Record_Count")
total_records_traveled <- sum(data_counts_traveled$Record_Count)
data_counts_traveled <- data_counts_traveled %>%
  mutate(Percentage = (Record_Count / total_records_traveled) * 100,
         traveled_label = ifelse(traveled == 1, "Sí", "No"))
ggplot(data_counts_traveled, aes(x = "", y = Record_Count, fill = traveled_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribución de Viajes a Otro País", fill = "Viaje a Otro País") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Filtrar y calcular porcentaje de estratos válidos (1 a 6)
valid_estrata <- c(1, 2, 3, 4, 5, 6)
filtered_database <- database %>%
  filter(stratum %in% valid_estrata)
data_counts_stratum <- filtered_database %>%
  count(stratum, name = "Record_Count")
total_records_stratum <- sum(data_counts_stratum$Record_Count)
data_counts_stratum <- data_counts_stratum %>%
  mutate(Percentage = (Record_Count / total_records_stratum) * 100)
data_counts_stratum$stratum <- factor(data_counts_stratum$stratum,
                                        levels = as.character(valid_estrata),
                                        labels = c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4", "Estrato 5", "Estrato 6"))
ggplot(data_counts_stratum, aes(x = "", y = Record_Count, fill = stratum)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribución de Estratos Socioeconómicos", fill = "Estrato Socioeconómico") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Boxplot para edad
ggplot(database, aes(y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot de Edades", y = "Edad") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title.y = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14))

# Boxplot para semana epidemiologica
ggplot(database, aes(y = epidemiological_week)) +
  geom_boxplot() +
  labs(title = "Boxplot de Semanas Epidemiológicas", y = "Semana Epidemiológica") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title.y = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14))

# Boxplot para genero y edad
ggplot(database, aes(x = gender, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot de Edades por Género", x = "Género", y = "Edad") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title.y = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot fuente infeccion y edad
ggplot(database, aes(x = infection_source, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot de Edades por Fuente de Infección", x = "Fuente de Infección", y = "Edad") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title.y = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 5),
        axis.text.x = element_text(angle = 25, hjust = 1))

# Grafica de dispersion edad y semana epidemioloigica con genero en color
ggplot(database, aes(x = age, y = epidemiological_week, color = gender)) +
  geom_point() +
  labs(title = "Dispersión de Edad vs. Semana Epidemiológica", x = "Edad", y = "Semana Epidemiológica") +
  theme_minimal(base_family = "Palatino", base_size = 14) +
  theme(plot.title = element_text(family = "Palatino", face = "bold", size = 20),
        axis.title = element_text(family = "Palatino", face = "bold", size = 16),
        axis.text = element_text(family = "Palatino", size = 14),
        legend.text = element_text(family = "Palatino", size = 14),
        legend.title = element_blank())

##################### Fin del codigo de la primera entrega




##########################ENTREGA 2################################
