# Instalar Librerias
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("skimr")

library(tidyverse)
library(janitor)
library(skimr)

# Carga de datos
dengue_data <- read.csv(file.choose())

# Normalizar los nombres con janitor y mostrarlos
dengue_data <- clean_names(dengue_data)
names(dengue_data)

# Inspección inicial
glimpse(dengue_data)

# Resumen general
summary(dengue_data)

# Valores faltantes
colSums(is.na(dengue_data))

#==========================
# PREPARACIÓN DEL DATASET
#==========================
# Número de registros antes de conversión
nrow(dengue_data)

# Conversión explícita a char antes de parse_number para modelo 2
# Se generaba un warning
dengue_data <- dengue_data %>%
  mutate(
    across(
      c(incidence_rate_c, lab_confirm_incidence_rate),
      ~ parse_number(as.character(.))
    )
  ) %>%
  drop_na(deaths, incidence_rate_c)

# Número de registros después de conversión
nrow(dengue_data)

# Cuántos NA se generan  
sum(is.na(dengue_data$incidence_rate_c))
mean(is.na(dengue_data$incidence_rate_c)) * 100

sum(is.na(dengue_data$lab_confirm_incidence_rate))
mean(is.na(dengue_data$lab_confirm_incidence_rate)) * 100

# Verificación
glimpse(dengue_data)

#==========================
# EDA GENERAL
#==========================

# Número de países
n_distinct(dengue_data$country_or_subregion)

# Rango temporal
range(dengue_data$year)

# Resumen numérico
summary(
  dengue_data %>%
    select(deaths, incidence_rate_c)
)

#===============
# DISTRIBUCIÓN Y ASIMETRÍA
#===============
# Limpieza
dengue_data <- dengue_data %>%
  filter(is.finite(deaths))


# Histogramas
ggplot(dengue_data, aes(x = deaths)) +
  geom_histogram(bins = 30)

ggplot(dengue_data, aes(x = incidence_rate_c)) +
  geom_histogram(bins = 30)

# Boxplots
ggplot(dengue_data, aes(y = deaths)) +
  geom_boxplot()

ggplot(dengue_data, aes(y = incidence_rate_c)) +
  geom_boxplot()

#==========================
# RELACIÓN VISUAL
#==========================
ggplot(dengue_data,
       aes(x = incidence_rate_c,
           y = deaths)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE)

#=============================================
# DIAGNÓSTICO DE SUPUESTOS
#=============================================
# Correlación visual avanzada
library(GGally)

ggpairs(
  dengue_data %>%
    select(deaths, incidence_rate_c)
)

# Correlación de Pearson
cor.test(
  dengue_data$incidence_rate_c,
  dengue_data$deaths,
  method = "pearson"
)

# Modelo de regresión
modelo_lm <- lm(
  deaths ~ incidence_rate_c,
  data = dengue_data
)

summary(modelo_lm)

#=============================================
# DIAGNÓSTICO FORMAL DE RESIDUOS Y SUPUESTOS
#=============================================

# Diagnóstico gráfico
par(mfrow = c(2,2))
plot(modelo_lm)

# Normalidad de residuos
shapiro.test(residuals(modelo_lm))

# Heterocedasticidad
library(lmtest)
bptest(modelo_lm)

# Observaciones influyentes
library(car)
influencePlot(modelo_lm)

