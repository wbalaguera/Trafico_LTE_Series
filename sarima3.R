# Cargar librerías necesarias
library(forecast)
library(tidyverse)
library(tsibble)
library(fable)
library(rsample)

# Cargar y preparar datos
data_el <- read.csv("data_sitio_smoothed.csv", sep = ",")
data_el$M_FECHA_DIA <- as.Date(data_el$M_FECHA_DIA)
data_el <- data_el %>%
  arrange(M_FECHA_DIA) %>%
  select(M_FECHA_DIA, PAYLOAD_4G_smoo_mean, FIESTA)

# Crear una serie temporal tsibble y llenar huecos
data_tsibble <- data_el %>%
  as_tsibble(index = M_FECHA_DIA) %>%
  fill_gaps()

# Verificar los datos llenados
head(data_tsibble)


# Ajustar el modelo SARIMA
fit_sarima <- data_tsibble %>%
  model(SARIMA = ARIMA(PAYLOAD_4G_smoo_mean ~ FIESTA + pdq(1,1,1) + PDQ(0,1,1, period = 365)))


# Realizar la validación cruzada
set.seed(123)  # Para reproducibilidad
cv_folds <- rsample::vfold_cv(data_tsibble, v = 10)

# Función para ajustar y evaluar el modelo en cada fold
evaluate_model <- function(split) {
  train_data <- training(split)
  test_data <- testing(split)

  fit_sarima <- train_data %>%
    model(SARIMA = ARIMA(PAYLOAD_4G_smoo_mean ~ FIESTA + pdq(1,1,1) + PDQ(0,1,1, period = 365)))

  forecast_sarima <- fit_sarima %>%
    forecast(new_data = test_data)

  accuracy(forecast_sarima, test_data)
}

# Aplicar la validación cruzada
cv_results <- cv_folds %>%
  mutate(metrics = map(splits, evaluate_model))

# Calcular el promedio de las métricas de error
cv_metrics <- cv_results %>%
  unnest(metrics) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

print(cv_metrics)

# Mostrar las métricas de rendimiento
cv_metrics %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Métricas de Error Promedio en Validación Cruzada",
       x = "Métrica", y = "Valor Promedio")


