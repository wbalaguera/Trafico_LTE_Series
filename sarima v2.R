library(forecast)
library(dplyr)
library(lubridate)
library(ggplot2)  # Asegúrate de cargar el paquete ggplot2

# Leer los datos
data_sitio_fiesta <- read.csv("data_sitio_smoothed.csv", sep = ",")
data_sitio_fiesta$M_FECHA_DIA <- as.Date(data_sitio_fiesta$M_FECHA_DIA)
data_sitio_fiesta <- data_sitio_fiesta[order(data_sitio_fiesta$M_FECHA_DIA), ]
df <- data_sitio_fiesta %>%
  select(M_FECHA_DIA, PAYLOAD_4G_smoo_mean, FIESTA) %>%
  rename(ds = M_FECHA_DIA, y = PAYLOAD_4G_smoo_mean)

# Crear series temporales
serie_y <- ts(df$y, frequency = 365, start = c(year(min(df$ds)), yday(min(df$ds))))
regresor_fiestas <- ts(df$FIESTA, frequency = 365, start = c(year(min(df$ds)), yday(min(df$ds))))

# Ajustar el modelo SARIMA con regresores
fit_sarima <- auto.arima(serie_y, xreg = regresor_fiestas, seasonal = TRUE)

# Resumen del modelo
summary(fit_sarima)

# Hacer predicciones
futuro <- data.frame(ds = seq.Date(from = max(df$ds) + 1, by = "day", length.out = 365))
futuro$FIESTA <- ifelse(futuro$ds %in% as.Date(c('2024-10-10', '2024-10-11', '2024-10-12', '2024-10-13', '2024-10-14')), 1, 0)

# Convertir las fechas a serie temporal
futuro_regresor <- ts(futuro$FIESTA, frequency = 365, start = c(year(min(futuro$ds)), yday(min(futuro$ds))))

# Hacer predicciones usando el modelo ajustado
predicciones <- forecast(fit_sarima, xreg = futuro_regresor, h = 365)

# Graficar las predicciones
autoplot(predicciones) +
  labs(title = "Pronóstico SARIMA con Fiestas como Regresor", x = "Fecha", y = "Valor Predicho")

# Evaluar el modelo
actual <- df$y
predicted <- fitted(fit_sarima)

# Métricas de error
mae_value <- mean(abs(actual - predicted))
mse_value <- mean((actual - predicted)^2)
rmse_value <- sqrt(mse_value)
r2_value <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
mape_value <- mean(abs((actual - predicted) / actual)) * 100

cat("MSE:", mse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("R²:", r2_value, "\n")
cat("MAPE:", mape_value, "\n")

# Prueba de residuos de Ljung-Box
residuos <- residuals(fit_sarima)
ljung_box_result <- Box.test(residuos, lag = 30, type = "Ljung-Box")
print(ljung_box_result)

# Graficar los residuos
autoplot(residuos) +
  labs(title = "Residuos del Modelo SARIMA", x = "Fecha", y = "Residuos")

# Graficar ACF y PACF de los residuos
acf_plot <- ggAcf(residuos) +
  labs(title = "ACF de los Residuos del Modelo SARIMA")
pacf_plot <- ggPacf(residuos) +
  labs(title = "PACF de los Residuos del Modelo SARIMA")

# Mostrar los gráficos
print(acf_plot)
print(pacf_plot)
