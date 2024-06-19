
###prueba


```{r}
# Ajustar automáticamente el mejor modelo SARIMA
fit_sarima_auto <- df_tsibble %>%
  model(SARIMA = ARIMA(y ~ FIESTA))

# Resumen del modelo ajustado
report(fit_sarima_auto)

# Realizar la predicción futura utilizando el modelo ajustado
prediccion_futura_sarima_auto <- forecast(fit_sarima_auto, new_data = futuro_ar)

# Graficar las predicciones
autoplot(prediccion_futura_sarima_auto) +
  labs(title = "Predicción SARIMA Automático con Regresor 'FIESTA'", x = "Fecha", y = "Valor")

# Asegurarse de que 'actual' y 'predicted' sean vectores numéricos
actual_sarima_auto <- as.numeric(df$y)
predicted_sarima_auto <- as.numeric(fitted(fit_sarima_auto)$`.fitted`)

# Calcular métricas de rendimiento para el ajuste del modelo
mae_sarima_auto <- mean(abs(actual_sarima_auto - predicted_sarima_auto), na.rm = TRUE)
mse_sarima_auto <- mean((actual_sarima_auto - predicted_sarima_auto)^2, na.rm = TRUE)
rmse_sarima_auto <- sqrt(mse_sarima_auto)
ss_res_sarima_auto <- sum((actual_sarima_auto - predicted_sarima_auto)^2, na.rm = TRUE)
ss_tot_sarima_auto <- sum((actual_sarima_auto - mean(actual_sarima_auto, na.rm = TRUE))^2, na.rm = TRUE)
r_squared_sarima_auto <- 1 - (ss_res_sarima_auto / ss_tot_sarima_auto)
mape_sarima_auto <- mean(abs((actual_sarima_auto - predicted_sarima_auto) / actual_sarima_auto), na.rm = TRUE) * 100

# Imprimir las métricas de rendimiento
cat("MAE SARIMA Automático:", mae_sarima_auto, "\n")
cat("MSE SARIMA Automático:", mse_sarima_auto, "\n")
cat("RMSE SARIMA Automático:", rmse_sarima_auto, "\n")
cat("R² SARIMA Automático:", r_squared_sarima_auto, "\n")
cat("MAPE SARIMA Automático:", mape_sarima_auto, "\n")

# Prueba de Ljung-Box para los residuos del modelo SARIMA Automático
residuos_sarima_auto <- as.numeric(residuals(fit_sarima_auto)$`.resid`)

ljung_box_result_sarima_auto <- Box.test(residuos_sarima_auto, lag = 30, type = "Ljung-Box")
print(ljung_box_result_sarima_auto)

```
