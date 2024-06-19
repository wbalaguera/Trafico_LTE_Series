# Cargar librerías necesarias
library(zoo)
library(quantmod)
library(Metrics)
library(RSNNS)
library(DT)
library(dplyr)

# Cargar los datos
data_el <- read.csv("data_sitio_smoothed.csv", sep = ",")
data_el$M_FECHA_DIA <- as.Date(data_el$M_FECHA_DIA)
data_el <- data_el[order(data_el$M_FECHA_DIA), ]
data_el <- data_el %>%
  select(M_FECHA_DIA, PAYLOAD_4G_smoo_mean, FIESTA)
head(data_el)


# Asegurar que la columna "FIESTA" esté presente y no tenga valores NA
if(!"FIESTA" %in% colnames(data_el)) {
  data_el$FIESTA <- 0
}

# Agregar festividades futuras
future_festivities <- as.Date(c('2024-10-10', '2024-10-11', '2024-10-12', '2024-10-13', '2024-10-14'))
future_dates <- seq.Date(from = max(data_el$M_FECHA_DIA) + 1, by = "day", length.out = length(future_festivities))

# Asegurar que future_data tenga las mismas columnas que data_el
future_data <- data.frame(M_FECHA_DIA = future_dates, PAYLOAD_4G_smoo_mean = NA, FIESTA = ifelse(future_dates %in% future_festivities, 1, 0))
future_data

# Asegurar que ambas tablas tienen las mismas columnas
all_cols <- union(names(data_el), names(future_data))
all_cols
data_el <- data_el[, all_cols, drop = FALSE]
future_data <- future_data[, all_cols, drop = FALSE]

# Unir los datos existentes con los futuros
data_el <- rbind(data_el, future_data)

# Crear la serie temporal
data_ts <- ts(data_el$PAYLOAD_4G_smoo_mean, start = c(as.numeric(format(min(data_el$M_FECHA_DIA), "%Y")), as.numeric(format(min(data_el$M_FECHA_DIA), "%j"))), frequency = 365)
data_fiestas <- ts(data_el$FIESTA, start = c(as.numeric(format(min(data_el$M_FECHA_DIA), "%Y")), as.numeric(format(min(data_el$M_FECHA_DIA), "%j"))), frequency = 365)

# Normalizar los datos
Z <- as.ts(data_ts, F)
S <- (Z - min(Z, na.rm = TRUE)) / (max(Z, na.rm = TRUE) - min(Z, na.rm = TRUE))
F <- (data_fiestas - min(data_fiestas)) / (max(data_fiestas) - min(data_fiestas))

head(F)


# Dividir el dataset en entrenamiento y prueba
tamano_total <- length(S)
tamano_train <- round(tamano_total * 0.75)
train <- 1:tamano_train
test <- (tamano_train + 1):tamano_total

# Preparar los datos para la red neuronal
y <- as.zoo(S)
lags <- 12
inputs <- do.call(cbind, lapply(1:lags, function(k) Lag(y, k)))
fiestas_lag <- do.call(cbind, lapply(1:lags, function(k) Lag(F, k)))
inputs <- cbind(inputs, fiestas_lag)
slogN <- cbind(y, inputs)
slogN1 <- slogN[-(1:lags), ]

inputs <- slogN1[, 2:(lags + 1 + lags)]
outputs <- slogN1[, 1]

# Eliminar filas con NA en inputs y outputs
complete_cases <- complete.cases(inputs, outputs)
inputs <- inputs[complete_cases, ]
outputs <- outputs[complete_cases]

# Dividir el dataset en entrenamiento y prueba
tamano_total <- nrow(inputs)
tamano_train <- round(tamano_total * 0.75)
train_indices <- 1:tamano_train
test_indices <- (tamano_train + 1):tamano_total

inputs_train <- inputs[train_indices, ]
outputs_train <- outputs[train_indices]
inputs_test <- inputs[test_indices, ]
outputs_test <- outputs[test_indices]

# Verificar si hay valores faltantes
anyNA(inputs_train)
anyNA(outputs_train)

# Entrenar la red neuronal Elman
fit <- elman(inputs_train, outputs_train, size = c(5), learnFuncParams = c(0.1), maxit = 1000)

# Gráfica de error iterativo
plotIterativeError(fit, main = "Iterative Error for 5 Neuron")

# Realizar la predicción
pred <- predict(fit, inputs_test)
actual <- as.vector(outputs_test)

# Desnormalizar predicciones y valores reales
pred <- pred * (max(Z, na.rm = TRUE) - min(Z, na.rm = TRUE)) + min(Z, na.rm = TRUE)
actual <- actual * (max(Z, na.rm = TRUE) - min(Z, na.rm = TRUE)) + min(Z, na.rm = TRUE)

# Asegurarse de que pred y actual tengan la misma longitud
if (length(pred) != length(actual)) {
  stop("Las longitudes de 'pred' y 'actual' no coinciden.")
}

# Visualización de resultados
plot(actual, type = "l", col = "blue", ylab = "Valor", xlab = "Índice")
lines(pred, col = "red")
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)

# Métricas de error o rendimiento
mse_value <- mse(actual, pred)
mae_value <- mae(actual, pred)
rmse_value <- rmse(actual, pred)
r2_value <- cor(actual, pred)^2
mape_value <- mape(actual, pred)
cat("MSE:", mse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("R²:", r2_value, "\n")
cat("MAPE:", mape_value, "\n")
