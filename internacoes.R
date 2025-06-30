
library(ggplot2)
library(forecast)
library(dplyr)
library(readr)

# -----------------------------
# 1. Carregar e preparar os dados
# -----------------------------
data_frame <- read.csv("air_quality_health_dataset.csv")

# Verificar se há valores ausentes
any(is.na(data_frame))          # Retorna TRUE se houver qualquer NA
sum(is.na(data_frame))          # Conta o total de valores NA

# Converter coluna 'date' para o tipo Date
data_frame$date <- as.Date(data_frame$date, format = "%Y-%m-%d")

# Transformar variáveis qualitativas em fatores
data_frame$city <- as.factor(data_frame$city)
data_frame$population_density <- as.factor(data_frame$population_density)

# ---------------------------------
# 2. Análise exploratória dos dados
# ---------------------------------

# Boxplot do número de internações
boxplot(data_frame$hospital_admissions,
        main = "Boxplot - Internações",
        ylab = "Número de internações")

# Visualizar casos com internações acima de 16 por dia
library(dplyr)
picos <- data_frame %>%
  filter(hospital_admissions > 16) %>%
  arrange(desc(hospital_admissions)) %>%
  select(date, hospital_admissions)
print(picos)

#----------------------------------|
# 3.Análise de correlação dos dados|
#----------------------------------|

# Análise 1 – hospital_admissions vs date
ggplot(data_frame, aes(x = date, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs data", x = "Data", y = "Hospitalizações") +
  theme_minimal()

modelo_date <- lm(hospital_admissions ~ date, data = data_frame)
summary(modelo_date)$r.squared

# Análise 2 – hospital_admissions vs aqi
ggplot(data_frame, aes(x = aqi, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs AQI", x = "AQI", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$aqi, use = "complete.obs")
modelo_aqi <- lm(hospital_admissions ~ aqi, data = data_frame)
summary(modelo_aqi)$r.squared

# Análise 3 – hospital_admissions vs pm2_5
ggplot(data_frame, aes(x = pm2_5, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs PM2.5", x = "PM2.5", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$pm2_5, use = "complete.obs")
modelo_pm2_5 <- lm(hospital_admissions ~ pm2_5, data = data_frame)
summary(modelo_pm2_5)$r.squared

# Análise 4 – hospital_admissions vs pm10
ggplot(data_frame, aes(x = pm10, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs PM10", x = "PM10", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$pm10, use = "complete.obs")
modelo_pm10 <- lm(hospital_admissions ~ pm10, data = data_frame)
summary(modelo_pm10)$r.squared

# Análise 5 – hospital_admissions vs no2
ggplot(data_frame, aes(x = no2, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs NO2", x = "NO2", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$no2, use = "complete.obs")
modelo_no2 <- lm(hospital_admissions ~ no2, data = data_frame)
summary(modelo_no2)$r.squared

# Análise 6 – hospital_admissions vs o3
ggplot(data_frame, aes(x = o3, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs O3", x = "O3", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$o3, use = "complete.obs")
modelo_o3 <- lm(hospital_admissions ~ o3, data = data_frame)
summary(modelo_o3)$r.squared

# Análise 7 – hospital_admissions vs hospital_capacity
ggplot(data_frame, aes(x = hospital_capacity, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs Leitos disponíveis", x = "Leitos disponíveis", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$hospital_capacity, use = "complete.obs")
modelo_hospital_capacity <- lm(hospital_admissions ~ hospital_capacity, data = data_frame)
summary(modelo_hospital_capacity)$r.squared

# Análise 8 – hospital_admissions vs temperature
ggplot(data_frame, aes(x = temperature, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs Temperatura", x = "Temperatura", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$temperature, use = "complete.obs")
modelo_temperature <- lm(hospital_admissions ~ temperature, data = data_frame)
summary(modelo_temperature)$r.squared

# Análise 9 – hospital_admissions vs humidity
ggplot(data_frame, aes(x = humidity, y = hospital_admissions)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Hospitalizações vs Umidade", x = "Umidade", y = "Hospitalizações") +
  theme_minimal()

cor(data_frame$hospital_admissions, data_frame$humidity, use = "complete.obs")
modelo_humidity <- lm(hospital_admissions ~ humidity, data = data_frame)
summary(modelo_humidity)$r.squared

# -----------------------------
# 4. Filtrar dados para uma cidade específica (Londres)
# -----------------------------
london_data <- subset(data_frame, city == "London")

# Criar série temporal com frequência diária
london_ts <- ts(london_data$hospital_admissions, start = c(2020, 1), frequency = 365)

# -----------------------------
# 5. Visualizar a série temporal
# -----------------------------

# Construir dataframe com datas correspondentes
london_df <- data.frame(
  date = seq.Date(from = as.Date("2020-01-01"), length.out = length(london_ts), by = "day"),
  hospital_admissions = as.numeric(london_ts)
)

# Gráfico de linhas da série temporal
ggplot(london_df, aes(x = date, y = hospital_admissions)) +
  geom_line() +
  labs(title = "Internações Hospitalares em Londres", x = "Data", y = "Internações")

# -----------------------------
# 6. Decomposição da série temporal
# -----------------------------

# Decomposição com STL (Seasonal-Trend decomposition using Loess)
decomp <- stl(london_ts, s.window = "periodic")
plot(decomp)

#-----------------------------
# 7. Criando modelos de previsão
#-----------------------------

# Criação do dataframe temporal com as variáveis relevantes
london_df <- london_data %>%
  mutate(
    date = as.Date(date),
    year = as.numeric(format(date, "%Y")),
    month = as.factor(format(date, "%m")),
    weekday = as.factor(weekdays(date))
  ) %>%
  select(
    date,
    total = hospital_admissions,
    year, month, weekday,
    aqi, pm2_5, pm10, no2, o3, temperature, humidity
  )

# -----------------------------
# 8. Separar dados de treino e teste (últimos 365 dias para teste)
# -----------------------------
indice_teste <- tail(1:nrow(london_df), 365)

treino <- london_df[-indice_teste, ]
teste  <- london_df[indice_teste, ]

# -----------------------------
# 9. Ajustar modelo de regressão múltipla com variáveis ambientais
# -----------------------------
modelo_multiplo <- lm(total ~ year + month + weekday + aqi + pm2_5 + pm10 + no2 + o3 + temperature + humidity,
                      data = treino)

# Verificar resíduos
checkresiduals(modelo_multiplo)

# -----------------------------
# 10. Prever valores no conjunto de teste
# -----------------------------
prev_mult <- predict(modelo_multiplo, newdata = teste)
real <- teste$total

# Calcular erro absoluto
abs_error <- function(yreal, yprev) {
  abs(yreal - yprev)
}
erro <- abs_error(real, prev_mult)

# -----------------------------
# 11. Visualizar previsão vs valores reais
# -----------------------------
comparacao_df <- data.frame(
  date = teste$date,
  real = real,
  predito = prev_mult
)

ggplot(comparacao_df, aes(x = date)) +
  geom_line(aes(y = real, color = "Real")) +
  geom_line(aes(y = predito, color = "Predito")) +
  labs(title = "Internacoes: Real vs Predito (Regressao com Variaveis Ambientais)",
       x = "Data", y = "Internacoes") +
  scale_color_manual(name = "Legenda", values = c("Real" = "black", "Predito" = "blue")) +
  theme_minimal()

# -----------------------------
# 12. Métricas de erro
# -----------------------------
mae <- mean(erro)
rmse <- sqrt(mean(erro^2))
cat("MAE:", mae, "\nRMSE:", rmse, "\n")


#----------------------------------------------
# SCRIPT COMPLETO PARA ANÁLISE SARIMA MENSAL
#----------------------------------------------

# Carregar pacotes
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Preparar os dados
# Agrupar os dados por mês, calculando a média de hospitalizações
mensal_data <- data_frame %>%
  mutate(mes = floor_date(as.Date(date), unit = "month")) %>%
  group_by(mes) %>%
  summarise(hospital_admissions = mean(hospital_admissions, na.rm = TRUE))

# 2. Criar série temporal mensal
mensal_ts <- ts(mensal_data$hospital_admissions, start = c(2020, 1), frequency = 12)

# 3. Separar treino (tudo menos os últimos 12 meses) e teste (últimos 12 meses)
n <- length(mensal_ts)
treino <- window(mensal_ts, end = c(2020 + (n - 13)/12))
teste <- window(mensal_ts, start = c(2020 + (n - 12)/12))

# 4. Ajustar modelo SARIMA automático (mensal)
modelo_sarima <- auto.arima(treino, seasonal = TRUE,
                            stepwise = TRUE, approximation = TRUE)

# 5. Diagnóstico dos resíduos
checkresiduals(modelo_sarima)

# 6. Fazer previsões para o período de teste
previsao <- forecast(modelo_sarima, h = length(teste))

# 7. Avaliar desempenho
real <- as.numeric(teste)
previsto <- as.numeric(previsao$mean)

mae <- mean(abs(real - previsto))
rmse <- sqrt(mean((real - previsto)^2))

# 8. Exibir métricas
cat("Modelo SARIMA Mensal\n")
cat("MAE:", round(mae, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n")

# Cria um data frame com datas mensais, valores reais e preditos
comparacao_df <- data.frame(
  date = mensal_data$mes[(n - 11):n],  # últimos 12 meses (mesmo índice do teste)
  real = as.numeric(teste),
  predito = as.numeric(previsao$mean)
)

# Plotando Real vs Predito
ggplot(comparacao_df, aes(x = date)) +
  geom_line(aes(y = real, color = "Real")) +
  geom_line(aes(y = predito, color = "Predito")) +
  labs(
    title = "Hospitalizacoes: Real vs Predito (SARIMA)",
    x = "Data", y = "Hospitalizacoes"
  ) +
  scale_color_manual(
    name = "Legenda",
    values = c("Real" = "black", "Predito" = "blue")
  ) +
  theme_minimal()
