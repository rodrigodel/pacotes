# Instale e carregue o pacote "forecast", se ainda não o fez
# install.packages("forecast")
library(forecast)

# Carregue a série temporal AirPassengers
data("AirPassengers")

#Previsão de Média

# Criação do modelo de média
mean_model <- meanf(AirPassengers, h=12)  # Previsão para os próximos 12 pontos

# Visualização das previsões
plot(mean_model)
mean_model_values <- mean_model$mean
print(mean_model_values)

# Naive

# Criação do modelo naive
naive_model <- naive(AirPassengers, h=12)  # Previsão para os próximos 12 pontos

# Visualização das previsões
plot(naive_model)
naive_model_values <- naive_model$mean
print(naive_model_values)

#Previsão Drift

# Criação do modelo de drift
drift_model <- rwf(AirPassengers, drift=TRUE, h=12)  # Previsão para os próximos 12 pontos

# Visualização das previsões
plot(drift_model)
drift_model_values <- drift_model$mean
print(drift_model_values)

#Suavização Exponencial

# Criação do modelo de Suavização Exponencial Simples
ses_model <- ses(AirPassengers, h=12)  # Previsão para os próximos 12 pontos

# Visualização das previsões
plot(ses_model)

# Para obter os valores previstos
ses_model_values <- ses_model$mean
print(ses_model_values)

autoplot(ses_model)

#Modelo de HOlt

# Criação do modelo de Holt
holt_model <- holt(AirPassengers, h=12)  # Previsão para os próximos 12 pontos

# Visualização das previsões
plot(holt_model)

# Para obter os valores previstos
holt_model_values <- holt_model$mean
print(holt_model_values)

holt_model

#Arima

# Criação do modelo ARIMA
# auto.arima() escolhe automaticamente os melhores parâmetros ARIMA com base no AIC
arima_model <- auto.arima(AirPassengers)

# Previsão para os próximos 12 pontos
arima_forecast <- forecast(arima_model, h=12)

# Visualização das previsões
plot(arima_forecast)

# Para obter os valores previstos
arima_forecast_values <- arima_forecast$mean
print(arima_forecast_values)

arima_model

#ARMA

arma_model <- auto.arima(AirPassengers, d=0, seasonal=FALSE)

# Imprima o resumo do modelo para ver os parâmetros escolhidos
summary(arma_model)

# Previsão para os próximos 12 pontos
arma_forecast <- forecast(arma_model, h=12)

# Visualização das previsões
plot(arma_forecast)

# Para obter os valores previstos
arma_forecast_values <- arma_forecast$mean
print(arma_forecast_values)

#SARIMA

# Criação do modelo SARIMA
# auto.arima() escolhe automaticamente os melhores parâmetros SARIMA com base no AIC
sarima_model <- auto.arima(AirPassengers)

# Imprima o resumo do modelo para ver os parâmetros escolhidos
summary(sarima_model)

# Previsão para os próximos 12 pontos
sarima_forecast <- forecast(sarima_model, h=12)

# Visualização das previsões
plot(sarima_forecast)

# Para obter os valores previstos
sarima_forecast_values <- sarima_forecast$mean
print(sarima_forecast_values)

#KFAS

library(KFAS)
library(ggfortify)
model <- SSModel(
  AirPassengers ~ SSMtrend(degree=1, Q=matrix(NA)), H=matrix(NA)
)

fit <- fitSSM(model=model, inits=c(log(var(Nile)),log(var(Nile))), method="BFGS")
smoothed <- KFS(fit$model)
autoplot(smoothed)

filtered <- KFS(fit$model, filtering="mean", smoothing='none')
autoplot(filtered)

#Arima

library(forecast)
d.arima <- auto.arima(AirPassengers)
d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast)

autoplot(d.forecast, ts.colour = 'firebrick1', predict.colour = 'red',
         predict.linetype = 'dashed', conf.int = FALSE)

#Vars

library(vars)
d.vselect <- VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
d.var <- VAR(Canada, p = d.vselect, type = 'const')

autoplot(predict(d.var, n.ahead = 50), ts.colour = 'dodgerblue4',
         predict.colour = 'blue', predict.linetype = 'dashed')

#Changepoint
library(changepoint)
autoplot(cpt.meanvar(AirPassengers))

autoplot(cpt.meanvar(AirPassengers), cpt.colour = 'blue', cpt.linetype = 'solid')


#Strucchange

library(strucchange)
autoplot(breakpoints(Nile ~ 1), ts.colour = 'blue', ts.linetype = 'dashed',
         cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')

#Estatísticas

autoplot(stl(AirPassengers, s.window = 'periodic'), ts.colour = 'blue')

autoplot(acf(AirPassengers, plot = FALSE))

autoplot(acf(AirPassengers, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma')

autoplot(spec.ar(AirPassengers, plot = FALSE))

ggcpgram(arima.sim(list(ar = c(0.7, -0.5)), n = 50))

library(forecast)
ggtsdiag(auto.arima(AirPassengers))

ggfreqplot(AirPassengers)

ggfreqplot(AirPassengers, freq = 4)
