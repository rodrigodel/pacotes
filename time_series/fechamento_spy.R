#' @title Análise dos Preços de Fechamento do SPY
#' @description Este script recupera e analisa os preços de fechamento do SPY (S&P 500 ETF) 
#' do período de 01/01/2021 até 31/10/2022. Ele usa vários pacotes R para realizar esta tarefa.

# Carregar as bibliotecas necessárias
library(KFAS)      # Para modelos de espaço de estados
library(tseries)   # Para análise de séries temporais
library(timeSeries) # Para objetos de séries temporais financeiras
library(zoo)        # Para trabalhar com observações ordenadas
library(quantmod)  # Para gerenciar, modelar e analisar dados quantitativos financeiros
library(forecast)  # Para funções de previsão

#' @description Recuperar os preços de fechamento para o SPY do período de 01/01/2021 até 31/10/2022
getSymbols("SPY",  from = as.Date("2021-01-01"),
           end = as.Date("2022-10-31"),
           quote = "Close")

# Armazenar os preços de fechamento em 'cls'
cls <- SPY$SPY.Close

# Calcular os retornos diários
sp_r <- dailyReturn(SPY)

# Converter para objeto de série temporal
sp_r <- as.ts(sp_r)

# Ajustar um modelo estrutural aos retornos
fit_structural  <-  SSModel(sp_r ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
fit_structural <- fitSSM(fit_structural, inits = c(0, 0),method = "BFGS")

# Exibir a matriz Q do modelo
fit_structural$model["Q"]

# Plotar o modelo ajustado
plot(fit_structural$model)

# Ajustar um modelo estrutural aos preços de fechamento
fit_structural2  <-  SSModel(cls ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
fit_structural2 <- fitSSM(fit_structural2, inits = c(0, 0),method = "BFGS")

# Exibir a matriz Q do modelo
fit_structural2$model["Q"]

# Plotar os preços de fechamento ajustados pelo modelo
plot(fit_structural2$model$y)

# Comparar os preços de fechamento originais e ajustados
plot(fit_structural2$model$y, cls)

# Destacar os preços de fechamento ajustados
plot((fit_structural2$model$y), lty = "dotted", lwd =2)

# Prever os próximos 10 preços de fechamento e plotar
fit <- forecast(fit_structural2$model$y, h = 10 )
plot(fit)





