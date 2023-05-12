# https://towardsdatascience.com/queueing-models-with-r-a794c78e6820

# Import queueing package
library(queueing)

# Vamos analisar uma fila com uma taxa de chegada de 13 demandas por semana, com um time de 7 pessoas e cada integrante do time consegue fazer 2.14 demandas por semana.

# Set queue model input parameters
input_mm1 <- NewInput.MMC(lambda=13, mu=2.1428, c=7, n=0, method=0)

# Create queue class object
output_mm1 <- QueueingModel(input_mm1)

# Get queue model report
Report(output_mm1)

# Get queue model summary
summary(output_mm1)

# Summary Output Nomenclatures
# RO (ρ): overall system utilization (Utilização geral do sistema)
# P0: the probability that all servers are idle (Probabilidade de todos as pessoas ficarem ociosas)
# Lq: long-run average number of customers in the queue ()
# Wq: long-run average time spent in the queue (Temopo médio de longo prazo na fila, na mesma unidade, no exemplo são 13 por semana)
# X: system throughput (Taxa de vazão do sistema)
# L: long-run average number of customers in the system
# W: long-run average time spent in the system
# Wqq: long-run average time spent in queue when there is queue in a queueing model
# Lqq: long-run average number of customers in queue when there is queue in a queueing model

curve(dpois(x, input_mm1$lambda),
      from = 0, 
      to = 20, 
      type = "b", 
      lwd = 2,
      xlab = "Number of customers",
      ylab = "Probability",
      main = "Poisson Distribution for Arrival Process",
      ylim = c(0, 0.25),
      n = 21)

curve(dexp(x, rate = 1/input_mm1$lambda),
      from = 0, 
      to = 10,
      type = "l", 
      lwd = 2,
      xlab = "Interarrival Time",
      ylab = "Probaility",
      main = "Exponential Distribution for Interarrival Time",
      ylim = c(0, 1))
abline(h = 0)

curve(dexp(x, rate = input_mm1$mu),
      from = 0, 
      to = 5, 
      type = "l", 
      lwd = 2,
      xlab = "Service Waiting Time",
      ylab = "Probaility",
      main = "Exponential Distribution for Service Process",
      ylim = c(0, 1))
abline(h = 0)
