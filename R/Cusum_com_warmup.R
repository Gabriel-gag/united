library(united)
library(daltoolbox)
library(harbinger)
library(dplyr)
library(ggplot2)

# Carrega dataset e seleciona primeira poço
data(oil_3w_Type_1)
df_instancia <- oil_3w_Type_1[[1]]

# CONFIGURAÇÃO
START_INDEX <- 1 # Índice de início da análise
series_full <- df_instancia$p_tpt
labels_full <- df_instancia$event

# Descartar as amostras iniciais e reajustar o índice
series <- series_full[START_INDEX:length(series_full)]
labels <- labels_full[START_INDEX:length(labels_full)]

#CONFIGURAÇÕES DE WARMUP E MONITORAMENTO
warmup_len <- 500 # 500 amostras para aprender o regime normal (8000 a 8500)
SLEEP_WINDOW <- 500 # Janela de Silêncio

cat(paste("Amostras descartadas (Queda Inicial):", START_INDEX - 1, "\n"))
cat(paste("Período de Warmup (Treino): Amostras", START_INDEX, "a", START_INDEX + warmup_len - 1, "(Série Original).\n"))
cat(paste("Período de Monitoramento Inicia em:", START_INDEX + warmup_len, "(Série Original).\n"))
cat(paste("Janela de Silêncio (Sleep Window):", SLEEP_WINDOW, "amostras.\n"))

#  CÁLCULO DA BASELINE E PARÂMETROS ESTATÍSTICOS
warmup_data <- series[1:warmup_len]
mu_target <- mean(warmup_data)
sigma_target <- sd(warmup_data)

#  Parâmetros do CUSUM Tabular (Baseados em Sigma)
k_param <- 0.5 * sigma_target     # Offset de penalidade (k = 0.5 * sigma)
h_std <- 5 * sigma_target         # Limiar para Standard CUSUM (h = 5 * sigma)
h_low <- 3 * sigma_target         # Limiar Baixo para DT (Alerta)
h_high <- 5 * sigma_target        # Limiar Alto para DT (Confirmação)


# FUNÇÕES CUSUM TABULAR COM JANELA DE SILÊNCIO

# Função 1: CUSUM Standard Tabular (Detecção Única)
cusumStandardRobust <- function(values, mu_ref, k, threshold, warmup_samples, sleep_size){
  n <- length(values)
  detection <- rep(FALSE, n)

  posCusum <- 0
  negCusum <- 0
  sleep_counter <- 0

  # Começa DEPOIS do warmup
  for(i in (warmup_samples + 1):n){
    x <- values[i]

    if (sleep_counter > 0) {
      sleep_counter <- sleep_counter - 1
      next # Pula a iteração (Detecção Única)
    }

    posCusum <- max(0, posCusum + (x - mu_ref) - k)
    negCusum <- min(0, negCusum + (x - mu_ref) + k)

    if(posCusum > threshold | negCusum < -threshold){
      detection[i] <- TRUE
      posCusum <- 0
      negCusum <- 0
      sleep_counter <- sleep_size
    }
  }
  return(detection)
}

# Função 2: CUSUM Double Threshold Tabular (Permite Alerta Contínuo, Bloqueia Reset)
cusumDoubleThresholdRobust <- function(values, mu_ref, k, lowTC, highTC, warmup_samples, sleep_size){
  n <- length(values)
  # 0 = Normal, 1 = Warning (Low), 2 = Change (High)
  status <- rep(0, n)

  posCusum <- 0
  negCusum <- 0
  sleep_counter <- 0

  for(i in (warmup_samples + 1):n){
    x <- values[i]

    if (sleep_counter > 0) {
      sleep_counter <- sleep_counter - 1
    }

    # Atualiza acumuladores com penalidade k
    posCusum <- max(0, posCusum + (x - mu_ref) - k)
    negCusum <- min(0, negCusum + (x - mu_ref) + k)

    # Verifica Limiar Baixo (Alerta/Sensibilidade alta)
    if(posCusum > lowTC | negCusum < -lowTC){
      status[i] <- 1 # Alerta de possível mudança
    }

    # Verifica Limiar Alto (Mudança de Regime)
    if(sleep_counter == 0 && (posCusum > highTC | negCusum < -highTC)){
      status[i] <- 2 # Mudança confirmada
      posCusum <- 0  # Reseta após confirmação
      negCusum <- 0
      sleep_counter <- sleep_size # Inicia o período de silêncio
    }
  }
  return(status)
}


#  APLICAÇÃO

# Standard CUSUM (Tabular com Sleep Window)
det_std <- cusumStandardRobust(series, mu_ref = mu_target, k = k_param,
                               threshold = h_std, warmup_samples = warmup_len,
                               sleep_size = SLEEP_WINDOW)

# Double Threshold (Tabular com Alerta Contínuo)
det_dt <- cusumDoubleThresholdRobust(series, mu_ref = mu_target, k = k_param,
                                     lowTC = h_low, highTC = h_high,
                                     warmup_samples = warmup_len,
                                     sleep_size = SLEEP_WINDOW)


#  AVALIAÇÃO E PLOTAGEM

# Avaliação
labels_monitored <- labels[(warmup_len + 1):length(labels)]
det_std_monitored <- det_std[(warmup_len + 1):length(det_std)]
det_dt_monitored_any <- det_dt[(warmup_len + 1):length(det_dt)]

# Avaliação:
ev_std <- evaluate(har_eval_soft(sw = 90), det_std_monitored, labels_monitored)
ev_dt_any <- evaluate(har_eval_soft(sw = 90), det_dt_monitored_any > 0, labels_monitored)

metrics <- data.frame(
  Method = c("Standard CUSUM (Tabular)", "Double Threshold CUSUM (Tabular)"),
  k_param = c(round(k_param, 2), round(k_param, 2)),
  Accuracy = c(ev_std$accuracy, ev_dt_any$accuracy),
  F1 = c(ev_std$F1, ev_dt_any$F1)
)

cat("\n Tabela Comparativa de Métricas (CUSUM Tabular) \n")
print(metrics)


# Plotagem

# Definições comuns
index_full <- START_INDEX:(START_INDEX + length(series) - 1)
warmup_end_index_original <- START_INDEX + warmup_len - 1
anomaly_indices <- which(labels) + START_INDEX - 1 # DEFINIDO AQUI

# Layout: 2 linhas, 1 coluna (um gráfico em cima do outro)
par(mfrow = c(2, 1))


# GRÁFICO 1: Standard CUSUM

plot(index_full, series, type = "l",
     main = "Standard CUSUM (Detecção Única)",
     ylab = "Pressão (p_tpt)", xlab = "Index Original", col = "gray")

# Elementos visuais
abline(v = warmup_end_index_original, col = "gray", lty = 2)
text(warmup_end_index_original, max(series), "Fim do Warmup", pos = 4, col = "gray", cex=0.8)

# Pontos de detecção
points(which(det_std == TRUE) + START_INDEX - 1, series[which(det_std == TRUE)], col = "blue", pch = 16)
# Anomalia Real
points(anomaly_indices, series[which(labels)], col = "black", pch = 8, cex = 1.5)

legend("topright",
       legend = c("Série", "Anomalia Real", "Detecção (Reset)"),
       col = c("gray", "black", "blue"),
       pch = c(NA, 8, 16), lty = c(1, NA, NA), cex = 0.8)



# GRÁFICO 2: Double Threshold CUSUM

plot(index_full, series, type = "l",
     main = "Double Threshold CUSUM (Alerta + Detecção)",
     ylab = "Pressão (p_tpt)", xlab = "Index Original", col = "gray")

# Elementos visuais
abline(v = warmup_end_index_original, col = "gray", lty = 2)
text(warmup_end_index_original, max(series), "Fim do Warmup", pos = 4, col = "gray", cex=0.8)

# Pontos de detecção
# Nível 1 (Alerta) - Desenhado PRIMEIRO para ficar no fundo
points(which(det_dt == 1) + START_INDEX - 1, series[which(det_dt == 1)], col = "orange", pch = 15, cex=0.8)
# Nível 2 (Reset) - Desenhado DEPOIS para ficar por cima
points(which(det_dt == 2) + START_INDEX - 1, series[which(det_dt == 2)], col = "red", pch = 17, cex=1.2)
# Anomalia Real
points(anomaly_indices, series[which(labels)], col = "black", pch = 8, cex = 1.5)

legend("topright",
       legend = c("Série", "Anomalia Real", "Nível 2 (Reset)", "Nível 1 (Alerta)"),
       col = c("gray", "black", "red", "orange"),
       pch = c(NA, 8, 17, 15), lty = c(1, NA, NA, NA), cex = 0.8)

# Resetar layout para o padrão
par(mfrow = c(1, 1))
