library(united)
library(daltoolbox)
library(harbinger)
library(dplyr)
library(ggplot2)

# Carrega dataset
data(oil_3w_Type_1)
df_instancia <- oil_3w_Type_1[[1]]

# --- CONFIGURAÇÃO INICIAL ---
START_INDEX <- 1 
series_full <- df_instancia$p_tpt 
labels_full <- df_instancia$event

# Recorte da série
series <- series_full[START_INDEX:length(series_full)]
labels <- labels_full[START_INDEX:length(labels_full)] 

# --- CONFIGURAÇÃO DO ADAPTATIVO ---
INITIAL_WARMUP <- 500  # Tamanho do treino inicial
RETRAIN_SIZE <- 200    # Quantas amostras usar para reaprender após uma mudança

cat(paste("Tamanho da Série:", length(series), "\n"))
cat(paste("Warmup Inicial:", INITIAL_WARMUP, "\n"))
cat(paste("Tamanho do Retreino:", RETRAIN_SIZE, "\n"))


# --- FUNÇÃO CUSUM ADAPTATIVO TABULAR (DOUBLE THRESHOLD) ---
cusumAdaptiveRobust <- function(values, initial_warmup, retrain_size){
  n <- length(values)
  status <- rep(0, n)        # Vetor de detecções (0=Normal, 1=Alerta, 2=Mudança, 3=Retreino)
  mu_dynamic <- rep(NA, n)   # Para plotagem da média vigente
  
  # 1. TREINAMENTO INICIAL
  training_data <- values[1:initial_warmup]
  
  # Calcula estatísticas iniciais
  mu_ref <- mean(training_data)
  sigma_ref <- sd(training_data)
  
  # Define parâmetros do CUSUM Tabular (Robustos)
  k <- 0.5 * sigma_ref      # Folga
  h_low <- 3 * sigma_ref    # Alerta (3 sigmas)
  h_high <- 5 * sigma_ref   # Mudança Crítica (5 sigmas)
  
  # Preenche histórico inicial
  mu_dynamic[1:initial_warmup] <- mu_ref
  
  posCusum <- 0
  negCusum <- 0
  
  # Controle de Retreino
  is_retraining <- FALSE
  retrain_buffer <- c()
  
  # 2. LOOP DE MONITORAMENTO
  for(i in (initial_warmup + 1):n){
    x <- values[i]
    
    # --- MODO RETREINO ---
    if(is_retraining){
      retrain_buffer <- c(retrain_buffer, x)
      status[i] <- 3 # Código 3 = Retreinando
      mu_dynamic[i] <- mu_ref # Mantém visualmente a média antiga
      
      # Se completou o tamanho do buffer, recalcula tudo
      if(length(retrain_buffer) >= retrain_size){
        mu_ref <- mean(retrain_buffer)
        sigma_ref <- sd(retrain_buffer)
        
        # Atualiza parâmetros dinamicamente
        k <- 0.5 * sigma_ref
        h_low <- 3 * sigma_ref
        h_high <- 5 * sigma_ref
        
        # Sai do modo retreino
        is_retraining <- FALSE
        retrain_buffer <- c()
        posCusum <- 0
        negCusum <- 0
        
        cat(paste("-> [Retreino] Índice:", i, "| Nova Média:", round(mu_ref, 2), "| Novo Sigma:", round(sigma_ref, 2), "\n"))
      }
      next # Pula o cálculo do CUSUM nesta iteração
    }
    
    # --- MODO MONITORAMENTO ---
    mu_dynamic[i] <- mu_ref
    
    # Cálculo do CUSUM Tabular
    posCusum <- max(0, posCusum + (x - mu_ref) - k)
    negCusum <- min(0, negCusum + (x - mu_ref) + k)
    
    # Verifica Nível 1 (Alerta)
    if(posCusum > h_low | negCusum < -h_low){
      status[i] <- 1
    }
    
    # Verifica Nível 2 (Mudança Confirmada) -> GATILHO DE RETREINO
    if(posCusum > h_high | negCusum < -h_high){
      status[i] <- 2
      # Ao confirmar a mudança, entramos em modo de aprendizado do novo padrão
      is_retraining <- TRUE
      posCusum <- 0 
      negCusum <- 0
    }
  }
  
  return(list(status = status, mu_series = mu_dynamic))
}

# --- APLICAÇÃO ---

# Roda o modelo adaptativo
resultado <- cusumAdaptiveRobust(series, 
                                 initial_warmup = INITIAL_WARMUP, 
                                 retrain_size = RETRAIN_SIZE)

det_status <- resultado$status
mu_series <- resultado$mu_series

# --- AVALIAÇÃO (Focada nas detecções de Nível 2 - Mudança de Regime) ---

# Ajustando vetores para avaliação (pós-warmup)
labels_eval <- labels[(INITIAL_WARMUP + 1):length(labels)]
det_eval <- det_status[(INITIAL_WARMUP + 1):length(det_status)]

# Avalia apenas as mudanças confirmadas (Nível 2)
ev <- evaluate(har_eval_soft(sw = 90), det_eval == 2, labels_eval)

cat("\n--- Matriz de Confusão (CUSUM Adaptativo Tabular) ---\n")
print(ev$confMatrix)
cat(paste("Accuracy:", round(ev$accuracy, 4), "\n"))
cat(paste("F1 Score:", round(ev$F1, 4), "\n"))


# --- PLOTAGEM COM GGPLOT2 ---

index_full <- START_INDEX:(START_INDEX + length(series) - 1)

# Dataframe para ggplot
df_plot <- data.frame(
  Index = index_full,
  Pressure = as.numeric(series),
  Mean = mu_series,
  Status = det_status,
  RealAnomaly = labels
)

# Plotagem
g <- ggplot(df_plot, aes(x = Index, y = Pressure)) +
  # 1. Série Temporal (Fundo)
  geom_line(color = "gray60", alpha=0.7) +
  
  # 2. Média Dinâmica (Linha Azul Tracejada)
  geom_line(aes(y = Mean, color = "Média Adaptativa"), linetype = "dashed", linewidth = 0.8) +
  
  # 3. Pontos de Retreino (Verde) - Espaçados (a cada 50 pontos) para visualização mais limpa
  geom_point(data = subset(df_plot, Status == 3 & Index %% 50 == 0), 
             aes(color = "Retreinando"), shape = 4, size = 2, stroke = 1) +
  
  # 4. Pontos de Alerta (Laranja)
  geom_point(data = subset(df_plot, Status == 1), aes(color = "Alerta"), size = 1) +
  
  # 5. Pontos de Mudança (Vermelho) - Destacados
  geom_point(data = subset(df_plot, Status == 2), aes(color = "Mudança"), shape = 17, size = 3) +
  
  # 6. Anomalia Real (Estrela Preta)
  geom_point(data = subset(df_plot, RealAnomaly == TRUE), aes(color = "Real"), shape = 8, size = 4, stroke=1.2) +
  
  # Configuração de Cores e Legenda
  scale_color_manual(name = "Legenda", 
                     values = c("Média Adaptativa" = "blue",
                                "Retreinando" = "darkgreen", 
                                "Alerta" = "orange", 
                                "Mudança" = "red", 
                                "Real" = "black"),
                     breaks = c("Real", "Mudança", "Alerta", "Retreinando", "Média Adaptativa")) +
  
  labs(title = "CUSUM Tabular Adaptativo (Retreino Dinâmico)", 
       subtitle = paste("P_TPT | Warmup:", INITIAL_WARMUP, "| Retrain Size:", RETRAIN_SIZE),
       y = "Pressão", x = "Índice Original") +
  
  theme_minimal() +
  theme(legend.position = "bottom")

print(g)