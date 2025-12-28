# =========================================================
# BIBLIOTECAS
# =========================================================
library(united)
library(daltoolbox)
library(daltoolboxdp)
library(ggplot2)
library(gridExtra)

# =========================================================
# FUNÇÕES AUXILIARES
# =========================================================

# Janela deslizante
ts_data <- function(data, sw) {
  n <- length(data)
  if (n < sw) return(NULL)
  mat <- matrix(NA, nrow = n - sw + 1, ncol = sw)
  for (i in 1:nrow(mat)) {
    mat[i, ] <- data[i:(i + sw - 1)]
  }
  as.data.frame(mat)
}

# Normalização Min-Max global
ts_norm_gminmax <- function() {
  structure(list(min = NULL, max = NULL), class = "ts_norm_gminmax")
}

fit.ts_norm_gminmax <- function(obj, data) {
  m <- as.matrix(data)
  obj$min <- min(m)
  obj$max <- max(m)
  obj
}

transform.ts_norm_gminmax <- function(obj, data) {
  m <- as.matrix(data)
  as.data.frame((m - obj$min) / (obj$max - obj$min + 1e-8))
}

# CUSUM simples bilateral
cusum_step <- function(x, k, h, pos, neg) {
  pos <- max(0, pos + x - k)
  neg <- min(0, neg + x + k)
  
  alarm <- (pos > h) | (neg < -h)
  
  list(pos = pos, neg = neg, alarm = alarm)
}

# =========================================================
# DADOS
# =========================================================

data(oil_3w_Type_1)
df <- oil_3w_Type_1[[1]]

series <- df$p_tpt
labels <- df$event
n <- length(series)

# =========================================================
# PARÂMETROS
# =========================================================

WARMUP        <- 500
WINDOW_SIZE  <- 10
LATENT_SIZE  <- 3
ERR_NORM_WIN <- 100      # janela de normalização do erro
CUSUM_INIT   <- 150

K_FACTOR <- 0.25
H_FACTOR <- 3

# =========================================================
# TREINO INICIAL DO AUTOENCODER
# =========================================================

train_init <- series[1:WARMUP]
ts_train <- ts_data(train_init, WINDOW_SIZE)

norm <- ts_norm_gminmax()
norm <- fit(norm, ts_train)
ts_train_norm <- transform(norm, ts_train)

ae <- autoenc_ed(WINDOW_SIZE, LATENT_SIZE)
ae <- fit(ae, ts_train_norm)

# =========================================================
# LOOP ONLINE — ERRO NORMALIZADO + CUSUM
# =========================================================

error_raw   <- rep(NA, n)
error_norm  <- rep(NA, n)
alarm_flag  <- rep(0, n)

pos <- 0
neg <- 0
error_buffer <- c()
alarm_idx <- c()

for (t in (WARMUP + WINDOW_SIZE):n) {
  
  # janela atual
  win <- series[(t - WINDOW_SIZE + 1):t]
  win_df <- as.data.frame(t(win))
  win_norm <- transform(norm, win_df)
  
  rec <- transform(ae, win_norm)
  
  # 1) erro bruto
  e_raw <- mean((as.matrix(win_norm) - as.matrix(rec))^2)
  error_raw[t] <- e_raw
  error_buffer <- c(error_buffer, e_raw)
  
  # 2) erro NORMALIZADO LOCALMENTE
  if (t > ERR_NORM_WIN) {
    e_scaled <- scale(error_raw[(t - ERR_NORM_WIN):t])
    e_t <- as.numeric(e_scaled[length(e_scaled)])
    error_norm[t] <- e_t
  } else {
    next
  }
  
  # 3) inicializa parâmetros do CUSUM
  if (length(error_buffer) == CUSUM_INIT) {
    sigma <- sd(error_buffer, na.rm = TRUE)
    k <- K_FACTOR 
    h <- H_FACTOR 
  }
  
  # 4) CUSUM
  if (length(error_buffer) >= CUSUM_INIT && sigma > 0) {
    e_bar <- mean(error_norm[(t - 20):t], na.rm = TRUE)
    cs <- cusum_step(e_bar, k, h, pos, neg)
    pos <- cs$pos
    neg <- cs$neg
    
    if (cs$alarm) {
      alarm_flag[t] <- 1
      alarm_idx <- c(alarm_idx, t)
      pos <- 0
      neg <- 0
    }
  }
}

# =========================================================
# DATAFRAMES PARA PLOT
# =========================================================

df_plot <- data.frame(
  Index = 1:n,
  Series = series,
  ErrorNorm = error_norm,
  Alarm = alarm_flag
)

df_anom <- data.frame(
  Index = which(labels == 1),
  Value = series[labels == 1]
)

df_alarm <- df_plot[df_plot$Alarm == 1, ]

# =========================================================
# GRÁFICOS
# =========================================================

g1 <- ggplot(df_plot, aes(x = Index)) +
  geom_line(aes(y = Series), color = "gray40") +
  geom_point(
    data = df_alarm,
    aes(y = Series, color = "Alarme CUSUM"),
    shape = 17, size = 3
  ) +
  geom_point(
    data = df_anom,
    aes(x = Index, y = Value, color = "Anomalia Real"),
    shape = 4, size = 3
  ) +
  scale_color_manual(
    name = "Legenda",
    values = c("Alarme CUSUM" = "blue", "Anomalia Real" = "black")
  ) +
  labs(
    title = "Série temporal com CUSUM sobre erro normalizado do Autoencoder",
    y = "Pressão"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

g2 <- ggplot(df_plot, aes(x = Index, y = ErrorNorm)) +
  geom_line(color = "red") +
  geom_hline(yintercept = c(3, -3),
             linetype = "dashed",
             color = "gray40") +

  geom_point(
    data = df_plot[df_plot$Alarm == 1, ],
    aes(y = ErrorNorm),
    color = "blue",
    shape = 17,
    size = 3
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(
    title = "Erro de reconstrução normalizado (CUSUM)",
    y = "Erro normalizado",
    x = "Índice"
  ) +
  theme_minimal()


grid.arrange(g1, g2, ncol = 1, heights = c(1.2, 1))
