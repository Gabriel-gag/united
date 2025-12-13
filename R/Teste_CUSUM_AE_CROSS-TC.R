library(daltoolbox)
library(daltoolboxdp)
library(united)
library(ggplot2)
library(dplyr)

ts_data <- function(data, sw) {
  n <- length(data)
  mat <- matrix(NA, nrow = n - sw + 1, ncol = sw)
  for (i in 1:nrow(mat)) {
    mat[i, ] <- data[i:(i + sw - 1)]
  }
  as.data.frame(mat)
}


ts_norm_gminmax <- function() {
  obj <- list(min = NULL, max = NULL)
  class(obj) <- "ts_norm_gminmax"
  obj
}

fit.ts_norm_gminmax <- function(obj, data) {
  obj$min <- min(as.matrix(data))
  obj$max <- max(as.matrix(data))
  obj
}

transform.ts_norm_gminmax <- function(obj, data) {
  as.data.frame((as.matrix(data) - obj$min) / (obj$max - obj$min))
}

if (!isGeneric("fit")) setGeneric("fit", function(obj, ...) standardGeneric("fit"))
if (!isGeneric("transform")) setGeneric("transform", function(obj, ...) standardGeneric("transform"))


cusum_basic <- function(x, Tc) {
  mu <- mean(x)
  S_pos <- 0
  S_neg <- 0
  alarm <- rep(0, length(x))
  
  for (i in seq_along(x)) {
    S_pos <- max(0, S_pos + (x[i] - mu))
    S_neg <- min(0, S_neg + (x[i] - mu))
    
    if (S_pos > Tc || S_neg < -Tc) {
      alarm[i] <- 1
      S_pos <- 0
      S_neg <- 0
    }
  }
  alarm
}


cusum_cross_tc <- function(error_series, Tc_low, Tc_high) {
  
  alarm_low  <- cusum_basic(error_series, Tc_low)
  alarm_high <- cusum_basic(error_series, Tc_high)
  
  final_alarm <- rep(0, length(error_series))
  
  for (i in which(alarm_low == 1)) {
    win <- max(1, i - 2):min(length(error_series), i + 2)
    if (any(alarm_high[win] == 1)) {
      final_alarm[i] <- 1
    }
  }
  
  final_alarm
}

data(oil_3w_Type_1)
df <- oil_3w_Type_1[[1]]

series <- df$p_tpt[1:1500]

series <- df$p_tpt
labels <- df$event

WINDOW_SIZE <- 5
LATENT_SIZE <- 2

ts_df <- ts_data(series, WINDOW_SIZE)

norm <- ts_norm_gminmax()
norm <- fit(norm, ts_df)
ts_norm <- transform(norm, ts_df)

model_ae <- autoenc_ed(WINDOW_SIZE, LATENT_SIZE)
model_ae <- fit(model_ae, ts_norm)

reconstruction <- transform(model_ae, ts_norm)

error_sq <- (as.matrix(ts_norm) - as.matrix(reconstruction))^2
e_t <- apply(error_sq, 1, mean)

Tc_low  <- quantile(e_t, 0.92)
Tc_high <- quantile(e_t, 0.99)

alarms <- cusum_cross_tc(e_t, Tc_low, Tc_high)

df_plot <- data.frame(
  Time = (WINDOW_SIZE):length(series),
  Serie = series[(WINDOW_SIZE):length(series)],
  Erro = e_t,
  Alarme = alarms
)

df_plot$SerieNorm <- df_plot$Serie / max(df_plot$Serie)
df_plot$ErroNorm  <- df_plot$Erro  / max(df_plot$Erro)

ggplot(df_plot, aes(x = Time)) +
  
  geom_line(
    aes(y = SerieNorm, color = "Série Temporal"),
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(y = ErroNorm, color = "Erro de Reconstrução (AE)"),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  geom_point(
    data = subset(df_plot, Alarme == 1),
    aes(y = SerieNorm, color = "Ponto de Mudança (CUSUM Cross-Tc)"),
    shape = 8,
    size = 3
  ) +
  
  scale_color_manual(
    name = "Legenda",
    values = c(
      "Série Temporal" = "gray30",
      "Erro de Reconstrução (AE)" = "red",
      "Ponto de Mudança (CUSUM Cross-Tc)" = "black"
    )
  ) +
  
  labs(
    title = "Autoencoder Vanilla + CUSUM Cross-Tc",
    x = "Tempo",
    y = "Valor Normalizado"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

