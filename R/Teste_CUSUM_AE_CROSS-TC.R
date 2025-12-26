library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(dplyr)
library(ggplot2)
library(gridExtra)


# 1. FUNÇÕES AUXILIARES


# Janela deslizante
ts_data <- function(data, sw) {
  n <- length(data)
  if (n < sw) return(NULL)
  mat <- matrix(0, nrow = n - sw + 1, ncol = sw)
  for (i in 1:nrow(mat)) {
    mat[i, ] <- data[i:(i + sw - 1)]
  }
  as.data.frame(mat)
}

# Normalização Min-Max global
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


# CUSUM ADAPTATIVO (CROSS-TC)


cusum_adaptive_indices <- function(values, warmup, retrain_size) {
  n <- length(values)
  status <- rep(0, n)

  mu <- mean(values[1:warmup])
  sdv <- sd(values[1:warmup])

  k <- 0.5 * sdv
  h <- 5 * sdv

  pos <- 0
  neg <- 0

  retraining <- FALSE
  buffer <- c()
  events <- list()

  for (i in (warmup + 1):n) {
    x <- values[i]

    if (retraining) {
      buffer <- c(buffer, x)
      status[i] <- 3

      if (length(buffer) >= retrain_size) {
        events[[length(events) + 1]] <- list(
          start = i - retrain_size + 1,
          end = i
        )
        mu <- mean(buffer)
        sdv <- sd(buffer)
        k <- 0.5 * sdv
        h <- 5 * sdv
        buffer <- c()
        retraining <- FALSE
        pos <- 0; neg <- 0
      }
      next
    }

    pos <- max(0, pos + (x - mu) - k)
    neg <- min(0, neg + (x - mu) + k)

    if (pos > h || neg < -h) {
      status[i] <- 2
      retraining <- TRUE
      pos <- 0; neg <- 0
    }
  }

  list(status = status, retrain_events = events)
}

# DADOS


data(oil_3w_Type_1)
df <- oil_3w_Type_1[[1]]

series <- df$p_tpt
labels <- df$event

WARMUP <- 500
RETRAIN <- 200
WIN <- 10
LATENT <- 3


# CUSUM


cusum <- cusum_adaptive_indices(series, WARMUP, RETRAIN)
events <- cusum$retrain_events


#  AUTOENCODER + CROSS-TC


full_error <- rep(NA, length(series))

train_ae <- function(data_vec) {
  ts <- ts_data(data_vec, WIN)
  norm <- ts_norm_gminmax()
  norm <- fit(norm, ts)
  ts_n <- transform(norm, ts)

  ae <- autoenc_ed(WIN, LATENT)
  ae <- fit(ae, ts_n)

  list(model = ae, norm = norm)
}

apply_ae <- function(model, norm, data_vec, start_idx) {

  if (length(data_vec) < WIN) {
    return(NULL)
  }

  ts <- ts_data(data_vec, WIN)
  if (is.null(ts)) return(NULL)

  ts_n <- transform(norm, ts)
  rec <- transform(model, ts_n)

  err <- rowMeans((as.matrix(ts_n) - as.matrix(rec))^2)
  idx <- start_idx + WIN - 1 + seq_along(err)

  list(err = err, idx = idx)
}

# Modelo inicial
modelA <- train_ae(series[1:WARMUP])

endA <- if (length(events) > 0) events[[1]]$start - 1 else length(series)
resA <- apply_ae(modelA$model, modelA$norm, series[1:endA], 1)

if (!is.null(resA)) {
  full_error[resA$idx] <- resA$err
}

# Retreinamentos
if (length(events) > 0) {
  for (i in seq_along(events)) {
    e <- events[[i]]
    modelN <- train_ae(series[e$start:e$end])

    next_end <- if (i < length(events)) events[[i + 1]]$start - 1 else length(series)
    resN <- apply_ae(
      modelN$model,
      modelN$norm,
      series[e$end:next_end],
      e$end
    )

    if (!is.null(resN)) {
      full_error[resN$idx] <- resN$err
    }

  }
}


# DATAFRAMES PARA PLOT


df_plot <- data.frame(
  Index = 1:length(series),
  OriginalSeries = series,
  CusumStatus = cusum$status
)


df_error <- data.frame(
  Index = which(!is.na(full_error)),
  Error = full_error[!is.na(full_error)]
)

df_error$ErrorNorm <- df_error$Error / max(df_error$Error)

rects <- if (length(events) > 0) {
  do.call(rbind, lapply(events, as.data.frame))
} else {
  data.frame(start = numeric(), end = numeric())
}

df_anom <- data.frame(
  Index = which(labels == 1),
  Value = series[labels == 1]
)


# GRÁFICOS


g1 <- ggplot() +

  # Warm-up
  geom_rect(
    aes(xmin = 1, xmax = WARMUP, ymin = -Inf, ymax = Inf),
    fill = "green", alpha = 0.1
  ) +

  # Retreinamentos
  geom_rect(
    data = rects,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "purple", alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # Série original
  geom_line(
    data = df_plot,
    aes(x = Index, y = OriginalSeries, color = "Série Original"),
    linewidth = 0.7
  ) +

  # Mudanças detectadas pelo CUSUM
  geom_point(
    data = df_plot[df_plot$CusumStatus == 2, ],
    aes(x = Index, y = OriginalSeries, color = "Mudança Detectada (CUSUM)"),
    shape = 17, size = 3
  ) +

  # Anomalias reais
  geom_point(
    data = df_anom,
    aes(x = Index, y = Value, color = "Anomalia Real"),
    shape = 4, size = 3, stroke = 1.3
  ) +

  scale_color_manual(
    name = "Legenda",
    values = c(
      "Série Original" = "gray40",
      "Mudança Detectada (CUSUM)" = "blue",
      "Anomalia Real" = "black"
    )
  ) +

  labs(
    title = "Série Temporal com Supervisão CUSUM e Anomalias Reais",
    subtitle = "Verde: treino inicial | Roxo: retreinamento | ▲ CUSUM | ✕ anomalia real",
    y = "Pressão",
    x = "Índice temporal"
  ) +

  theme_minimal() +
  theme(legend.position = "bottom")


df_error$ErrorNorm <- df_error$Error / max(df_error$Error, na.rm = TRUE)

g2 <- ggplot(df_error, aes(x = Index, y = ErrorNorm)) +

  # Warm-up
  geom_rect(
    xmin = 1, xmax = WARMUP,
    ymin = -Inf, ymax = Inf,
    fill = "green", alpha = 0.1
  ) +

  # Retreinamentos
  geom_rect(
    data = rects,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "purple", alpha = 0.1,
    inherit.aes = FALSE
  ) +

  geom_line(
    color = "red",
    linewidth = 0.7
  ) +

  labs(
    title = "2. Erro de Reconstrução do Autoencoder",
    subtitle = "Erro calculado ao final de cada janela temporal",
    y = "Erro Normalizado",
    x = "Índice"
  ) +

  theme_minimal()
grid.arrange(
  g1,
  g2,
  ncol = 1,                 # um embaixo do outro
  heights = c(1.2, 1)       # dá mais espaço para a série original
)
