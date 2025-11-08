source("etl/ucr_anomaly/create_labels.R")
ucr_sample <- create_labels(ucr)


for (i in 1:length(ucr)){
  plot(as.ts(ucr[[i]]),
       main=names(ucr[i]))
}


for (i in 1:length(ucr)){
  print(names(ucr[i]))
  print("Events: ", end="")
  print(sum(ucr_sample[[i]]$event))
}






ucr_label_interval <- ucr
file <- "etl/ucr_anomaly/source/ucr_label_interval.RData"
save(ucr_label_interval, file=file, compress = "xz")

names(ucr)

ucr <- ucr_sample

for (i in 1:length(ucr)){
  plot(as.ts(ucr[[i]]),
       main=names(ucr[i]))
}

file <- "etl/ucr_anomaly/source/ucr.RData"
save(ucr, file=file, compress = "xz")





