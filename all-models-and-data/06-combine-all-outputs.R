library(yotover)

load("all-models-and-data/01-chapter1-traditional-gravity.RData")
load("all-models-and-data/02-chapter1-distance-puzzle.RData")
load("all-models-and-data/03-chapter1-regional-trade-agreements.RData")
load("all-models-and-data/04-chapter2-trade-without-borders.RData")
load("all-models-and-data/05-chapter2-impact-of-rtas.RData")

save.image("all-models-and-data/all-models-and-data.RData", compress = "xz")
