## Problem Set 2 Question 3

## Open sample data
data <- read.table("~/Documents/Geog618/rainfallsamplepoints2.txt", sep = ",", header = TRUE)
rainData <- data[,c(8,9)]
rainDatamm <- as.matrix(data[-8,c(5)])

## Isolate davidson location
Davidson <- rainData[8,]
rainData_short <- rainData[-8,]

## Distance matrix of all known coordinates
Matrix00 <- as.matrix(dist(rainData_short, method = "euclidean", diag = TRUE))