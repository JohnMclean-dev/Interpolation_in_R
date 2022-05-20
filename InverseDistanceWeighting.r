## Problem Set 2 Question 3

## Open sample data
data <- read.table("~/Documents/Geog618/rainfallsamplepoints2.txt", sep = ",", header = TRUE)
rainData <- data[,c(8,9,5)]

## Isolate Davidson from the other stations

Davidson <- rainData[8,]
rainData_short <- rainData[-8,]

## Calculate distance
lng = Davidson[1,"easting"]
lat = Davidson[1,"northing"]
distance = function(x){
  return(
    sqrt(
      (lng - as.numeric(x["easting"]))^2 + (lat - as.numeric(x["northing"]))^2
    )
  )
}
rainData_short$distance = apply(rainData_short, MARGIN = 1, distance)

## Calculate inverse distance
inv_dist = function(x){
  return(
    1 / x["distance"]
  )
}
rainData_short$inverse_distance = apply(rainData_short, MARGIN = 1, inv_dist)

# Sum of the inverse distances
inv_dist_sum = sum(rainData_short["inverse_distance"])

# Calculate the Normed I.D. (weights)
weights = function(x){
  return(
    x["inverse_distance"] / inv_dist_sum
  )
}
rainData_short$NormId = apply(rainData_short, MARGIN = 1, weights)

# Calculate the weighted values
weight_val = function(x){
  return(
    x["annualrainmm"] * x["NormId"]
  )
}
rainData_short$weightVal = apply(rainData_short, MARGIN = 1, weight_val)

# Sum the weighted values for the IDW value
IDW = sum(rainData_short$weightVal)