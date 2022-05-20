## Problem Set 2 Question 3

## Open sample data
data <- read.table("C:/Users/John/Documents/Geog618/ProblemSet2/rainfallsamplepoints2.txt", sep = ",", header = TRUE)

## Create the extent
buffer=50000
xmax = max(data[,"easting"])+buffer
xmin = min(data[,"easting"])-buffer
ymax = max(data[,"northing"])+buffer
ymin = min(data[,"northing"])-buffer

x_extent = seq(xmin, xmax, by = buffer)
y_extent = seq(ymin, ymax, by = buffer)

surface = matrix(1:(length(y_extent)*length(x_extent)), nrow = length(y_extent), ncol = length(x_extent))
rownames(surface) = y_extent
colnames(surface) = x_extent

## Create C matrix
## Find the location of all the weather stations within the surface
tmp = as.matrix(data.frame(
  northing = findInterval(data$northing, y_extent),
  easting = findInterval(data$easting, x_extent),
  val = data$annualrainmm
))

surface[tmp[,1:2]] = tmp[,3]
surface_vector = as.vector(surface)

## Isolate the rainfall values
y_bar = surface_vector[surface_vector>0]

## Determine the index for the rainfalldata within the surface
c_prep = as.data.frame(cbind(
  val = y_bar,
  col_ = lapply(
    y_bar,
    FUN = function(i)
      match(i, surface_vector)
  )
))

## Create empty C matrix
C = matrix(0, length(y_bar),length(surface))

## Populate C matrix with known values represented as one
j=1
for(i in c_prep$col_){
  C[j, i] = 1
  j = j+1
}

## Create L matrix
l_prep = data.frame(
  col = rep(colnames(surface), each = nrow(surface)),
  row = rep(rownames(surface), ncol(surface)),
  value = ifelse(as.vector(surface)>0,1,0),
  id = seq.int(length(surface))
  #og_row = seq.int(nrow(surface))
  )

## Find all neighbours to the right
combinations = (nrow(surface)-1)*(ncol(surface)) + (nrow(surface))*(ncol(surface)-1)
