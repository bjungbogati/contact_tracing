library(tidyverse)
library(geosphere)
library(cluster)

xy <- local_data %>% select(longitude, latitude)

xy$cluster = clusters(cgraph)$membership

dmat = outer(1:nrow(xy), 1:nrow(xy), function(i,j)distHaversine(xy[i,],xy[j,]))

cmat = dmat < 15000

require(igraph)
cgraph = graph.adjacency(cmat)

plot(cgraph)

clusters(cgraph)$membership

xy$cluster = clusters(cgraph)$membership