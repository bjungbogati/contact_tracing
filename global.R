
pacman::p_load('sf', 'shiny', 'shinydashboard', 'shinyBS' , 'leaflet', 'RColorBrewer', 'scales', 'lattice', 'dplyr', 'shinyjs', 'leaflet.extras', 'plotly', 'ggthemes', 'firebase', 'DT')


district <- readRDS("./data/district.rds")
local_data <- readRDS("./data/local_data.rds")
sum_data <- readRDS("./data/sum_data.rds")

local_data <- local_data %>% distinct(id, .keep_all = T)
local_data <- local_data[sample(nrow(local_data), 1200, replace = T), ]

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)


# local_data <- local_data[1:2000, ]



# local_data %>% count(cluster)
  
# v <- sample(local_data, size = 10, replace = FALSE)

