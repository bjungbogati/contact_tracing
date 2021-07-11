library(lubridate)
library(factoextra)

xy <- local_data %>% select(longitude, latitude)
xy <- scale(xy)

head(xy)

distance <- get_dist(xy)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Separate or mutate the Date/Time columns
# local_data$date <- ymd_hms(local_data$timestamp)

local_data$year <- factor(year(local_data$timestamp))
local_data$month <- factor(month(local_data$timestamp))
local_data$day <- factor(day(local_data$timestamp))
local_data$weekday <- factor(wday(local_data$timestamp))
local_data$hour <- factor(hour(local_data$timestamp))
local_data$minute <- factor(minute(local_data$timestamp))
local_data$second <- factor(round(second(local_data$timestamp)))


