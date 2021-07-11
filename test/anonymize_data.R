set.seed(1)

n_row <- nrow(local_data)
# library(ggplot2)
# ggplot(local_data, aes(x = longitude, y = latitude)) +
#   geom_line()

# seq(-4,4,length=200)

x <- rnorm(n = n_row, mean=0.02, sd=0.02)

r_long <- local_data$longitude + x
r_lat <- local_data$latitude + x

df <- data.frame(r_long, r_lat)

library(ggplot2)

ggplot() +
  geom_line(data = df, aes(x = r_long, y = r_lat, color = "green")) +
  geom_line(data = local_data, aes(x = longitude, y = latitude, color = "red"))




library(stringr)

n_row

local_data$contact + 10


str_sub(local_data$contact, start= -4)







