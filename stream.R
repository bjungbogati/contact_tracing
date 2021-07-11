library(dplyr) 
library(lubridate)

url <- "http://202.51.3.120/data/query-custom-result"
#body <- '{"entity": "hl7.org.fhir/Encounter","entity-filter": {"wiseyak.org/Encounter.type": ["hl7.org.fhir/nCOV-CSR-SelfReport"]},"return-json": "Condition: <hl7.org.fhir/Encounter.status> uid_enc: uid <hl7.org.fhir/Encounter.period>{ timestamp: <hl7.org.fhir/Period.start>} <hl7.org.fhir/Encounter.subject>{ uid: uid id: id age: <hl7.org.fhir/Patient.age> <hl7.org.fhir/Patient.telecom>{ Contact: <hl7.org.fhir/ContactPoint.value>} <hl7.org.fhir/Patient.name>{ famName: <hl7.org.fhir/HumanName.family> givName: <hl7.org.fhir/HumanName.given>}} <hl7.org.fhir/Encounter.location>{<hl7.org.fhir/Encounter.location.location>{<hl7.org.fhir/Location.position>{latitude: <hl7.org.fhir/Location.position.latitude> longitude: <hl7.org.fhir/Location.position.longitude>}}}"}'

#body <- '{"entity": "hl7.org.fhir/Encounter","entity-filter": {"wiseyak.org/Encounter.type": ["hl7.org.fhir/nCOV-CSR-SelfReport"]},"return-json": "Condition: <hl7.org.fhir/Encounter.status> <hl7.org.fhir/Encounter.period>{ timestamp: <hl7.org.fhir/Period.start>} <hl7.org.fhir/Encounter.subject>{id: id age: <hl7.org.fhir/Patient.age> <hl7.org.fhir/Patient.telecom>{ Contact: <hl7.org.fhir/ContactPoint.value>} <hl7.org.fhir/Patient.name>{ famName: <hl7.org.fhir/HumanName.family> givName: <hl7.org.fhir/HumanName.given>}} <hl7.org.fhir/Encounter.location>{<hl7.org.fhir/Encounter.location.location>{<hl7.org.fhir/Location.position>{latitude: <hl7.org.fhir/Location.position.latitude> longitude: <hl7.org.fhir/Location.position.longitude>}}}"}'
body <- '{"entity": "hl7.org.fhir/Encounter","entity-filter": {"wiseyak.org/Encounter.type": ["hl7.org.fhir/nCOV-CSR-SelfReport"]},"return-json": "Condition: <hl7.org.fhir/Encounter.status> <hl7.org.fhir/Encounter.period>{ timestamp: <hl7.org.fhir/Period.start>} <hl7.org.fhir/Encounter.subject>{id: id <hl7.org.fhir/Patient.telecom>{ Contact: <hl7.org.fhir/ContactPoint.value>} <hl7.org.fhir/Patient.name>{ famName: <hl7.org.fhir/HumanName.family> givName: <hl7.org.fhir/HumanName.given>}} <hl7.org.fhir/Encounter.location>{<hl7.org.fhir/Encounter.location.location>{<hl7.org.fhir/Location.position>{latitude: <hl7.org.fhir/Location.position.latitude> longitude: <hl7.org.fhir/Location.position.longitude>}}}"}'

r <- httr::POST(url = url, body = body, encode = "json")
local_data <- jsonlite::fromJSON(httr::content(r, as="text"))
names(local_data) <- c("condition", "contact", "fam_name", "giv_name", "id", "latitude", "longitude", "timestamp")

local_data <- local_data %>% filter(latitude < 83.81, longitude > 28.40)
local_data <- local_data %>% filter(latitude > 27.62 , longitude < 88)
local_data <- local_data %>% filter(latitude < 28.40)

# n <- local_data %>% 
#   mutate(lat = ifelse(latitude < 83.81, lat, long), 
#          long = ifelse(strand =="-", lat, long)) %>% 
#   select(sqn, start = start1, end, strand)

# 
# 26.404577 88.332042

# convert timestamp to format

local_data <- local_data %>% 
  mutate(timestamp = lubridate::ymd_hms(timestamp))


## make changes to dates into different 
local_data$timestamp[1:50] <- local_data$timestamp[1:50] + days(12) 
local_data$timestamp[50:300] <- local_data$timestamp[50:300] - days(4)
local_data$timestamp[300:600] <- local_data$timestamp[300:600] - days(3)
local_data$timestamp[600:1000] <- local_data$timestamp[600:1000] - days(2)
local_data$timestamp[1000:1300]  <- local_data$timestamp[1000:1300] - days(1) 
local_data$timestamp[1300:1500] <- local_data$timestamp[1300:1500] - days(5)
local_data$timestamp[1500:1700] <- local_data$timestamp[1500:1700] - days(6)
local_data$timestamp[1700:2000] <- local_data$timestamp[1700:2000] - days(7) 
local_data$timestamp[2000:2300] <- local_data$timestamp[2000:2300] - days(8)
local_data$timestamp[2300:2500] <- local_data$timestamp[2300:2500] - days(9) 
local_data$timestamp[2500:2700]  <- local_data$timestamp[2500:2700] - days(10)
local_data$timestamp[2700:2900] <- local_data$timestamp[2700:2900] - days(11) 
local_data$timestamp[2900:3100] <- local_data$timestamp[2900:3100] - days(12)
local_data$timestamp[3200:nrow(local_data)] <- local_data$timestamp[3200:nrow(local_data)] - days(13)

local_data <- local_data %>% 
  mutate(date = lubridate::date(timestamp)) %>% 
  mutate(time = format(timestamp, "%H:%M:%S"))
  # mutate(time = strptime(x = time, format = "%H:%M:%S"))

# filter(time2 >= "21:59:00" & time2 < "22:02:00") %>% select(-time2)
# local_data$time <- 1:nrow(local_data)

local_data$longitude <- as.numeric(local_data$longitude)
local_data$latitude <- as.numeric(local_data$latitude)
local_data$condition <- tolower(local_data$condition)

local_data$longitude <- round(local_data$longitude, 6)
local_data$latitude <- round(local_data$latitude, 6)

#### Changes to dates

local_data$date[local_data$date == "2020-04-17"] <- as.Date("2020-04-09")
local_data$date[local_data$date == "2020-04-28"] <- as.Date("2020-04-10")

# condition
# 
local_data$date[1:50] <- as.Date("2020-04-02")
local_data$date[50:300] <- as.Date("2020-04-03")
local_data$date[300:600] <- as.Date("2020-04-04")
local_data$date[600:1000] <- as.Date("2020-04-05")
local_data$date[1000:1300]  <- as.Date("2020-04-06")
local_data$date[1300:1500] <- as.Date("2020-04-07")
local_data$date[1500:1700] <- as.Date("2020-04-08")
local_data$date[1700:2000] <- as.Date("2020-04-09")
local_data$date[2000:2300] <- as.Date("2020-04-10")
local_data$date[2300:2500] <- as.Date("2020-04-11")
local_data$date[2500:2700]  <- as.Date("2020-04-12")
local_data$date[2700:2900] <- as.Date("2020-04-13")
local_data$date[2900:3100] <-as.Date("2020-04-14")
local_data$date[3200:nrow(local_data)] <- as.Date("2020-04-15")

local_data <- transform(local_data, condition = sample(condition))

# ###
# 
# local_data$condition[1:100] <- 


# district[district$district == "bhaktapur", ]

# bhk_coord <- data.frame(
#   lon = c(85.35419, 85.35826, 85.35744, 85.35907, 85.36233, 85.36966, 85.37210, 85.37373, 85.37618, 85.37781, 85.37781, 85.38025, 85.38351, 85.38676, 85.38676, 85.38921, 85.39165, 85.39084, 85.39165, 85.39491, 85.39735, 85.39817, 85.39898, 85.40468, 85.40550, 85.41038, 85.41446, 85.41446, 85.41201, 85.41364, 85.41690, 85.41771, 85.42260, 85.42342, 85.42912, 85.43075, 85.44133, 85.44133, 85.44541, 85.44785, 85.45111, 85.45355, 85.45518, 85.45762, 85.45925, 85.46170, 85.46495, 85.46740, 85.47065, 85.47147, 85.47798, 85.47880, 85.48206, 85.48939, 85.49916, 85.50568, 85.51789, 85.52278, 85.52767, 85.52197, 85.52360, 85.52278, 85.52360, 85.52360, 85.52278, 85.52360, 85.52197, 85.51545, 85.50812, 85.50568, 85.50486, 85.50079, 85.49835, 85.49835, 85.49590, 85.49672, 85.49346, 85.49265, 85.49509, 85.48694, 85.48206, 85.48043, 85.47880, 85.47880, 85.47880, 85.47473, 85.47228, 85.46658, 85.46251, 85.46170, 85.45599, 85.44948, 85.44215, 85.44052, 85.43808, 85.43075, 85.42586, 85.42097, 85.40957, 85.40794, 85.40631, 85.39898, 85.39572, 85.39328, 85.39165, 85.39002, 85.38676, 85.37699, 85.37536, 85.37373, 85.37210, 85.37210, 85.36722, 85.36559, 85.36314, 85.36559, 85.36314, 85.35419), 
#   lat = c(27.66769, 27.67589, 27.67835, 27.68245, 27.68163, 27.68819, 27.68901, 27.69229, 27.69065, 27.69065, 27.69434, 27.69311, 27.69393, 27.69229, 27.69598, 27.69680, 27.69721, 27.70090, 27.70213, 27.70254, 27.70090, 27.70172, 27.70336, 27.70541, 27.70828, 27.71156, 27.71156, 27.71320, 27.71648, 27.71730, 27.71607, 27.72017, 27.71976, 27.72345, 27.72345, 27.72509, 27.72796, 27.72591, 27.72427, 27.72550, 27.72386, 27.72140, 27.72181, 27.72058, 27.71976, 27.72058, 27.71853, 27.72017, 27.72017, 27.71730, 27.71607, 27.71730, 27.71853, 27.71853, 27.72304, 27.72345, 27.72755, 27.72796, 27.72509, 27.71812, 27.71361, 27.71033, 27.70623, 27.70049, 27.69844, 27.69393, 27.69270, 27.69147, 27.69270, 27.68901, 27.68327, 27.67548, 27.67343, 27.67015, 27.66523, 27.66277, 27.65826, 27.65580, 27.65211, 27.64719, 27.64186, 27.64473, 27.64432, 27.64227, 27.64063, 27.63489, 27.63366, 27.62546, 27.62423, 27.62792, 27.63571, 27.62874, 27.62710, 27.62341, 27.62259, 27.62464, 27.62013, 27.61972, 27.62915, 27.62956, 27.63243, 27.63284, 27.63571, 27.64104, 27.64473, 27.64350, 27.64883, 27.65129, 27.65375, 27.65334, 27.65457, 27.65990, 27.65990, 27.66236, 27.66113, 27.66482, 27.66892, 27.66769)
# )
# 
# min(bhk_coord$lon)
# max(bhk_coord$lon)
# min(bhk_coord$lat)
# max(bhk_coord$lat)


# local_data %>% filter(latitude >), 
#                       longitude < c(85.35419, 85.35826, 85.35744, 85.35907, 85.36233, 85.36966, 85.37210, 85.37373, 85.37618, 85.37781, 85.37781, 85.38025, 85.38351, 85.38676, 85.38676, 85.38921, 85.39165, 85.39084, 85.39165, 85.39491, 85.39735, 85.39817, 85.39898, 85.40468, 85.40550, 85.41038, 85.41446, 85.41446, 85.41201, 85.41364, 85.41690, 85.41771, 85.42260, 85.42342, 85.42912, 85.43075, 85.44133, 85.44133, 85.44541, 85.44785, 85.45111, 85.45355, 85.45518, 85.45762, 85.45925, 85.46170, 85.46495, 85.46740, 85.47065, 85.47147, 85.47798, 85.47880, 85.48206, 85.48939, 85.49916, 85.50568, 85.51789, 85.52278, 85.52767, 85.52197, 85.52360, 85.52278, 85.52360, 85.52360, 85.52278, 85.52360, 85.52197, 85.51545, 85.50812, 85.50568, 85.50486, 85.50079, 85.49835, 85.49835, 85.49590, 85.49672, 85.49346, 85.49265, 85.49509, 85.48694, 85.48206, 85.48043, 85.47880, 85.47880, 85.47880, 85.47473, 85.47228, 85.46658, 85.46251, 85.46170, 85.45599, 85.44948, 85.44215, 85.44052, 85.43808, 85.43075, 85.42586, 85.42097, 85.40957, 85.40794, 85.40631, 85.39898, 85.39572, 85.39328, 85.39165, 85.39002, 85.38676, 85.37699, 85.37536, 85.37373, 85.37210, 85.37210, 85.36722, 85.36559, 85.36314, 85.36559, 85.36314, 85.35419)
#                       )



# library(ggplot2)
# ggplot(local_data, aes(x = date, y = condition, color = condition)) +
#   geom_col() +
#   scale_color_manual(values = c("green", ""))




# local_data %>% 
#   slice(1:100) %>% 
#   count(condition, date) %>% 
#   ggplot(aes(x = date, y = n, color = condition)) +
#   geom_line()
#   scale_color_manual(values = c("green", ""))

## anonomize data
set.seed(1)
n_row <- nrow(local_data)

# longitude
x <- rnorm(n = n_row, mean= 0.0042, sd= 0.0042)
x <- round(x, 6)

# latitude
x1 <- rnorm(n = n_row, mean = 5, sd = 5)
x1 <- round(x, 6)

local_data$longitude <- local_data$longitude + x
local_data$latitude <- local_data$latitude + x1

y <- round(runif(n_row, 1000, 9999))

## change the number
rand_num <- stringr::str_sub(local_data$contact, end = 6)
local_data$contact <- paste(rand_num, y, sep="")

clusters <- kmeans(local_data[, 6:7], 50)
local_data$cluster <- as.factor(clusters$cluster)


### Change of data 
library(dplyr)

# local_data %>% 
#   group_by(date) %>% 
#   mutate(condition = case_when(
#     date = 
#     
#   ))

local_data$timestamp[1:50] <- local_data$timestamp[1:50] + days(12) 
local_data$timestamp[50:300] <- local_data$timestamp[50:300] - days(4)

# sum_data <- local_data %>% select(condition, id, date) %>% distinct(id, .keep_all = T) %>% 
#   group_by(date, condition) %>% 
#   summarise(count = n())


n <- data.frame(sort(round(rnorm(15, mean = 10, sd = 10))))
o <- data.frame(sort(round(rnorm(15, mean = 10, sd = 10))))
p <- data.frame(sort(round(rnorm(15, mean = 10, sd = 10))))
q <- data.frame(sort(round(rnorm(15, mean = 10, sd = 10))))

# 
# a %>% tidyr::pivot_longer(cols = 2:5, 
#                           names_to = "name", 
# values_to = "new")

a <- n %>% bind_rows(o) %>% bind_rows(p) %>% bind_rows(q)

a[a < 0] <- 0

names(a) <- "count"

sum_data <- data.frame(
  date = c("2020-04-02", "2020-04-03", "2020-04-04", "2020-04-05", "2020-04-06", 
           "2020-04-07", "2020-04-08", "2020-04-17", "2020-04-18", "2020-04-19", 
           "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24", 
           "2020-04-02", "2020-04-03", "2020-04-04", "2020-04-05", "2020-04-06", 
           "2020-04-07", "2020-04-08", "2020-04-17", "2020-04-18", "2020-04-19", 
           "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24", 
           "2020-04-02", "2020-04-03", "2020-04-04", "2020-04-05", "2020-04-06", 
           "2020-04-07", "2020-04-08", "2020-04-17", "2020-04-18", "2020-04-19", 
           "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24", 
           "2020-04-02", "2020-04-03", "2020-04-04", "2020-04-05", "2020-04-06", 
           "2020-04-07", "2020-04-08", "2020-04-17", "2020-04-18", "2020-04-19", 
           "2020-04-20", "2020-04-21", "2020-04-22", "2020-04-23", "2020-04-24"
  ), 
  condition = c("green", "green", "green", "green", "green",
                "green", "green", "green", "green", "green",
                "green", "green", "green", "green", "green",
                "blue", "blue", "blue", "blue", "blue",
                "blue", "blue", "blue", "blue", "blue",
                "blue", "blue", "blue", "blue", "blue",
                "yellow", "yellow", "yellow", "yellow", "yellow",
                "yellow", "yellow", "yellow", "yellow", "yellow",
                "yellow", "yellow", "yellow", "yellow", "yellow",
                "red", "red",  "red", "red", "red",
                "red", "red",  "red", "red", "red",
                "red", "red",  "red", "red", "red"), 
  count = a
)


sum_data$condition <- as.factor(sum_data$condition)
sum_data$date <- as.Date(sum_data$date)

saveRDS(sum_data, "./data/sum_data.rds")
saveRDS(local_data, "./data/local_data.rds")

# readr::write_csv(local_data, "./data/local_data.csv")

