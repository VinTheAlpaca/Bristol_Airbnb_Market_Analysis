rm(list=ls())
library(tidyverse)
library(zoo)
library(ggplot2)
library(fpp)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(gridExtra)
library(grid)


testfile <- read_csv("listings.csv")
testfile$price<-as.numeric(gsub("[$,]","",as.character(testfile$price)))
class(testfile$price)

write.csv(testfile, file = "listings_converted.csv")
#Read as tibble
#airbnb <- read_csv("listings.csv")

monthes <-  seq(as.Date("2018/4/1"), as.Date("2020/3/1"), by = "month") %>%
            as.yearmon()


setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/Coursework/data/data")
airbnb.ts.listNO <- data.frame(
  month <- c(rep(NA, length(monthes))),
  listing_numbers <- c(rep(NA, length(monthes))),
  review_number <- c(rep(NA, length(monthes)))
)

names(airbnb.ts.listNO)[1] <- "month"
names(airbnb.ts.listNO)[2] <- "listing_numbers"
names(airbnb.ts.listNO)[3] <- "review_number"

for (i in 1:length(monthes)){
  file_name = paste0 ("listings_" , format(as.Date(monthes[i]),"%Y%m"), ".csv")
  airbnb.ts.this.file <- read_csv(file_name)
  airbnb.ts.listNO[i,1] <- monthes[i]
  airbnb.ts.listNO[i,2] <- nrow(airbnb.ts.this.file)
  airbnb.ts.listNO[i,3] <- sum(airbnb.ts.this.file$reviews_per_month, na.rm = TRUE)
}



ts.listNO <-ts(airbnb.ts.listNO$listing_numbers, start = as.yearmon(airbnb.ts.listNO[1,1]), frequency = 12)
ts.listNO.re <-ts(airbnb.ts.listNO$review_number, start = as.yearmon(airbnb.ts.listNO[1,1]), frequency = 12)

fit.arima <- auto.arima(ts.listNO)
fit.arima.re <- auto.arima(ts.listNO.re)
summary(fit.arima)
summary(fit.arima.re)

fit.ets <- ets(ts.listNO, damped = TRUE)
fit.ets.re <- ets(ts.listNO.re, damped = TRUE)

plot(decompose(ts.listNO.re))

autoplot(ts.listNO) + 
  #autolayer(fitted(fit.arima),series="Damped ETS") + 
  autolayer(forecast(fit.arima, h=12),series="Forecast") + 
  autolayer(ts.listNO, series="Real Data" ) +
  xlab("Month") +
  ylab("Number of Houses") + 
  scale_color_manual(values = c( "red")) +
  theme(legend.position = "bottom", legend.title = element_blank())


autoplot(ts.listNO.re) + 
  #autolayer(fitted(fit.arima),series="Damped ETS") + 
  autolayer(forecast(fit.arima.re, h=12),series="Forecast") + 
  autolayer(ts.listNO.re, series="Real Data" ) +
  xlab("Month") +
  ylab("Average Monthly Review Number") + 
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom", legend.title = element_blank())

autoplot(decompose(ts.listNO.re))









setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/Coursework/data/detailed_data")
monthes <-  seq(as.Date("2018/7/1"), as.Date("2020/3/1"), by = "month") %>%
  as.yearmon()
first_file = read_csv("listings_201807.csv")

airbnb.df.monthly <- data.frame(
  
  host_id <- first_file$host_id,
  property_type <- first_file$property_type,
  room_type <- first_file$room_type,
  bed_type <- first_file$bed_type,
  price <- first_file$price,
  minimum_nights <- first_file$minimum_nights,
  neighbourhood_cleansed <- first_file$neighbourhood_cleansed,
  number_of_reviews <- first_file$number_of_reviews,
  reviews_per_month <- first_file$reviews_per_month,
  cleaning_fee <- first_file$cleaning_fee,
  cancellation_policy <- first_file$cancellation_policy,
  month <- rep(monthes[1], nrow(first_file))
  
  
  
) 
?decompose
names(airbnb.df.monthly)[1] = "host_id"
names(airbnb.df.monthly)[2] = "property_type"
names(airbnb.df.monthly)[3] = "room_type"
names(airbnb.df.monthly)[4] = "bed_type"
names(airbnb.df.monthly)[5] = "price"
names(airbnb.df.monthly)[6] = "minimum_nights"
names(airbnb.df.monthly)[7] = "neighbourhood_cleansed"
names(airbnb.df.monthly)[8] = "number_of_reviews"
names(airbnb.df.monthly)[9] = "reviews_per_month"
names(airbnb.df.monthly)[10] = "cleaning_fee"
names(airbnb.df.monthly)[11] = "cancellation_policy"
names(airbnb.df.monthly)[12] = "month"


for (i in 2:length(monthes)){
  file_name = paste0 ("listings_" , format(as.Date(monthes[i]),"%Y%m"), ".csv")
  airbnb.ts.this.file <- read_csv(file_name)
  
  airbnb.df.monthly.new <- data.frame(
    host_id <- airbnb.ts.this.file$host_id,
    property_type <- airbnb.ts.this.file$property_type,
    room_type <- airbnb.ts.this.file$room_type,
    bed_type <- airbnb.ts.this.file$bed_type,
    price <- airbnb.ts.this.file$price,
    minimum_nights <- airbnb.ts.this.file$minimum_nights,
    neighbourhood_cleansed <- airbnb.ts.this.file$neighbourhood_cleansed,
    number_of_reviews <- airbnb.ts.this.file$number_of_reviews,
    reviews_per_month <- airbnb.ts.this.file$reviews_per_month,
    cleaning_fee <- airbnb.ts.this.file$cleaning_fee,
    cancellation_policy <- airbnb.ts.this.file$cancellation_policy,
    month <- rep(monthes[i], nrow(airbnb.ts.this.file))
    
  ) 
  
  names(airbnb.df.monthly.new)[1] = "host_id"
  names(airbnb.df.monthly.new)[2] = "property_type"
  names(airbnb.df.monthly.new)[3] = "room_type"
  names(airbnb.df.monthly.new)[4] = "bed_type"
  names(airbnb.df.monthly.new)[5] = "price"
  names(airbnb.df.monthly.new)[6] = "minimum_nights"
  names(airbnb.df.monthly.new)[7] = "neighbourhood_cleansed"
  names(airbnb.df.monthly.new)[8] = "number_of_reviews"
  names(airbnb.df.monthly.new)[9] = "reviews_per_month"
  names(airbnb.df.monthly.new)[10] = "cleaning_fee"
  names(airbnb.df.monthly.new)[11] = "cancellation_policy"
  names(airbnb.df.monthly.new)[12] = "month"
  
  airbnb.df.monthly <- rbind(airbnb.df.monthly, airbnb.df.monthly.new)
  
}

airbnb.df.monthly$price <- as.numeric(gsub("[$,]", "" ,as.character(airbnb.df.monthly$price)))
airbnb.df.monthly$cleaning_fee <- as.numeric(gsub("[$,]", "" ,as.character(airbnb.df.monthly$cleaning_fee)))

#airbnb.df.monthly$income <- airbnb.df.monthly$price*airbnb.df.monthly$minimum_nights*airbnb.df.monthly$reviews_per_month

############################# room type ###########################
room_type <- data.frame(
  airbnb.df.monthly$month,
  airbnb.df.monthly$room_type,
  airbnb.df.monthly$reviews_per_month
)

names(room_type)[1] <- "month"
names(room_type)[2] <- "room_type"
names(room_type)[3] <- "monthly_reviews"

room.ts <- room_type %>% 
  group_by(room_type, month) %>%
  summarise(Reviews = mean(monthly_reviews, na.rm = TRUE))

names(room.ts)[1] <- "Room Type"
names(room.ts)[2] <- "Month"
names(room.ts)[3] <- "Monthly Reviews"


ggplot(room.ts, aes(Month, `Monthly Reviews`, group = `Room Type`, color = `Room Type`)) +
  geom_line() +
  theme(legend.position="bottom")

############################# room type ###########################


############################# bed type ###########################
bed_type <- data.frame(
  airbnb.df.monthly$month,
  airbnb.df.monthly$bed_type,
  airbnb.df.monthly$reviews_per_month
)

names(bed_type)[1] <- "month"
names(bed_type)[2] <- "bed_type"
names(bed_type)[3] <- "monthly_reviews"

bed.ts <- bed_type %>% 
  group_by(bed_type, month) %>%
  summarise(Reviews = mean(monthly_reviews, na.rm = TRUE))
?summarise
names(bed.ts)[1] <- "Bed Type"
names(bed.ts)[2] <- "Month"
names(bed.ts)[3] <- "Monthly Reviews"


ggplot(bed.ts, aes(Month, `Monthly Reviews`, group = `Bed Type`, color = `Bed Type`)) +
  geom_line() +
  theme(legend.position="bottom")

############################# bed type ###########################


prop <- data.frame(
  airbnb.df.monthly$month,
  airbnb.df.monthly$property_type,
  airbnb.df.monthly$income
)

names(prop)[1] <- "month"
names(prop)[2] <- "property_type"
names(prop)[3] <- "income"

prop.ts.filter <- filter(prop, prop$property_type %in% c("Apartment", "House", "Townhouse", "Serviced apartment", "Condominium", "Loft"))
   
prop.ts <- prop %>% 
  group_by(property_type, month) %>%
  summarise(income = mean(income, na.rm = TRUE))

names(prop.ts)[1] <- "Property Type"
names(prop.ts)[2] <- "Month"
names(prop.ts)[3] <- "Income"
prop.ts$Month <- as.Date(prop.ts$Month)

ggplot(prop.ts, aes(Month, Income, group = `Property Type`, color = `Property Type`)) +
  geom_line() +
  theme(legend.position="bottom")

















distinct(cor_file[87])
names(cor_file)


setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/Coursework/data/detailed_data")

cor_file = read_csv("listings_202003.csv")

cor_data <- data.frame(
  
  monthly_reviews <- cor_file$reviews_per_month,
  cancellation_policy <- cor_file$cancellation_policy,
  cleaning_fee <- cor_file$cleaning_fee,
  host_response_time <- cor_file$host_response_time,
  host_response_rate <- cor_file$host_response_rate,
  stringsAsFactors = FALSE
)

names(cor_data)[1] = "Monthly Reviews"
names(cor_data)[2] = "Cancellation policy"
names(cor_data)[3] = "Cleaning fee"
names(cor_data)[4] = "Host response time"
names(cor_data)[5] = "Host response rate"


cor_data[cor_data$`Cancellation policy` == 'strict_14_with_grace_period', 2] <- 1
cor_data[cor_data$`Cancellation policy` == 'super_strict_30', 2] <- 2
cor_data[cor_data$`Cancellation policy` == 'super_strict_60', 2] <- 3
cor_data[cor_data$`Cancellation policy` == 'moderate', 2] <- 4
cor_data[cor_data$`Cancellation policy` == 'flexible', 2] <- 5
cor_data$`Cancellation policy` <- as.numeric(cor_data$`Cancellation policy` )


cor_data[is.na(cor_data$`Cleaning fee`), 3] <- "$0.00"
cor_data[,3] <- as.numeric(gsub("[$,]", "" ,as.character(cor_data[,3])))


cor_data[cor_data$`Host response time` == 'N/A', 4] <- 1
cor_data[cor_data$`Host response time` == 'a few days or more', 4] <- 2
cor_data[cor_data$`Host response time` == 'within a day', 4] <- 3
cor_data[cor_data$`Host response time` == 'within a few hours', 4] <- 4
cor_data[cor_data$`Host response time` == 'within an hour', 4] <- 5
cor_data$`Host response time` <- as.numeric(cor_data$`Host response time`)



cor_data[cor_data$`Host response rate` == 'N/A', 5] <- "0.00%"
cor_data[,5] <- as.numeric(gsub("[%]", "" ,as.character(cor_data[,5])))


cor_data[is.na(cor_data$`Monthly Reviews`), 1] <- 0

#cor.data = cor(cor_data[,1], cor_data[,2], method = c("spearman"))

cor.data = cor(cor_data, method = c("spearman"))
cor.data = round(cor.data, 3)
write.csv(cor.data, file = "cor.csv")
write.csv(cor_data, file = "cor_data.csv")
grid.table(cor.data)
class(cor_data$`Host response rate`)
distinct(cor_data[1])

plot(cor_data$`Monthly Reviews`, cor_data$`Host response rate`)

cor(1:10, 10:1)






airbnb.list.monthly <- array(NA, c(3000, 11, 21))
setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/Coursework/data/detailed_data")
for (i in 1:length(monthes)){
  file_name = paste0 ("listings_" , format(as.Date(monthes[i]),"%Y%m"), ".csv")
  airbnb.ts.this.file <- read_csv(file_name)
  
  airbnb.list.monthly[1:length(airbnb.ts.this.file$host_id), 1, i] <- airbnb.ts.this.file$host_id
  airbnb.list.monthly[1:length(airbnb.ts.this.file$property_type), 2, i] <- airbnb.ts.this.file$property_type
  airbnb.list.monthly[1:length(airbnb.ts.this.file$room_type), 3, i] <- airbnb.ts.this.file$room_type
  airbnb.list.monthly[1:length(airbnb.ts.this.file$bed_type), 4, i] <- airbnb.ts.this.file$bed_type
  airbnb.list.monthly[1:length(airbnb.ts.this.file$price), 5, i] <- airbnb.ts.this.file$price
  airbnb.list.monthly[1:length(airbnb.ts.this.file$minimum_nights), 6, i] <- airbnb.ts.this.file$minimum_nights
  airbnb.list.monthly[1:length(airbnb.ts.this.file$neighbourhood_cleansed), 7, i] <- airbnb.ts.this.file$neighbourhood_cleansed
  airbnb.list.monthly[1:length(airbnb.ts.this.file$number_of_reviews), 8, i] <- airbnb.ts.this.file$number_of_reviews
  airbnb.list.monthly[1:length(airbnb.ts.this.file$reviews_per_month), 9, i] <- airbnb.ts.this.file$reviews_per_month
  airbnb.list.monthly[1:length(airbnb.ts.this.file$cleaning_fee), 10, i] <- airbnb.ts.this.file$cleaning_fee
  airbnb.list.monthly[1:length(airbnb.ts.this.file$cancellation_policy), 11, i] <- airbnb.ts.this.file$cancellation_policy
  
  
  
}


airbnb.list.monthly[is.na(airbnb.list.monthly)] <- 0

airbnb.list.monthly[,5,] <- as.numeric(gsub("[$,]", "" ,as.character(airbnb.list.monthly[,5,])))
airbnb.list.monthly[,10,] <- as.numeric(gsub("[$,]", "" ,as.character(airbnb.list.monthly[,10,])))




airbnb.202003.types <- data.frame(
  
  income <- as.numeric(airbnb.list.monthly[, 5, 21])*as.numeric(airbnb.list.monthly[, 6, 21]),
  property <- airbnb.list.monthly[,2,21],
  room <- airbnb.list.monthly[,3,21],
  bed <- airbnb.list.monthly[,4,21]
)


names(airbnb.202003.types)[1] = "income"
names(airbnb.202003.types)[2] = "property"
names(airbnb.202003.types)[3] = "room"
names(airbnb.202003.types)[4] = "bed"


test <-  filter(airbnb.202003.types, airbnb.202003.types$property == "Apartment")
sum(test$income)

barplot(airbnb.202003.types$property, airbnb.202003.types$income)


?barplot







airbnb.prop.historical <- data.frame(
  month <- NA,
  income <- NA,
  property <- NA
)

names(airbnb.prop.historical)[1] = "month"
names(airbnb.prop.historical)[2] = "income"
names(airbnb.prop.historical)[3] = "property"

month = as.yearmon(as.Date("2018/7/1"))
  
for (i in 1:1){
  i = 1
  month = month + i - 1 
  airbnb.prop.historical[3000*(i-1) + 1 : 3000*(i-1) + 1 + 3000, 1] = month
  airbnb.prop.historical[3000*(i-1) + 1 : 3000*(i-1) + 1 + 3000, 2] = as.numeric(airbnb.list.monthly[, 5, i])*as.numeric(airbnb.list.monthly[, 6, i])
  airbnb.prop.historical[3000*(i-1) + 1 : 3000*(i-1) + 1 + 3000, 3] = airbnb.list.monthly[,2,i]

  }



airbnb.list.monthly[,5,1] <- as.numeric(airbnb.list.monthly[,5,1])

sum(airbnb.list.monthly[,5,1])
class(airbnb.list.monthly[,5,1])










setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/Coursework/data/detailed_data")
airbnb.ts.this.file = read.csv("listings_202003.csv")
airbnb.ts.this.file$price<-as.numeric(gsub("[$,]","",as.character(airbnb.ts.this.file$price)))

airbnb.ts.this.file$heat = airbnb.ts.this.file$reviews_per_month
#airbnb.ts.this.file[is.na(airbnb.ts.this.file)] <- 0


leaflet(data = airbnb.ts.this.file) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addTiles() %>%
  addHeatmap(lng=~longitude, 
             lat=~latitude, 
             intensity = ~heat, 
             blur = 7, 
             minOpacity = 0.05, max = 1, 
             radius = 4) %>%
  addLabelOnlyMarkers(lng = -2.59173, lat = 51.43994, label = "test")
  



?addHeatmap
log(100000, 1.1)
  
  max(airbnb.ts.this.file$monthly_income)
  min(airbnb.ts.this.file$monthly_income)
addCircles(lat = ~latitude,
             lng = ~longitude,
             weight = 1,
             radius = 50,
             color = "green",
             fillOpacity  = 1
  )

file <- read_csv("listings_201807.csv")
airbnb.ts.listings <- data.frame(
  '201807_long' <- file$longitude,
  '201807_lat' <- file$latitude
)

test[1] <- list(c(file$longitude, file$latitude))
test <- list(c(3, 2))
test[3] <- NA

file1 <- read_csv("listings_201808.csv")
airbnb.ts.listings$`201808_long` <- file1$longitude
airbnb.ts.listings$`201807_lat` <- file1$latitude

?cbind
cbind(airbnb.ts.listings, file1$longitude)
dplyr::bind_cols(airbnb.ts.listings, file1$longitude)



?leaflet



















setwd("C:/Users/44757/Desktop/Bath/MN50644_Analytics_in_practice/")
file_js = FROM_GeoJson(url_file_string = "neighbourhoods.geojson")

file_js$features[[8]]$geometry$coordinates

ggplot(airbnb.ts.listNO, aes(month, listing_numbers)) +
  geom_point() +
  geom_line()

long = file_js$features[[8]]$geometry$coordinates[[1]][,1]
lat = file_js$features[[8]]$geometry$coordinates[[1]][,2]



ggplot() + 
  geom_point(x = long, y = lat)

class(file_js$features[[8]]$geometry$coordinates)



plot(airbnb.ts.listNO, type="b")
lines(airbnb.ts.listNO)


#Example regex on descriptions
spain_amenities_data <- unlist(strsplit(airbnb$amenities, ","))
spain_amenities_data_clean <- gsub("[[:punct:]]","", spain_amenities_data)
spain_freq <- as.data.frame(table(spain_amenities_data_clean))
spain_freq <- spain_freq %>% arrange(desc(Freq))


#names(airbnb)
