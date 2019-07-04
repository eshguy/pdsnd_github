library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# read in data
chi <- read.csv("chicago.csv")
ny <- read.csv("new_york_city.csv")
was <- read.csv("washington.csv")

# tweak the washington data (add columns gender & dob)
was$Gender <- ""
was$Birth.Year <- NA

# add a city column to each dataset
chi$City <- "Chicago"
was$City <- "Washington"
ny$City <- "NewYork"

#combine datasets
allCities <- rbind(chi, ny, was)

# check data structure
str(allCities)
allCities$City <- as.factor(allCities$City)

# convert data times to date times rather than factors.
# start by making them characters
allCities$City <- as.character(allCities$City)

allCities$Start.Time <-  as.POSIXct(x = allCities$Start.Time, 
                                     tz = "",
                                     format = "%Y-%m-%d %H:%M:%S")


allCities$End.Time <-  as.POSIXct(x = allCities$End.Time, 
                                    tz = "",
                                    format = "%Y-%m-%d %H:%M:%S")

str(allCities)

# fix the Unknown Gender Transactions
allCities$Gender <- as.character(allCities$Gender)
allCities$Gender[allCities$Gender == ""] <- "Unknown"
allCities$Gender <- as.factor(allCities$Gender)
summary(allCities$Gender)
str(allCities)

cityStats <- allCities %>% group_by(City) %>% summarise(ave = mean(Trip.Duration, na.rm = T),
                                                        std = sd(Trip.Duration, na.rm = T))

genderStats <- allCities %>% group_by(Gender) %>% summarise(ave = mean(Trip.Duration, na.rm = T),
                                                          std = sd(Trip.Duration, na.rm = T))

monthStats <- allCities %>% group_by(TripMonth) %>% summarise(ave = mean(Trip.Duration, na.rm = T),
                                                            std = sd(Trip.Duration, na.rm = T))
ggplot(data = allCities) +
  geom_histogram(aes(x = Trip.Duration), binwidth = 250) +
  coord_cartesian(xlim = c(0,10000)) +
  facet_wrap(facets = ~City) +
  geom_vline(data = cityStats, aes(xintercept = ave), color = "red") +
  geom_text(data = cityStats ,aes(x = ave + 100,
                                  label = round(ave, 2),
                                  y = 15000),
            hjust = 0, vjust = 0,
            color = "red") +
  labs(title = "Trip Duration Distribution by City",
       subtitle = "Red Line and Text are the Means",
       x = "Trip Duration",
       tag = "A")

ggplot(data = allCities) +
  geom_histogram(aes(x = Trip.Duration), binwidth = 250) +
  coord_cartesian(xlim = c(0,10000)) +
  facet_wrap(facets = ~Gender) +
  geom_vline(data = genderStats, aes(xintercept = ave), color = "red") +
  geom_text(data = genderStats ,aes(x = ave + 100,
                                    label = round(ave, 2),
                                    y = 15000),
            hjust = 0, vjust = 0,
            color = "red") +
  labs(title = "Trip Duration Distribution by Gender",
       subtitle = "Red Line and Text are the Means",
       x = "Trip Duration",
       tag = "B")

ggplot(data = allCities) +
  geom_histogram(aes(x = Trip.Duration), binwidth = 250) +
  coord_cartesian(xlim = c(0,10000)) +
  facet_wrap(facets = ~TripMonth) +
  geom_vline(data = monthStats, aes(xintercept = ave), color = "red") +
  geom_text(data = monthStats ,aes(x = ave + 100,
                                   label = round(ave, 2),
                                   y = 15000),
            hjust = 0, vjust = 0,
            color = "red") +
  labs(title = "Trip Duration Distribution by Month",
       subtitle = "Red Line and Text are the Means",
       x = "Trip Duration",
       tag = "C")

aov(data = allCities[!is.na(allCities$Trip.Duration) , ], formula = Trip.Duration ~ City)
kruskal.test(x = allCities$Trip.Duration[!is.na(allCities$Trip.Duration)], 
             g = allCities$City[!is.na(allCities$Trip.Duration)])
summary(allCities$Trip.Duration)
