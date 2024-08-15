# Case Study 1

#setting the working directory
setwd("~/Desktop/GA Capstone Project/Project 2_ Aug2024/RAW")

#Load Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)

#Import CSV
Jul_2024 <- read.csv("202407-divvy-tripdata.csv")
Jun_2024 <- read.csv("202406-divvy-tripdata.csv")
May_2024 <- read.csv("202405-divvy-tripdata.csv")
Apr_2024 <- read.csv("202404-divvy-tripdata.csv")
Mar_2024 <- read.csv("202403-divvy-tripdata.csv")
Feb_2024 <- read.csv("202402-divvy-tripdata.csv")
Jan_2024 <- read.csv("202401-divvy-tripdata.csv")
Dec_2023 <- read.csv("202312-divvy-tripdata.csv")
Nov_2023 <- read.csv("202311-divvy-tripdata.csv")
Oct_2023 <- read.csv("202310-divvy-tripdata.csv")
Sep_2023 <- read.csv("202309-divvy-tripdata.csv")
Aug_2023 <- read.csv("202308-divvy-tripdata.csv")

# Compare column names each of the files
colnames(Jul_2024)
colnames(Jun_2024)
colnames(May_2024)
colnames(Apr_2024)
colnames(Mar_2024)
colnames(Feb_2024)
colnames(Jan_2024)
colnames(Dec_2023)
colnames(Nov_2023)
colnames(Oct_2023)
colnames(Sep_2023)
colnames(Aug_2023)

str(Jul_2024)

# Merge
all_trips <- bind_rows(Jul_2024, Jun_2024, May_2024, Apr_2024, Mar_2024, Feb_2024, Jan_2024, 
                       Dec_2023, Nov_2023, Oct_2023, Sep_2023, Aug_2023)

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#quality check with results in excel
check <- mean(all_trips$ride_length[all_trips$month == "07"])
print(check/60)

# New version without un usable data (ride_length<10sec, faulty bike put back, inspection, problem with application, etc)
all_trips_v2 <- all_trips[!(all_trips$ride_length<10),]

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# The average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# The days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Visualization of the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Analyze ridership data by type of membership and month
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title="Total Number of Ride by Month", x = "Month", y = "Number of Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Created column to organize by seasons
all_trips_v2 <- all_trips_v2 %>%
  mutate(seasons = 
           lapply(month, function(x) {
             switch(x,
                    "06" = "Summer",
                    "07" = "Summer",
                    "08" = "Summer",
                    "09" = "Fall",
                    "10" = "Fall",
                    "11" = "Fall",
                    "12" = "Winter",
                    "01" = "Winter",
                    "02" = "Winter",
                    "Spring")
           }))

#Evaluate what season casual members spend time riding, and how it is different from members
member_casual_by_season <- all_trips_v2 %>%
  group_by(member_casual, seasons) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  group_by(member_casual) %>%
  mutate(percentage = number_of_rides / sum(number_of_rides) * 100)



