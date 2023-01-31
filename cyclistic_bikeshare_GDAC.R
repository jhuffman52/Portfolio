### Capstone project for Google Data Analytics Certificate
### I attempted to do most of this on my own, simply following the prompts given in the guide
### There were some lines of code that I had to copy and paste from the guide. 
### I have made a note where I used code from the guide. 

#### load any packages that might be needed
install.packages("data.table")
library(data.table) ### used to write to csv quickly. 
library(tidyverse)  ### Helps wrangle data
library(dplyr)      ### helps manipulate data
library(ggplot2)    ### used to visualize data
library(lubridate)

### Import the data from the working directory. 
jan_trips <- read_csv("202201-divvy-tripdata.csv")
feb_trips <- read_csv("202202-divvy-tripdata.csv")
mar_trips <- read_csv("202203-divvy-tripdata.csv")
apr_trips <- read_csv("202204-divvy-tripdata.csv")
may_trips <- read_csv("202205-divvy-tripdata.csv")
jun_trips <- read_csv("202206-divvy-tripdata.csv")
jul_trips <- read_csv("202207-divvy-tripdata.csv")
aug_trips <- read_csv("202208-divvy-tripdata.csv")
sep_trips <- read_csv("202209-divvy-publictripdata.csv")
oct_trips <- read_csv("202210-divvy-tripdata.csv")
nov_trips <- read_csv("202211-divvy-tripdata.csv")
dec_trips <- read_csv("202212-divvy-tripdata.csv")

### Convert the datetime format to all be the same format so calculations can be done later. 
nov_trips$started_at <- as.POSIXct(nov_trips$started_at, format = '%m/%d/%Y %H:%M')
aug_trips$started_at <- as.POSIXct(aug_trips$started_at, format = '%m/%d/%Y %H:%M')
aug_trips$ended_at <- as.POSIXct(aug_trips$ended_at, format = '%m/%d/%Y %H:%M')
jul_trips$started_at <- as.POSIXct(jul_trips$started_at, format = '%m/%d/%Y %H:%M')
nov_trips$ended_at <- as.POSIXct(nov_trips$ended_at, format = '%m/%d/%Y %H:%M')

str(jan_trips)
str(feb_trips)
str(mar_trips)
str(apr_trips)
str(may_trips)
str(jun_trips)
str(jul_trips)
str(aug_trips)
str(sep_trips)
str(oct_trips)
str(nov_trips)
str(dec_trips)
### combine all dataframes into one year own code

all_trips <- bind_rows(jan_trips, feb_trips, mar_trips, apr_trips, may_trips, jun_trips, jul_trips, aug_trips, sep_trips, oct_trips, nov_trips, dec_trips )

#### Remove columns that are not needed.

all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lat, end_lng))

### inspect data to make sure the correct columns were removed.
colnames(all_trips)
### check to see how many riders fall under casual and member. 
### There should only be two user types and this confirms that. Code is from the guide. 
table(all_trips_v2$member_casual)

### Add columns for date, weekday, month, year and ride length. Code is from the guide
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
### Check structure of ride_length
str(all_trips$ride_length)

### convert ride_length to numeric to make calculcations easier in the future
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

### create new df that has ride_length values of over 0. The guide also mentioned removing 
### station names with "HQ QR", but when i did this, it removed 0 rows so I have omitted that step

all_trips <- all_trips[!(all_trips$ride_length <1),]

#### save file to wd.
fwrite(all_trips_v2, "all_trips_v2.csv")

all_trips_v2 <- read_csv("all_trips_v2.csv")
### Start analysis of data

summary(all_trips_v2$ride_length)

### compare members to casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


### put the days of the week in order 

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### Convert ride length from seconds to minutes
all_trips_v2$ride_length <- all_trips_v2$ride_length/60

### find the average ride length for each member type
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


### create vizzies

###  The first  explore number of rides per day and average duration of each ride per day, sorted by member_casual
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

### Now I will create a plot that shows the number of rides by user type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE))%>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + scale_y_continuous(labels = scales::label_number_si())

### Now I will make a visualization for the average duration of each bike ride
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")

### Next visual will show average duration for each user type by month. 
### First the months column needs to be put in order



all_trips_v2$month <- ordered(all_trips_v2$month, levels = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
### Now the order needs to be double checked.
### This is also a quick summary of the length of rides by user type per month. 
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$month, FUN = mean)


all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + 
  ggtitle(label = "Average Duration of Bike Rides per Month") + xlab("Month") + ylab("Average Duration of Ride") + labs(fill = "Usertype")

### next visual will show number of rides per month by user type
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  ggtitle(label = "Number of Rides Per Month by Usertype") + xlab("Month") + ylab("Number Of Rides") + labs(fill = "Usertype") + 
  scale_y_continuous(labels = scales::label_number_si())

### Look at rideable_type by usertype
all_trips_v2 %>%
  group_by(member_casual, rideable_type, month)%>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type)%>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = rideable_type)) + geom_col(position = "dodge") + facet_wrap(~month)


### Line graph of usage by month
all_trips_v2 %>%
  group_by(member_casual, rideable_type, month)%>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type)%>%
ggplot(aes(month, number_of_rides, group = member_casual)) + geom_point() + geom_smooth(method = 'lm', se=FALSE)


all_trips_v2 %>%
  group_by(member_casual, start_station_name)%>%
  summarise(number_of_rides = n())%>%
  arrange(member_casual, start_station_name)%>%
  ggplot(aes(start_station_name, number_of_rides, color = member_casual)) + geom_col(position = "dodge")
### end of code




