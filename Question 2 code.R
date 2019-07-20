if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
library(dplyr)

if (!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}
library(tidyr)

if (!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}
library(lubridate)

if (!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}
library(stringr)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}
library(ggplot2)


raw_crashes = read.csv("plane_crash_data.csv", stringsAsFactors = FALSE)
# remove the "flight number" column, as it does not provide any statistically significant data and convert "date" and "time" columns from character strings to date and times respectively
no_flightnum = raw_crashes %>%
  mutate(flight_number = NULL, aircraft_type = as.factor(aircraft_type), date = mdy(date), time = hm(time))

# split the "abaord" column into three: total number abaord, number of passengers aboard and number of crew members aboard
aboard_split = separate(no_flightnum, aboard, c("num_aboard", "breakdown"), sep = "[(]")

aboard_split = separate(aboard_split, breakdown, c("passengers_aboard", "crew_aboard"), sep = " ")
aboard_split = separate(aboard_split, passengers_aboard, c("delete", "passengers_aboard"), sep = ":")
aboard_split = aboard_split %>%
  mutate(delete = NULL)

aboard_split = separate(aboard_split, crew_aboard, c("delete", "crew_aboard"), sep = ":")
aboard_split = aboard_split %>%
  mutate(delete = NULL)
aboard_split = separate(aboard_split, crew_aboard, c("crew_aboard", "delete"), sep = "[)]")
aboard_split = aboard_split %>%
  mutate(delete = NULL)

# same code to separate the fatalities column
fatality_split = separate(aboard_split, fatalities, c("num_fatalities", "breakdown"), sep = "[(]")

fatality_split = separate(fatality_split, breakdown, c("passenger_fatality", "crew_fatality"), sep = " ")
fatality_split = separate(fatality_split, passenger_fatality, c("delete", "passenger_fatality"), sep = ":")
fatality_split = fatality_split %>%
  mutate(delete = NULL)
fatality_split = separate(fatality_split, crew_fatality, c("delete", "crew_fatality"), sep = ":")
fatality_split = fatality_split %>%
  mutate(delete = NULL)
fatality_split = separate(fatality_split, crew_fatality, c("crew_fatality", "delete"), sep = "[)]")
fatality_split = fatality_split %>%
  mutate(delete = NULL)

# remove extraneous white space from values
split_crashes = fatality_split %>%
  mutate(num_aboard = str_trim(num_aboard), passengers_aboard = str_trim(passengers_aboard), crew_aboard = str_trim(crew_aboard), num_fatalities = str_trim(num_fatalities), passenger_fatality = str_trim(passenger_fatality), crew_fatality = str_trim(crew_fatality))

#covert all "?" to NA
NoQ_crashes = split_crashes %>%
  mutate(crew_fatality = na_if(crew_fatality, "?"), time = na_if(time, "?"), route = na_if(route, "?"), registration = na_if(registration, "?"), cn_ln = na_if(cn_ln, "?"), num_aboard = na_if(num_aboard, "?"), passengers_aboard = na_if(passengers_aboard, "?"), crew_aboard = na_if(crew_aboard, "?"), num_fatalities = na_if(num_fatalities, "?"), passenger_fatality = na_if(passenger_fatality, "?"), ground = na_if(ground, "?"), summary = na_if(summary, "?"))

# covert all numbers from character strings to numeric values
NoQ_crashes = NoQ_crashes %>%
  mutate(crew_aboard = as.numeric(crew_aboard), crew_fatality = as.numeric(crew_fatality), num_fatalities = as.numeric(num_fatalities), passenger_fatality = as.numeric(passenger_fatality), passengers_aboard = as.numeric(passengers_aboard), num_aboard = as.numeric(num_aboard), ground = as.numeric(ground))

# checking that the number of fatalities never exceeds the number aboard
aboard_vs_fatality = (NoQ_crashes$num_fatalities > NoQ_crashes$num_aboard)

TRUE %in% aboard_vs_fatality

clean_crashes = NoQ_crashes

# subset of military planes from 1980 onwards to check for technological error
military_planes = clean_crashes %>%
  filter(grepl("Military", operator), year(date) >= 1980) %>%
  group_by(aircraft_type) %>%
  mutate(freq = n()) %>%
  filter(freq > 3, !grepl("shot down", summary), !grepl("hijacker", summary), !grepl("missile", summary))

# Histogram of number of unattacked military plane crashes by aircraft type
ggplot(military_planes, aes(x = aircraft_type, color = aircraft_type)) + geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Aircraft type") +
  ylab("Number of Crashes") +
  ggtitle("Aircraft Type vs. Number of Crashes") +
  scale_y_continuous(breaks = seq(0, 13, by = 1))

# Check if proprtion of crew members affects number of crashes
crew_function = clean_crashes %>%
  filter(!grepl("Military", operator), passengers_aboard != 0) %>%
  filter(num_aboard == (crew_aboard + passengers_aboard), num_fatalities == (crew_fatality + passenger_fatality)) %>%
  mutate(crew_proportion = crew_aboard/num_aboard,
         prop_passenger_fatal = passenger_fatality/passengers_aboard) %>%
  group_by(crew_proportion)

# Create a scatterplot to see if there is a correlation
ggplot(crew_function, aes(x = crew_proportion, y = prop_passenger_fatal)) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Proportion of people aboard that are crew members") +
  ylab("Proportion of passengers on board that were killed") +
  ggtitle("Crew Proportion vs. Proportion of Passenger Fatalities")

# Create a subset of the data frame containing only military planes, with a new column for type of crash (intended or not)
all_mil_planes = clean_crashes %>%
  filter(grepl("Military", operator)) %>%
  group_by(year(date)) %>%
  mutate(crash_type = (!grepl("shot down", summary) & !grepl("hijacker", summary) & !grepl("missile", summary)))

# Line graph measuring number of military plane crashes by year, comparing attacked planes to malfunctioning ones
ggplot(all_mil_planes, aes(x = year(date), color = crash_type)) + geom_line(stat = "bin", bins = 50) +
  xlab("Year") +
  ylab("Number of Crashes") +
  ggtitle(" Military Plane Crashes by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Cause of crash", labels = c("Attacked", "Other"))

#same code for non-military planes
non_mil_planes = clean_crashes %>%
  filter(!grepl("Military", operator)) %>%
  group_by(year(date)) %>%
  mutate(crash_type = (!grepl("shot down", summary) & !grepl("hijacker", summary) & !grepl("missile", summary)))

ggplot(non_mil_planes, aes(x = year(date), color = crash_type)) + geom_line(stat = "bin", bins = 50) +
  xlab("Year") +
  ylab("Number of Crashes") +
  ggtitle(" Non-Military Plane Crashes by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Cause of crash", labels = c("Attacked", "Other"))
