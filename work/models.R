library(dplyr)

# Read in data
full <- read.csv('data/full_data.csv')

# Remove commas from attendance
full$Attendance <- gsub(",", "", full$Attendance)

# Convert to numerics
full$Attendance <- as.numeric(full$Attendance)

# Aggregate by year
full_league <- full %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance))

# Only take group of teams

# Only take Red Sox
red_sox <- full %>% 
  filter(Tm == 'Boston Red Sox') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))
