library(dplyr)
library(forecast)

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

# Only take Phillies
phillies <- full %>% 
  filter(Tm == 'Philadelphia Phillies' | Tm == 'Philadelphia Phils') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Only take White Sox
w_sox <- full %>% 
  filter(Tm == 'Chicago White Sox') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Only take Tigers
tigers <- full %>% 
  filter(Tm == 'Detroit Tigers') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Only take Pirates
pirates <- full %>% 
  filter(Tm == 'Pittsburgh Pirates') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Only take Cardinals
cards <- full %>% 
  filter(Tm == 'St. Louis Cardinals') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Only take Cubs
cubs <- full %>% 
  filter(Tm == 'Chicago Cubs') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# FULL LEAGUE MODEL
# Create time series object
full_ts <- ts(full_league$attendance, start = 1903, end = 2024)
nTrain <- 92
nVal <- length(full_ts) - nTrain
full_train <- window(full_ts, start = 1903, end = c(1903, nTrain))
full_val <- window(full_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_full <- Arima(full_train, order = c(1, 0, 0))
summary(rw_full)
t_stat <- (0.9766 - 1) / 0.0209
2 * pnorm(t_stat)

# WHITE SOX MODEL
# Create time series object
w_sox_ts <- ts(w_sox$attendance, start = 1903, end = 2024)
w_sox_train <- window(w_sox_ts, start = 1903, end = c(1903, nTrain))
w_sox_val <- window(w_sox_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_w_sox <- Arima(w_sox_train, order = c(1, 0, 0))
summary(rw_w_sox)
t_stat <- (0.8618 - 1) / 0.0524
2 * pnorm(t_stat)

# TIGERS MODEL
# Create time series object
tigers_ts <- ts(tigers$attendance, start = 1903, end = 2024)
tigers_train <- window(tigers_ts, start = 1903, end = c(1903, nTrain))
tigers_val <- window(tigers_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_tigers <- Arima(tigers_train, order = c(1, 0, 0))
summary(rw_tigers)
t_stat <- (0.8570 - 1) / 0.0528
2 * pnorm(t_stat)

# PIRATES MODEL
# Create time series object
pirates_ts <- ts(pirates$attendance, start = 1903, end = 2024)
pirates_train <- window(pirates_ts, start = 1903, end = c(1903, nTrain))
pirates_val <- window(pirates_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_pirates <- Arima(pirates_train, order = c(1, 0, 0))
summary(rw_pirates)
t_stat <- (0.8389 - 1) / 0.0548
2 * pnorm(t_stat)
