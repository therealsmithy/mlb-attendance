library(dplyr)
library(forecast)
library(fpp3)
library(zoo)

# Read in data
full <- read.csv('data/full_data.csv')
cpi <- read.csv('data/cpi.csv')

# Remove commas from attendance
full$Attendance <- gsub(",", "", full$Attendance)

# Convert to numerics
full$Attendance <- as.numeric(full$Attendance)

# Aggregate by year
full_league <- full %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance))

# Clean cpi data
colnames(cpi)[1] <- 'year'
cpi <- cpi %>% 
  select(year, Avg)

# Join with full data
full_league <- full_league %>% 
  left_join(cpi, by = 'year')

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

# FULL LEAGUE MODEL
# Create time series object
full_ts <- zoo(full_league$attendance, order.by = full_league$year)
nVal <- 5
nTrain <- length(full_ts) - nVal
full_train <- window(full_ts, start = 1903, end = c(1903 + nTrain))
full_val <- window(full_ts, start = c(1903 + nTrain + 1), end = c(1903 + nTrain + nVal + 1))

# Test for Random Walk
rw_full <- Arima(full_train, order = c(1, 0, 0))
summary(rw_full)
t_stat <- (0.9766 - 1) / 0.0209
2 * pnorm(t_stat)
# Random Walk...

# Model with lagged predictors - SARIMAX
# Plot
plot(full_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Full League Attendance')
lines(full_train, col = 'blue')
lines(full_val, col = 'red')

# WHITE SOX MODEL
# Create time series object
w_sox_ts <- zoo(w_sox$attendance, order.by = w_sox$year)
w_sox_train <- window(w_sox_ts, start = 1903, end = c(1903 + nTrain))
w_sox_val <- window(w_sox_ts, start = c(1903 + nTrain + 1), end = c(1903 + nTrain + nVal))

# Test for Random Walk
rw_w_sox <- Arima(w_sox_train, order = c(1, 0, 0))
summary(rw_w_sox)
t_stat <- (0.8618 - 1) / 0.0524
2 * pnorm(t_stat)
# NOT a random walk

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')

# TIGERS MODEL
# Create time series object
tigers_ts <- zoo(tigers$attendance, order.by = tigers$year)
tigers_train <- window(tigers_ts, start = 1903, end = c(1903 + nTrain))
tigers_val <- window(tigers_ts, start = c(1903 + nTrain + 1), end = c(1903 + nTrain + nVal))

# Test for Random Walk
rw_tigers <- Arima(tigers_train, order = c(1, 0, 0))
summary(rw_tigers)
t_stat <- (0.8570 - 1) / 0.0528
2 * pnorm(t_stat)
# NOT a random walk

# Plot it out
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')

# PIRATES MODEL
# Create time series object
pirates_ts <- zoo(pirates$attendance, order.by = pirates$year)
pirates_train <- window(pirates_ts, start = 1903, end = c(1903 + nTrain))
pirates_val <- window(pirates_ts, start = c(1903 + nTrain + 1), end = c(1903 + nTrain + nVal))

# Test for Random Walk
rw_pirates <- Arima(pirates_train, order = c(1, 0, 0))
summary(rw_pirates)
t_stat <- (0.8389 - 1) / 0.0548
2 * pnorm(t_stat)
# NOT a random walk

# Plot it out
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')
