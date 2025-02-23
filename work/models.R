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

# Impute 2020 as average of 2019 and 2021
full_league <- rbind(full_league, c(1957, NA, NA, NA))
full_league <- rbind(full_league, c(2020, NA, NA, NA))
full_league <- full_league[order(full_league$year),]
full_league$attendance[full_league$year == 1957] <- mean(full_league$attendance[full_league$year %in% c(1956, 1958)])
full_league$attendance[full_league$year == 2020] <- mean(full_league$attendance[full_league$year %in% c(2019, 2021)])

# Clean cpi data
colnames(cpi)[1] <- 'year'
cpi <- cpi %>% 
  select(year, Avg) %>% 
  mutate(cpi = Avg)

# Join with full data
full_league <- full_league %>% 
  left_join(cpi, by = 'year')

# Take years without NAs
full_league <- full_league %>% 
  filter(!is.na(cpi))

# Create lagged CPI
full_league$cpi_lag <- lag(full_league$Avg, 1)

# Only take White Sox
w_sox <- full %>% 
  filter(Tm == 'Chicago White Sox') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Add 1957 and 2020
w_sox <- rbind(w_sox, c(1957, NA, NA))
w_sox <- rbind(w_sox, c(2020, NA, NA))
w_sox <- w_sox[order(w_sox$year),]
w_sox$attendance[w_sox$year == 1957] <- mean(w_sox$attendance[w_sox$year %in% c(1956, 1958)])
w_sox$attendance[w_sox$year == 2020] <- mean(w_sox$attendance[w_sox$year %in% c(2019, 2021)])
w_sox$wins[w_sox$year == 1957] <- mean(w_sox$wins[w_sox$year %in% c(1956, 1958)])
w_sox$wins[w_sox$year == 2020] <- mean(w_sox$wins[w_sox$year %in% c(2019, 2021)])

# Only take Tigers
tigers <- full %>% 
  filter(Tm == 'Detroit Tigers') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Add 1957 and 2020
tigers <- rbind(tigers, c(1957, NA, NA))
tigers <- rbind(tigers, c(2020, NA, NA))
tigers <- tigers[order(tigers$year),]
tigers$attendance[tigers$year == 1957] <- mean(tigers$attendance[tigers$year %in% c(1956, 1958)])
tigers$attendance[tigers$year == 2020] <- mean(tigers$attendance[tigers$year %in% c(2019, 2021)])
tigers$wins[tigers$year == 1957] <- mean(tigers$wins[tigers$year %in% c(1956, 1958)])
tigers$wins[tigers$year == 2020] <- mean(tigers$wins[tigers$year %in% c(2019, 2021)])

# Only take Pirates
pirates <- full %>% 
  filter(Tm == 'Pittsburgh Pirates') %>% 
  group_by(year) %>% 
  summarise(attendance = sum(Attendance),
            wins = sum(W))

# Add 1957 and 2020
pirates <- rbind(pirates, c(1957, NA, NA))
pirates <- rbind(pirates, c(2020, NA, NA))
pirates <- pirates[order(pirates$year),]
pirates$attendance[pirates$year == 1957] <- mean(pirates$attendance[pirates$year %in% c(1956, 1958)])
pirates$attendance[pirates$year == 2020] <- mean(pirates$attendance[pirates$year %in% c(2019, 2021)])
pirates$wins[pirates$year == 1957] <- mean(pirates$wins[pirates$year %in% c(1956, 1958)])
pirates$wins[pirates$year == 2020] <- mean(pirates$wins[pirates$year %in% c(2019, 2021)])

# FULL LEAGUE MODEL
# Create time series object
full_ts <- ts(full_league$attendance, start = 1913, end = 2024)
nVal <- 5
nTrain <- length(full_ts) - nVal
full_train <- window(full_ts, start = 1913, end = c(1913, nTrain))
full_val <- window(full_ts, start = c(1913, nTrain + 1), end = c(1913, nTrain + nVal))

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

# Naive model
naive_full <- naive(full_train, h = nVal)
naive_full_forecast <- forecast(naive_full, h = nVal)

# Sarimax model
sarimax_full <- auto.arima(full_train, xreg = full_league$Avg[1:length(full_train)])
sarimax_full_forecast <- forecast(sarimax_full, xreg = full_league$Avg[(length(full_train) + 1):(length(full_train) + nVal)])

# Sarmiax with lagged predictors
sarimax_full_lag <- auto.arima(full_train, xreg = full_league$cpi_lag[1:length(full_train)])
sarimax_full_lag_forecast <- forecast(sarimax_full_lag, xreg = full_league$cpi_lag[(length(full_train) + 1):(length(full_train) + nVal)])

# Check accuracy of three models
accuracy(naive_full_forecast, full_val)
accuracy(sarimax_full_forecast, full_val)
accuracy(sarimax_full_lag_forecast, full_val)
# Naive performs best, which makes sense for a random walk

# Plot three forecasts
plot(full_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Full League Attendance')
lines(full_train, col = 'blue')
lines(full_val, col = 'red')
lines(naive_full_forecast$mean, col = 'green')
lines(sarimax_full_forecast$mean, col = 'purple')
lines(sarimax_full_lag_forecast$mean, col = 'orange')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Sarimax', 'Sarimax Lagged'), col = c('blue', 'red', 'green', 'purple', 'orange'), lty = 1)

# Check residuals
checkresiduals(naive_full)

# Use naive model for forecast
naive_full_future <- naive(full_ts, h = 5)
plot(naive_full_future)

# WHITE SOX MODEL
# Create time series object
w_sox_ts <-ts(w_sox$attendance, start = 1903, end = 2024)
nTrain <- length(w_sox_ts) - nVal
nVal <- 5
w_sox_train <- window(w_sox_ts, start = 1903, end = c(1903, nTrain))
w_sox_val <- window(w_sox_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_w_sox <- Arima(w_sox_train, order = c(1, 0, 0))
summary(rw_w_sox)
t_stat <- (0.8618 - 1) / 0.0524
2 * pnorm(t_stat)
Acf(w_sox_ts)
# NOT a random walk

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')

# TREND AND NAIVE MODELS
# Naive model
naive_w_sox <- naive(w_sox_train, h = nVal)
naive_w_sox_forecast <- forecast(naive_w_sox, h = nVal)

# Polynomial with trend
lm_t_w_sox <- tslm(w_sox_train ~ trend)
lm_t_w_sox_forecast <- forecast(lm_t_w_sox, h = nVal)

# Check accuracies
accuracy(naive_w_sox_forecast, w_sox_val)
accuracy(lm_t_w_sox_forecast, w_sox_val)

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')
lines(naive_w_sox_forecast$mean, col = 'green')
lines(lm_t_w_sox_forecast$mean, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Trend'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# MOVING AVERAGE MODELS
# MA model
ma_w_sox <- rollmean(w_sox_train, k = 5, align = 'right')
ma_w_sox_last <- tail(ma_w_sox, 1)
ma_w_sox_forecast <- ts(rep(ma_w_sox_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Check accuracy
accuracy(ma_w_sox_forecast, w_sox_val)

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')
lines(naive_w_sox_forecast$mean, col = 'green')
lines(ma_w_sox_forecast, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'MA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# HOLT-WINTER MODELS
# Holt's method
holt_w_sox <- ets(w_sox_train, model = 'AAN')
holt_w_sox_forecast <- forecast(holt_w_sox, h = nVal)

# Check accuracy
accuracy(holt_w_sox_forecast, w_sox_val)

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')
lines(holt_w_sox_forecast$mean, col = 'purple')
lines(naive_w_sox_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Holt'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_w_sox <- auto.arima(w_sox_train)
auto_arima_w_sox_forecast <- forecast(auto_arima_w_sox, h = nVal)

# Check accuracy
accuracy(auto_arima_w_sox_forecast, w_sox_val)

# Plot
plot(w_sox_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'White Sox Attendance')
lines(w_sox_train, col = 'blue')
lines(w_sox_val, col = 'red')
lines(auto_arima_w_sox_forecast$mean, col = 'purple')
lines(naive_w_sox_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Auto ARIMA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

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
Acf(tigers_train)
# NOT a random walk

# Plot it out
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')

# TREND AND NAIVE MODELS
# Naive model
naive_tigers <- naive(tigers_train, h = nVal)
naive_tigers_forecast <- forecast(naive_tigers, h = nVal)

# Polynomial with trend
lm_t_tigers <- tslm(tigers_train ~ trend)
lm_t_tigers_forecast <- forecast(lm_t_tigers, h = nVal)

# Check accuracies
accuracy(naive_tigers_forecast, tigers_val)
accuracy(lm_t_tigers_forecast, tigers_val)

# Plot
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')
lines(naive_tigers_forecast$mean, col = 'green')
lines(lm_t_tigers_forecast$mean, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Trend'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# MOVING AVERAGE MODELS
# MA model
ma_tigers <- rollmean(tigers_train, k = 5, align = 'right')
ma_tigers_last <- tail(ma_tigers, 1)
ma_tigers_forecast <- ts(rep(ma_tigers_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Check accuracy
accuracy(ma_tigers_forecast, tigers_val)

# Plot
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')
lines(naive_tigers_forecast$mean, col = 'green')
lines(ma_tigers_forecast, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'MA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# HOLT-WINTER MODELS
# Holt's method
holt_tigers <- ets(tigers_train, model = 'AAN')
holt_tigers_forecast <- forecast(holt_tigers, h = nVal)

# Check accuracy
accuracy(holt_tigers_forecast, tigers_val)

# Plot
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')
lines(holt_tigers_forecast$mean, col = 'purple')
lines(naive_tigers_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Holt'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_tigers <- auto.arima(tigers_train)
auto_arima_tigers_forecast <- forecast(auto_arima_tigers, h = nVal)

# Check accuracy
accuracy(auto_arima_tigers_forecast, tigers_val)

# Plot
plot(tigers_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Tigers Attendance')
lines(tigers_train, col = 'blue')
lines(tigers_val, col = 'red')
lines(auto_arima_tigers_forecast$mean, col = 'purple')
lines(naive_tigers_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Auto ARIMA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

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
Acf(pirates_train)
# NOT a random walk

# Plot it out
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')

# TREND AND NAIVE MODELS
# Naive model
naive_pirates <- naive(pirates_train, h = nVal)
naive_pirates_forecast <- forecast(naive_pirates, h = nVal)

# Polynomial with trend
lm_t_pirates <- tslm(pirates_train ~ trend)
lm_t_pirates_forecast <- forecast(lm_t_pirates, h = nVal)

# Check accuracies
accuracy(naive_pirates_forecast, pirates_val)
accuracy(lm_t_pirates_forecast, pirates_val)

# Plot
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')
lines(naive_pirates_forecast$mean, col = 'green')
lines(lm_t_pirates_forecast$mean, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Trend'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# MOVING AVERAGE MODELS
# MA model
ma_pirates <- rollmean(pirates_train, k = 5, align = 'right')
ma_pirates_last <- tail(ma_pirates, 1)
ma_pirates_forecast <- ts(rep(ma_pirates_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Check accuracy
accuracy(ma_pirates_forecast, pirates_val)

# Plot
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')
lines(naive_pirates_forecast$mean, col = 'green')
lines(ma_pirates_forecast, col = 'purple')
legend('bottomright', c('Train', 'Validation', 'Naive', 'MA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# HOLT-WINTER MODELS
# Holt's method
holt_pirates <- ets(pirates_train, model = 'AAN')
holt_pirates_forecast <- forecast(holt_pirates, h = nVal)

# Check accuracy
accuracy(holt_pirates_forecast, pirates_val)

# Plot
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')
lines(holt_pirates_forecast$mean, col = 'purple')
lines(naive_pirates_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Holt'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_pirates <- auto.arima(pirates_train)
auto_arima_pirates_forecast <- forecast(auto_arima_pirates, h = nVal)

# Check accuracy
accuracy(auto_arima_pirates_forecast, pirates_val)

# Plot
plot(pirates_ts, type = 'n', xlab = 'Year', ylab = 'Attendance', main = 'Pirates Attendance')
lines(pirates_train, col = 'blue')
lines(pirates_val, col = 'red')
lines(auto_arima_pirates_forecast$mean, col = 'purple')
lines(naive_pirates_forecast$mean, col = 'green')
legend('bottomright', c('Train', 'Validation', 'Naive', 'Auto ARIMA'), col = c('blue', 'red', 'green', 'purple'), lty = 1)

# TOMRROW - RUN LINEAR MODELS WITH LAGGED WINS FACTORED IN