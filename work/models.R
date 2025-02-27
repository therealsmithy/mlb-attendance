library(dplyr)
library(forecast)
library(fpp3)
library(zoo)
library(ggplot2)

# CHANGE VAL TO 3 SO THAT 2021 DOES NOT INFLUENCE

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

# Create lagged wins
w_sox$wins_lag <- lag(w_sox$wins, 1)

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

# Create lagged wins
tigers$wins_lag <- lag(tigers$wins, 1)

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

# Create lagged wins
pirates$wins_lag <- lag(pirates$wins, 1)

# FULL LEAGUE MODEL
# Create time series object
full_ts <- ts(full_league$attendance, start = 1913, end = 2024)
nVal <- 3
nTrain <- length(full_ts) - nVal
full_train <- window(full_ts, start = 1913, end = c(1913, nTrain))
full_val <- window(full_ts, start = c(1913, nTrain + 1), end = c(1913, nTrain + nVal))

# Test for Random Walk
rw_full <- Arima(full_train, order = c(1, 0, 0))
summary(rw_full)
t_stat <- (0.9766 - 1) / 0.0209
2 * pnorm(t_stat)
# Random Walk...

# Acf of full league and differenced
Acf(full_ts)
Acf(diff(full_ts))

# Plot
autoplot(full_train) +
  autolayer(full_val, series = 'Validation') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Full League Attendance') + 
  theme_minimal()

# Naive model
naive_full <- naive(full_train, h = nVal)
naive_full_forecast <- forecast(naive_full, h = nVal)

# Arimax model
arimax_full <- auto.arima(full_train, xreg = full_league$Avg[1:length(full_train)])
arimax_full_forecast <- forecast(arimax_full, xreg = full_league$Avg[(length(full_train) + 1):(length(full_train) + nVal)])

# Armiax with lagged predictors
arimax_full_lag <- auto.arima(full_train, xreg = full_league$cpi_lag[1:length(full_train)])
arimax_full_lag_forecast <- forecast(arimax_full_lag, xreg = full_league$cpi_lag[(length(full_train) + 1):(length(full_train) + nVal)])

# Check accuracy of three models
accuracy(naive_full_forecast, full_val)
accuracy(arimax_full_forecast, full_val)
accuracy(arimax_full_lag_forecast, full_val)
# Lagged Arimax actually performs the best!

# Plot three forecasts
autoplot(full_ts) +
  autolayer(full_train, series = 'Train') +
  autolayer(full_val, series = 'Validation') +
  autolayer(naive_full_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_full_forecast, series = 'Arimax', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_full_lag_forecast, series = 'Arimax Lagged', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Full League Attendance') + 
  theme_minimal()

# Use lagged arimax model for forecast
# To do this we have to predict CPI for 2025 to 2027
# Fit simple linear model to estimate future CPI
cpi_years <- seq_along(full_league$cpi)
cpi_model <- lm(full_league$cpi ~ cpi_years)
cpi_future_years <- (max(cpi_years) + 1):(max(cpi_years) + 3)
cpi_future <- predict(cpi_model, newdata = data.frame(cpi_years = cpi_future_years))

# Forecast
arimax_full_forecast <- forecast(arimax_full_lag, xreg = cpi_future, h = 3)

# Plot
autoplot(full_ts) +
  autolayer(arimax_full_forecast) +
  xlab('Year') + ylab('Attendance') +
  ggtitle('Full League Attendance Forecast: ARIMAX with Lagged CPI') +
  theme_minimal()

# WHITE SOX MODEL
# Create time series object
w_sox_ts <-ts(w_sox$attendance, start = 1903, end = 2024)
nTrain <- length(w_sox_ts) - nVal
nVal <- 3
w_sox_train <- window(w_sox_ts, start = 1903, end = c(1903, nTrain))
w_sox_val <- window(w_sox_ts, start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal))

# Test for Random Walk
rw_w_sox <- Arima(w_sox_train, order = c(1, 0, 0))
summary(rw_w_sox)
t_stat <- (0.9074 - 1) / 0.0374
2 * pnorm(t_stat)
# NOT a random walk

# White Sox Acf and differenced Acf
Acf(w_sox_train)
Acf(diff(w_sox_train))

# Plot
autoplot(w_sox_train) +
  autolayer(w_sox_val, series = 'Validation') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('White Sox Attendance') + 
  theme_minimal()

# TREND AND NAIVE MODELS
# Naive model
naive_w_sox <- naive(w_sox_train, h = nVal)
naive_w_sox_forecast <- forecast(naive_w_sox, h = nVal)

# Linear with trend
lm_t_w_sox <- tslm(w_sox_train ~ trend)
lm_t_w_sox_forecast <- forecast(lm_t_w_sox, h = nVal)

# Polynomial with trend
lm_p_w_sox <- tslm(w_sox_train ~ trend + I(trend^2))
lm_p_w_sox_forecast <- forecast(lm_p_w_sox, h = nVal)

# Check accuracies
accuracy(naive_w_sox_forecast, w_sox_val)
accuracy(lm_t_w_sox_forecast, w_sox_val)
accuracy(lm_p_w_sox_forecast, w_sox_val)

# Plot
autoplot(w_sox_ts) +
  autolayer(w_sox_train, series = 'Train') +
  autolayer(w_sox_val, series = 'Validation') +
  autolayer(naive_w_sox_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(lm_t_w_sox_forecast, series = 'Trend', PI = FALSE, linetype = 'dashed') +
  autolayer(lm_p_w_sox_forecast, series = 'Polynomial', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('White Sox Naive/Trend Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/w_sox_naive_trend.jpeg', width = 10, height = 6, dpi = 300)

# MA AND HOLT
# MA model
ma_w_sox <- rollmean(w_sox_train, k = 3, align = 'right')
ma_w_sox_last <- tail(ma_w_sox, 1)
ma_w_sox_forecast <- ts(rep(ma_w_sox_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Holt's method
holt_w_sox <- ets(w_sox_train, model = 'AAN')
holt_w_sox_forecast <- forecast(holt_w_sox, h = nVal)

# Check accuracy
accuracy(ma_w_sox_forecast, w_sox_val)
accuracy(holt_w_sox_forecast, w_sox_val)

# Plot
autoplot(w_sox_ts) +
  autolayer(w_sox_train, series = 'Train') +
  autolayer(w_sox_val, series = 'Validation') +
  autolayer(naive_w_sox_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(ma_w_sox_forecast, series = 'MA', PI = FALSE, linetype = 'dashed') +
  autolayer(holt_w_sox_forecast, series = 'Holt', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('White Sox MA and Holt Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/w_sox_ma_holt.jpeg', width = 10, height = 6, dpi = 300)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_w_sox <- auto.arima(w_sox_train)
auto_arima_w_sox_forecast <- forecast(auto_arima_w_sox, h = nVal)

# Arimax with wins
arimax_w_sox <- auto.arima(w_sox_train, xreg = w_sox$wins[1:length(w_sox_train)])
arimax_w_sox_forecast <- forecast(arimax_w_sox, xreg = w_sox$wins[(length(w_sox_train) + 1):(length(w_sox_train) + nVal)])

# Arimax with lagged wins
arimax_w_sox_lag <- auto.arima(w_sox_train, xreg = w_sox$wins_lag[1:length(w_sox_train)])
arimax_w_sox_lag_forecast <- forecast(arimax_w_sox_lag, xreg = w_sox$wins_lag[(length(w_sox_train) + 1):(length(w_sox_train) + nVal)])

# Check accuracies
accuracy(auto_arima_w_sox_forecast, w_sox_val)
accuracy(arimax_w_sox_forecast, w_sox_val)
accuracy(arimax_w_sox_lag_forecast, w_sox_val)

# Plot
autoplot(w_sox_ts) +
  autolayer(w_sox_train, series = 'Train') +
  autolayer(w_sox_val, series = 'Validation') +
  autolayer(naive_w_sox_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(auto_arima_w_sox_forecast, series = 'Auto ARIMA', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_w_sox_forecast, series = 'Arimax', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_w_sox_lag_forecast, series = 'Arimax Lagged', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('White Sox ARIMA Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/w_sox_arima.jpeg', width = 10, height = 6, dpi = 300)

# Model Deployment
# Predict
auto_arima_w_sox_future <- auto.arima(w_sox_ts, xreg = w_sox$wins_lag)

# Fit simple linear model to estimate future wins
win_years <- seq_along(w_sox$wins)
w_sox_win_model <- lm(w_sox$wins ~ win_years)
w_sox_future_years <- (max(win_years) + 1):(max(win_years) + 4)
w_sox_future_wins <- predict(w_sox_win_model, newdata = data.frame(win_years = w_sox_future_years))
# Lag future wins
w_sox_future_wins_lag <- lag(w_sox_future_wins, 1)

# Forecast
arimax_w_sox_forecast <- forecast(arimax_w_sox_lag, xreg = na.omit(w_sox_future_wins_lag), h = 5)

# Plot
autoplot(w_sox_ts) +
  autolayer(arimax_w_sox_forecast) +
  xlab('Year') + ylab('Attendance') +
  ggtitle('White Sox Attendance Forecast: SARIMAX with Lagged Wins (2025-2027)') +
  theme_minimal()
#ggsave('vizzes/w_sox_forecast.jpeg', width = 10, height = 6, dpi = 300)

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
# NOT a random walk

# Acf of Tigers and differenced
Acf(tigers_train)
Acf(diff(tigers_train))

# Plot it out
autoplot(tigers_ts) +
  autolayer(tigers_train, series = 'Train') +
  autolayer(tigers_val, series = 'Validation') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Tigers Attendance') + 
  theme_minimal()

# TREND AND NAIVE MODELS
# Naive model
naive_tigers <- naive(tigers_train, h = nVal)
naive_tigers_forecast <- forecast(naive_tigers, h = nVal)

# Linear with trend
lm_t_tigers <- tslm(tigers_train ~ trend)
lm_t_tigers_forecast <- forecast(lm_t_tigers, h = nVal)

# Polynomial with trend
lm_p_tigers <- tslm(tigers_train ~ trend + I(trend^2))
lm_p_tigers_forecast <- forecast(lm_p_tigers, h = nVal)

# Check accuracies
accuracy(naive_tigers_forecast, tigers_val)
accuracy(lm_t_tigers_forecast, tigers_val)
accuracy(lm_p_tigers_forecast, tigers_val)

# Plot
autoplot(tigers_ts) +
  autolayer(tigers_train, series = 'Train') +
  autolayer(tigers_val, series = 'Validation') +
  autolayer(naive_tigers_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(lm_t_tigers_forecast, series = 'Trend', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Tigers Naive/Trend Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/tigers_naive_trend.jpeg', width = 10, height = 6, dpi = 300)

# MA and Holt
# MA model
ma_tigers <- rollmean(tigers_train, k = 3, align = 'right')
ma_tigers_last <- tail(ma_tigers, 1)
ma_tigers_forecast <- ts(rep(ma_tigers_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Holt's method
holt_tigers <- ets(tigers_train, model = 'AAN')
holt_tigers_forecast <- forecast(holt_tigers, h = nVal)

# Check accuracies
accuracy(holt_tigers_forecast, tigers_val)
accuracy(ma_tigers_forecast, tigers_val)

# Plot
autoplot(tigers_ts) +
  autolayer(tigers_train, series = 'Train') +
  autolayer(tigers_val, series = 'Validation') +
  autolayer(naive_tigers_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(ma_tigers_forecast, series = 'MA', PI = FALSE, linetype = 'dashed') +
  autolayer(holt_tigers_forecast, series = 'Holt', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Tigers MA/Holt Attendance Model (2020-2024)') + 
  theme_minimal()
#ggsave('vizzes/tigers_naive_ma.jpeg', width = 10, height = 6, dpi = 300)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_tigers <- auto.arima(tigers_train)
auto_arima_tigers_forecast <- forecast(auto_arima_tigers, h = nVal)

# Arimax
arimax_tigers <- auto.arima(tigers_train, xreg = tigers$wins[1:length(tigers_train)])
arimax_tigers_forecast <- forecast(arimax_tigers, xreg = tigers$wins[(length(tigers_train) + 1):(length(tigers_train) + nVal)])

# Arimax with lagged wins
arimax_tigers_lag <- auto.arima(tigers_train, xreg = tigers$wins_lag[1:length(tigers_train)])
arimax_tigers_lag_forecast <- forecast(arimax_tigers_lag, xreg = tigers$wins_lag[(length(tigers_train) + 1):(length(tigers_train) + nVal)])

# Check accuracies
accuracy(auto_arima_tigers_forecast, tigers_val)
accuracy(arimax_tigers_forecast, tigers_val)
accuracy(arimax_tigers_lag_forecast, tigers_val)

# Plot
autoplot(tigers_ts) +
  autolayer(tigers_train, series = 'Train') +
  autolayer(tigers_val, series = 'Validation') +
  autolayer(naive_tigers_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(auto_arima_tigers_forecast, series = 'Auto ARIMA', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_tigers_forecast, series = 'Sarimax', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_tigers_lag_forecast, series = 'Sarimax Lagged', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Tigers ARIMA Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/tigers_arima.jpeg', width = 10, height = 6, dpi = 300)

# Predict with MA
# MA assumes simple average continues
ma_tigers_future <- rollmean(tigers_ts, k = 3, align = 'right')
ma_tigers_last <- tail(ma_tigers_future, 1)
ma_tigers_forecast <- rep(ma_tigers_last, 3)
ma_tigers_forecast_ts <- ts(ma_tigers_forecast, start = 2025, end = 2027)

# Plot
autoplot(tigers_ts) +
  autolayer(ma_tigers_forecast_ts, series = 'Forecast') +
  xlab('Year') + ylab('Attendance') +
  ggtitle("Tigers Attendance Forecast: Moving Average Method") +
  theme_minimal()
#ggsave('vizzes/tigers_forecast.jpeg', width = 10, height = 6, dpi = 300)

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
# NOT a random walk

# Acf of Pirates and differenced
Acf(pirates_train)
Acf(diff(pirates_train))

# Plot it out
autoplot(pirates_ts) +
  autolayer(pirates_train, series = 'Train') +
  autolayer(pirates_val, series = 'Validation') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Pirates Attendance') + 
  theme_minimal()

# TREND AND NAIVE MODELS
# Naive model
naive_pirates <- naive(pirates_train, h = nVal)
naive_pirates_forecast <- forecast(naive_pirates, h = nVal)

# Linear with trend
lm_t_pirates <- tslm(pirates_train ~ trend)
lm_t_pirates_forecast <- forecast(lm_t_pirates, h = nVal)

# Polynomial with trend
lm_p_pirates <- tslm(pirates_train ~ trend + I(trend^2))
lm_p_pirates_forecast <- forecast(lm_p_pirates, h = nVal)

# Check accuracies
accuracy(naive_pirates_forecast, pirates_val)
accuracy(lm_t_pirates_forecast, pirates_val)
accuracy(lm_p_pirates_forecast, pirates_val)

# Plot
autoplot(pirates_ts) +
  autolayer(pirates_train, series = 'Train') +
  autolayer(pirates_val, series = 'Validation') +
  autolayer(naive_pirates_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(lm_t_pirates_forecast, series = 'Trend', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Pirates Naive/Trend Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/pirates_naive_trend.jpeg', width = 10, height = 6, dpi = 300)

# MA and Holt
# MA model
ma_pirates <- rollmean(pirates_train, k = 3, align = 'right')
ma_pirates_last <- tail(ma_pirates, 1)
ma_pirates_forecast <- ts(rep(ma_pirates_last, nVal), start = c(1903, nTrain + 1), end = c(1903, nTrain + nVal),
                        frequency = 1)

# Holt's method
holt_pirates <- ets(pirates_train, model = 'AAN')
holt_pirates_forecast <- forecast(holt_pirates, h = nVal)

# Check accuracy
accuracy(ma_pirates_forecast, pirates_val)
accuracy(holt_pirates_forecast, pirates_val)

# Plot
autoplot(pirates_ts) +
  autolayer(pirates_train, series = 'Train') +
  autolayer(pirates_val, series = 'Validation') +
  autolayer(naive_pirates_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(ma_pirates_forecast, series = 'MA', PI = FALSE, linetype = 'dashed') +
  autolayer(holt_pirates_forecast, series = 'Holt', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Pirates MA/Holt Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/pirates_ma_holt.jpeg', width = 10, height = 6, dpi = 300)

# ARIMA MODELS
# Auto ARIMA first
auto_arima_pirates <- auto.arima(pirates_train)
auto_arima_pirates_forecast <- forecast(auto_arima_pirates, h = nVal)

# Arimax
arimax_pirates <- auto.arima(pirates_train, xreg = pirates$wins[1:length(pirates_train)])
arimax_pirates_forecast <- forecast(arimax_pirates, xreg = pirates$wins[(length(pirates_train) + 1):(length(pirates_train) + nVal)])

# Arimax with lagged wins
arimax_pirates_lag <- auto.arima(pirates_train, xreg = pirates$wins_lag[1:length(pirates_train)])
arimax_pirates_lag_forecast <- forecast(arimax_pirates_lag, xreg = pirates$wins_lag[(length(pirates_train) + 1):(length(pirates_train) + nVal)])

# Check accuracies
accuracy(auto_arima_pirates_forecast, pirates_val)
accuracy(arimax_pirates_forecast, pirates_val)
accuracy(arimax_pirates_lag_forecast, pirates_val)

# Plot
autoplot(pirates_ts) +
  autolayer(pirates_train, series = 'Train') +
  autolayer(pirates_val, series = 'Validation') +
  autolayer(naive_pirates_forecast, series = 'Naive', PI = FALSE, linetype = 'dashed') +
  autolayer(auto_arima_pirates_forecast, series = 'Auto ARIMA', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_pirates_forecast, series = 'Sarimax', PI = FALSE, linetype = 'dashed') +
  autolayer(arimax_pirates_lag_forecast, series = 'Sarimax Lagged', PI = FALSE, linetype = 'dashed') +
  xlab('Year') + ylab('Attendance') + 
  ggtitle('Pirates ARIMA Attendance Model (2022-2024)') + 
  theme_minimal()
#ggsave('vizzes/pirates_arima.jpeg', width = 10, height = 6, dpi = 300)

# Predict with MA
# MA assumes simple average continues
ma_pirates_future <- rollmean(pirates_ts, k = 3, align = 'right')
ma_pirates_last <- tail(ma_pirates_future, 1)
ma_pirates_forecast <- rep(ma_pirates_last, 3)
ma_pirates_forecast_ts <- ts(ma_pirates_forecast, start = 2025, end = 2027)

# Plot
autoplot(pirates_ts) +
  autolayer(ma_pirates_forecast_ts, series = 'Forecast') +
  xlab('Year') + ylab('Attendance') +
  ggtitle("Pirates Attendance Forecast: Moving Average Method") +
  theme_minimal()
#ggsave('vizzes/pirates_forecast.jpeg', width = 10, height = 6, dpi = 300)

# Plot full time series for each team
autoplot(w_sox_ts, series = 'White Sox') +
  autolayer(tigers_ts, series = 'Tigers') +
  autolayer(pirates_ts, series = 'Pirates') +
  xlab('Year') + ylab('Attendance') +
  ggtitle('Attendance History: 1903-2024') +
  theme_minimal()
#ggsave('vizzes/attendance_ts.jpeg', width = 10, height = 6, dpi = 300)