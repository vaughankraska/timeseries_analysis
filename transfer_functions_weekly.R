# implement transfer function for weekly data
rm(list = ls())
library(TSA)
library(forecast)
library(ggplot2)

data <- readRDS("data/data_final_weekly.rds")

ggplot(data %>% 
         dplyr::filter(datetime >= as.Date("2015-02-01")) %>%
         dplyr::filter(datetime <= as.Date("2015-09-01")),
       aes(x = datetime)) +
  geom_line(aes(y = cfs), size = 1) + # cfs 
  geom_line(aes(y = PRCP, color = "red"), size = 1) +  
  scale_y_continuous(
    name = "Volume (cubic ft/s)",
    sec.axis = sec_axis(~ . , name = "")  # Secondary axis
  ) +
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precip",
    x = "Date",
    y = "Volume (cubic ft/s)",
  ) +
  theme_minimal()  

# try basic SARIMA first
y <- ts(data$cfs, start = data$datetime[1])
acf(y, lag.max = 52)
pacf(y, lag.max = 52)

acf(diff(y, lag = 52), lag.max = 52)
pacf(diff(y, lag = 52), lag.max = 52)
# check one more seasonal diff D = 2 [52]
acf(diff(diff(y, lag = 52),lag = 52), lag.max = 52)
pacf(diff(diff(y, lag = 52),lag = 52), lag.max = 52)

y <- data$cfs
ar <- TSA::arima(
  diff(log(y), lag = 52),
  order = c(2, 0, 1),
  seasonal = list(order = c(0, 0, 0), period = NA),
  include.mean = FALSE,
); lmtest::coeftest(ar)
qqnorm(ar$residuals)
hist(ar$residuals)
tsdiag(ar, gof.lag = 52) # best SARIMA(2, 0, 1)(0, 1, 0)[52], aic = -345
#still not perfectly stationary residuals

#################################################
#### Try transfer functions with Temp max * Precip
# y <- diff(ts(log(data$cfs), start = data$datetime[52]), lag = 52)
# x <- diff(ts(data$TMAX_PRCP, start = data$datetime[52]), lag = 52)
y <- ts(log(data$cfs), start = data$datetime[1])
x <- ts(data$TMAX_PRCP, start = data$datetime[1])
ccf(x, y, lag.max = 52)
n <- length(y)
r <- 0 # autoregressive part of the transfer function. influence of past x on y
s <- 1 # number of periods after which the effect of x is seen in y

model <- TSA::arima(
  y,
  order = c(2, 0, 1),
  seasonal = list(order = c(1, 1, 1), period = 52),
  xtransf = x,
  transfer = list(c(r, s)), # our (r, s)
  include.mean = FALSE,
  method = "ML"
); lmtest::coeftest(model)
# barplot(coef(model)[-(1:2)], las = 2)
e_t <- ts(model$residuals[seq(max(s, r)+1, n)])
adf.test(e_t, alternative = "stationary") # allegedly stationary
plot(e_t)
acf(e_t, lag.max = 52)
pacf(e_t, lag.max = 52)

#continue to fit an ARMA model on our errors (eta) even though
#the plots look pretty bad no matter what you try in the above step.
me <- forecast::auto.arima(
  e_t,
); me # auto forecast says no forecast 


#plot forecast from earlier SARIMA(2, 0, 1) X (0, 1, 0)[52]
m <- stats::arima(
  ts(log(data$cfs), start = data$datetime[1]),
  order = c(2, 0, 1),
  seasonal = list(order = c(0, 1, 0), period = 52),
  include.mean = FALSE,
); lmtest::coeftest(m);

obs_data <- data.frame(
  time = time(ts(log(data$cfs), start = data$datetime[1])),
  value = as.numeric(ts(log(data$cfs), start = data$datetime[1]))
)
forecast_20 <- forecast(m, h = 20)
forecast_data <- data.frame(
  date = seq.Date(
    from = data$datetime[dim(data)[1]],
    by = "week",
    length.out = 20
  ),
  forecast = exp(forecast_20$mean),  # Back-transforming
  lower_80 = exp(forecast_20$lower[, 1]),  # Back-transforming CI bounds
  upper_80 = exp(forecast_20$upper[, 1]),
  lower_95 = exp(forecast_20$lower[, 2]),
  upper_95 = exp(forecast_20$upper[, 2])
)

# Get the last 30 observations
obs_data <- data.frame(
  date = data$datetime,
  value = exp(ts(log(data$cfs), start = data$datetime[1]))
)

obs_data_recent <- tail(obs_data, 30)
ggplot() +
  geom_line(
    data = obs_data_recent, 
    aes(x = date, y = value), color = "blue"
  ) +
  geom_ribbon(
    data = forecast_data, 
    aes(x = date, ymin = lower_95, ymax = upper_95), 
    fill = "red", alpha = 0.2
  ) +
  geom_ribbon(
    data = forecast_data, 
    aes(x = date, ymin = lower_80, ymax = upper_80), 
    fill = "orange", alpha = 0.2
  ) +
  geom_line(
    data = forecast_data, 
    aes(x = date, y = forecast), 
    color = "red"
  ) +
  labs(title = "20-Step-Ahead Weekly Forecast SARIMA(2, 0, 1)X(0, 1, 0)[52]", 
       x = "Date", y = "Volume (cubic ft/s") +
  theme_minimal()

