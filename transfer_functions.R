# USE transfer function to forecast runoff
library(forecast)
library(ggplot2)

data = readRDS("data/data_final.rds")

ggplot(data %>% 
         dplyr::filter(datetime >= as.Date("2015-03-01")) %>%
         dplyr::filter(datetime <= as.Date("2015-07-01")),
       aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = TMIN * 5, color = "red"), size = 1) +  # Line for Precip
  scale_y_continuous(
    name = "Volume (cubic ft/s)",
    sec.axis = sec_axis(~ . , name = "Precipitation (dc) * 5")  # Secondary axis
  ) +
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precip",
    x = "Date",
    y = "Volume (cubic ft/s)",
  ) +
  theme_minimal() +  
  theme(legend.position = "bottom")  

# Start with TMIN input and find model for that
x <- ts(data$TMIN, start = data$datetime[1])
plot(decompose(ts(data$TMIN, start = data$datetime[1], frequency = 365)))

par(mfrow = c(2, 1))
acf(x, lag.max = 370)
pacf(x, lag.max = 370)

model <- auto.arima(
  x, 
  ic = "bic",
  seasonal = FALSE,
  #parallel = TRUE,
  stepwise = FALSE,
  num.cores =  parallel::detectCores() -1,
)
tsdiag(model, gof.lag = 40)
