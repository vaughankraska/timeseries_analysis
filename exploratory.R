# Exploratory analysis script
library(ggplot2)
library(forecast)
library(tseries)

data <- readRDS("data/data_final.rds")
str(data)

ggplot(data, aes(x=datetime, y=cfs)) +
  geom_line() +
  labs(
    title="Gallatin River Discharge 1981-2024", 
    x="Year", 
    y = "Volume (cubic ft/s)"
  )
# variance over time
plot(tail(data$datetime, -1), diff(data$cfs)^2, main = "Estimated Daily Variance")

# look at only last four years
ggplot(data %>% 
         dplyr::filter(datetime >= as.Date("2021-01-01")) %>%
         dplyr::filter(datetime <= as.Date("2024-01-01"))
       , aes(x=datetime, y=cfs)) +
  geom_line() +
  labs(
    title="Gallatin River Discharge 1981-2024", 
    x="Year", 
    y = "Volume (cubic ft/s)"
  )

# look at distribution across day of year
df <- data[c("datetime", "cfs")]
df$doy <- lubridate::yday(df$datetime)
ggplot(df, aes(x = doy, y = cfs)) +
  geom_point(alpha = 0.3, color = "gray") +  # Alpha for transparency
  geom_smooth(color = "blue", se = TRUE) +  # LOESS for smooth trend line
  labs(
    title = "Density of Discharge Across Day of Year",
    x = "Day of Year",
    y = "Discharge (cubic ft/s)"
  ) +
  theme_minimal()  # Minimalist theme

# and lines grouped by year
df$year = lubridate::year(df$datetime)
ggplot(df, aes(x = doy, y = cfs, group = year, color = factor(year))) +
  geom_line(alpha = 0.8) + 
  labs(
    title = "Discharge Across Day of Year by Year",
    x = "Day of Year",
    y = "Discharge (cubic ft/s)",
    color = "Year"  
  ) +
  theme_minimal()

############################
# autocorrelation analysis
df <- ts(data$cfs, start = data$datetime[1], frequency = 365)
adf_result <- adf.test(df, alternative = "stationary")
adf_result # already stationary
plot(decompose(df))

df <- ts(data$cfs, start = data$datetime[1])
acf(df, main="ACF", lag.max = 370)
# see on giant swing because of the annual cycles in our data

pacf(df, main="PACF", lag.max = 370)
# see heavy autocorr at lag 1, 2 and then drops off but comes back up 
# around the 365 lag mark

# try a 1 day and 365 day diff
df <- diff(df, lag = 365)
plot(df)
acf(df, main="ACF", lag.max = 370)
pacf(df, main="PACF", lag.max = 370)

model <- auto.arima(df, num.cores = 16)
tsdiag(model, gof.lag = 40)
# best I could do model:
# diff 365
#Series: df 
#ARIMA(5,0,5) with zero mean 
# still has terrible homoskedasticity


###############
# look into input variables now
summary(data)
# PRECIP
PRECIP_SCALE = 5
ggplot(data, aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = PRCP, color = "red"), size = 1) +  # Line for Precip
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
ccf(data$PRCP, data$cfs)
ccf(diff(data$PRCP, lag = 365), diff(data$cfs, lag = 365))

## SNOW
ggplot(data, aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = SNOW, color = "red"), size = 1) +  # Line for Precip
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


# TMIN
ggplot(data, aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = TMIN, color = "red"), size = 1) +  # Line for Precip
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
ccf(data$TMIN, data$cfs)
ccf(diff(data$TMIN, lag = 365), diff(data$cfs, lag = 365), lag.max = 20)


# TMAX
ggplot(data, aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = TMAX, color = "red"), size = 1) +  # Line for Precip
  scale_y_continuous(
    name = "Volume (cubic ft/s)",
    sec.axis = sec_axis(~ . , name = "Precipitation (dc) * 5")  
  ) +
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precip",
    x = "Date",
    y = "Volume (cubic ft/s)",
  ) +
  theme_minimal() +  
  theme(legend.position = "bottom")  
ccf(data$TMAX, data$cfs)


# TMAX * precip
p <- ggplot(data, aes(x = datetime)) +
  geom_line(aes(y = cfs, color = "blue"), size = 1) +  # Line for cfs
  geom_line(aes(y = TMAX*PRCP*.02, color = "red"), size = 1) +  # Line for Precip
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precipitation * Daily High Temp/5",
    x = "Date",
    y = "cfs (red) and HighTemp*Precipitation (blue)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p
ggsave(
  filename = "plots/cfs_and_TMAX_PRCP_ts.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 300
)
ccf((data$TMIN_PRCP), (data$cfs))

#### Correlation between vars:
cor_matrix <- cor(data[c("cfs", "PRCP", "SNOW", "TMAX", "TMIN", "TMAX_PRCP", "TMIN_PRCP")], use = "complete.obs")
cor_with_y <- cor_matrix["cfs", ]
cor_with_y




