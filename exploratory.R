# Exploratory analysis script
rm(list = ls())
library(ggplot2)
library(forecast)
library(tseries)

data <- readRDS("data/data_final.rds")
str(data)

ggplot(data, aes(x=datetime, y=cfs)) +
  geom_line() +
  labs(
    title="Gallatin River Discharge 1989-2024", 
    x="Year", 
    y = "Volume (cubic ft/s)"
  )
#ggsave("plots/discharge_full.png", plot = p)

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

temp <- ts(data[c("cfs", "PRCP", "TMIN")], 
           names = c("cfs", "PRCP", "TMIN"),
           start = c(1989, 212),
           frequency = 365
           )
# pair of prcp and cfs
ggplot2::autoplot(tail(temp, 800), facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Discharge, Precipitation, and Low Temp")

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
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precip",
    x = "Date",
    y = "Volume (cubic ft/s)",
  ) +
  theme_minimal() +  
  theme(legend.position = "bottom")  
ccf(data$PRCP, data$cfs, lag.max = 350)
ccf(diff(data$PRCP, lag = 365), diff(data$cfs, lag = 365))
ccf(diff(data$PRCP, lag = 365), diff(data$cfs, lag = 365), lag.max = 365)

autoplot(tail(temp[,2], 800),) +
  xlab("Year") + ylab("") +
  ggtitle("Precipitation")
ggAcf(data$PRCP, lag.max = 365)
ggPacf(data$PRCP, lag.max = 365)

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
ccf(data$TMIN, data$cfs, lag.max = 365)
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



######################################################
# weekly data
data_weekly = readRDS("data/data_final_weekly.rds")
ggplot(data_weekly, aes(x = week, y = cfs, group = lubridate::year(datetime), color = factor(lubridate::year(datetime)))) +
  geom_line(alpha = 0.8) + 
  labs(
    title = "Discharge Weekly",
    x = "Day of Year",
    y = "Discharge (cubic ft/s)",
    color = "Year"  
  ) +
  theme_minimal()

acf(ts(data_weekly$cfs, frequency = 52))
pacf(ts(data_weekly$cfs, frequency = 52))

acf(diff(data_weekly$cfs, lag = 52))
pacf(diff(data_weekly$cfs, lag = 52))

ccf(data_weekly$PRCP, data_weekly$cfs)
ccf(diff(data_weekly$PRCP, lag = 52), diff(data_weekly$cfs, lag = 52))

ccf(data_weekly$TMIN_PRCP, data_weekly$cfs)
ccf(diff(data_weekly$TMIN_PRCP, lag = 52), diff(data_weekly$cfs, lag = 52))
#####################################################
data_monthly <- readRDS("data/data_final_monthly.rds")

ggplot(data_monthly, aes(x = month, y = cfs, group = lubridate::year(datetime), color = factor(lubridate::year(datetime)))) +
  geom_line(alpha = 0.8) + 
    labs(
    title = "Discharge Monthly",
    x = "Day of Year",
    y = "Discharge (cubic ft/s)",
    color = "Year"  
  ) +
  theme_minimal()

acf(ts(data_monthly$cfs, frequency = 12))
pacf(ts(data_monthly$cfs, frequency = 12))

acf(diff(data_monthly$cfs, lag = 12))
pacf(diff(data_monthly$cfs, lag = 12))



########################
# main problem: making daily sationary
cfs <- ts(data$cfs, 
           names = c("cfs"),
           start = c(1989, 212),
           frequency = 365
           )

autoplot(tail(cfs, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")

# remove seasonality
cfs_sdiff1 <- diff(cfs, lag = 365)

autoplot(tail(cfs_sdiff1, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")

# still have seasonal spikes but at least some of the seasonality is gone...
plot(decompose(cfs_sdiff1, type = "multiplicative"))
plot(decompose(cfs_sdiff1, type = "additive"))
###
# what to do then?
###
# 1: try more seasonal differences?
cfs_sdiff2 <- diff(cfs_sdiff1, lag = 365)
autoplot(tail(cfs_sdiff2, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_sdiff2, type = "multiplicative"))
plot(decompose(cfs_sdiff2, type = "additive"))

cfs_sdiff3 <- diff(cfs_sdiff2, lag = 365)
autoplot(tail(cfs_sdiff3, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_sdiff3, type = "multiplicative"))
plot(decompose(cfs_sdiff3, type = "additive"))

# keep going?
cfs_sdiff4 <- diff(cfs_sdiff3, lag = 365)
autoplot(tail(cfs_sdiff4, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_sdiff4, type = "multiplicative"))
plot(decompose(cfs_sdiff4, type = "additive"))

cfs_sdiff5 <- diff(cfs_sdiff4, lag = 365)
cfs_sdiff6 <- diff(cfs_sdiff5, lag = 365)
cfs_sdiff7 <- diff(cfs_sdiff6, lag = 365)
cfs_sdiff8 <- diff(cfs_sdiff7, lag = 365)
cfs_sdiff9 <- diff(cfs_sdiff8, lag = 365)
cfs_sdiff10 <- diff(cfs_sdiff9, lag = 365)
autoplot(tail(cfs_sdiff10, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_sdiff10, type = "multiplicative"))
plot(decompose(cfs_sdiff10, type = "additive"))
# Hmmm that doesn't seem to help...

# 2. Try a transform? Box cox or log could normalize the jumps
cfs_bc <- BoxCox(cfs, lambda = "auto")
# full
autoplot(cfs_bc) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
#zoomed
autoplot(tail(cfs_bc, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_bc, type = "multiplicative"))
plot(decompose(cfs_bc, type = "additive"))
# and then seasonal difference
cfs_bc_sdiff1 <- diff(cfs_bc, lag = 365)
autoplot(cfs_bc_sdiff1) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
autoplot(tail(cfs_bc_sdiff1, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
plot(decompose(cfs_bc_sdiff1, type = "multiplicative"))
plot(decompose(cfs_bc_sdiff1, type = "additive"))
acf(cfs_bc_sdiff1, lag.max = 365)
pacf(cfs_bc_sdiff1, lag.max = 365)
# still have seasonal periodic jumps at lag 365
# again try more differences
cfs_bc_sdiff2 <- diff(cfs_bc_sdiff1, lag = 365)
autoplot(cfs_bc_sdiff2) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")
autoplot(tail(cfs_bc_sdiff2, 1000)) +
  xlab("Year") + ylab("") +
  ggtitle("CFS")

plot(decompose(cfs_bc_sdiff2, type = "multiplicative"))
plot(decompose(cfs_bc_sdiff2, type = "additive"))
# seasonality is getting worse

# my conclusion: I cannot seem to find a way to remove the seasonality
# which makes the data non-stationary and consequently the models fail.

# this pattern is common for the other covariates collected as well.