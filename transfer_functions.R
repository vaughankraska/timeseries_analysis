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
    sec.axis = sec_axis(~ . , name = "")  # Secondary axis
  ) +
  labs(
    title = "Time Series Runoff (cubic ft/s) and Precip",
    x = "Date",
    y = "Volume (cubic ft/s)",
  ) +
  theme_minimal() +  
  theme(legend.position = "bottom")  

# Start with TMIN input and find model for that
x <- ts(data$TMIN, start = data$datetime[1], frequency = 365)
plot(decompose(ts(data$TMIN, start = data$datetime[1], frequency = 365)))

par(mfrow = c(2, 1))
acf(x, lag.max = 370)
pacf(x, lag.max = 370)

model <- arima(diff(x, lag=365), order = c(3, 0, 1), include.mean = FALSE)
# model <- auto.arima(
#   x, 
#   ic = "bic",
#   seasonal = TRUE,
#   parallel = TRUE,
#   stepwise = FALSE,
#   num.cores =  parallel::detectCores() - 1,
# )
# BEST
# ARIMA(3,0,1)(0,1,0)[365] 
#Coefficients:
#  ar1      ar2     ar3      ma1
#1.4921  -0.6479  0.0871  -0.7716
#s.e.  0.1052   0.0725  0.0090   0.1056
#sigma^2 = 3079:  log likelihood = -67007.11
#AIC=134024.2   AICc=134024.2   BIC=134061.3
tsdiag(model, gof.lag = 40)

###### whiten the output
y <- data$cfs
phi1 <- as.numeric(model$coef[1])
phi2 <- as.numeric(model$coef[2])
phi3 <- as.numeric(model$coef[3])
theta1 <- as.numeric(model$coef[4])


# Apply AR part filtering to y
ytilde <- stats::filter(y, filter = c(1, -phi1, -phi2, -phi3), method = "convolution", sides = 1)

# Apply AR part filtering to w
w <- stats::filter(x, filter = c(1, -phi1, -phi2, -phi3), method = "convolution", sides = 1)

# Remove the first 3 values due to AR filter initialization
n <- length(y)
ytilde1 <- ytilde[seq(4, n)]
w1 <- w[seq(4, n)]

# Apply MA(1) correction
ytilde1 <- ytilde1 - theta1 * lag(ytilde1, k = 1)
w1 <- w1 - theta1 * lag(w1, k = 1)

# Remove the first value due to MA filter initialization
ytilde1 <- ytilde1[-1]
w1 <- w1[-1]

# Compute cross-correlation
ccf(ytilde1, w1, ylab = "CCF")






