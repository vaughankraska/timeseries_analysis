# USE transfer function to forecast runoff
library(forecast)
library(ggplot2)

data = readRDS("data/data_final.rds")

ggplot(data %>% 
         dplyr::filter(datetime >= as.Date("2015-03-01")) %>%
         dplyr::filter(datetime <= as.Date("2015-07-01")),
       aes(x = datetime)) +
  geom_line(aes(y = cfs), size = 1) + # cfs 
  geom_line(aes(y = PRCP * 3, color = "red"), size = 1) +  
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
x <- ts(data$PRCP, start = data$datetime[1], frequency = 365)
plot(decompose(ts(data$PRCP, start = data$datetime[1], frequency = 365)))

par(mfrow = c(2, 1))
acf(x, lag.max = 370)
pacf(x, lag.max = 370)

# model <- arima(diff(x, lag=365), order = c(3, 0, 1), include.mean = FALSE)# best for TMIN
model <- arima(diff(x, lag=365), order = c(3, 0, 2), include.mean = FALSE)# best for PRCP

# model <- auto.arima(
#   x,
#   ic = "bic",
#   seasonal = TRUE,
#   #parallel = TRUE, #only works on not windows
#   stepwise = FALSE,
#   num.cores =  parallel::detectCores() - 1,
# )
# BEST (x = TMIN)
# ARIMA(3,0,1)(0,1,0)[365] 
#Coefficients:
#  ar1      ar2     ar3      ma1
#1.4921  -0.6479  0.0871  -0.7716
#s.e.  0.1052   0.0725  0.0090   0.1056
#sigma^2 = 3079:  log likelihood = -67007.11
#AIC=134024.2   AICc=134024.2   BIC=134061.3

# BEST: x as PRCP
# ARIMA(1,0,2) with non-zero mean 
# 
# Coefficients:
#   ar1      ma1      ma2    mean
# 0.9715  -0.7956  -0.1388  9.3849
# s.e.  0.0052   0.0103   0.0090  0.5447
# 
# sigma^2 = 715:  log likelihood = -59724.56
# AIC=119459.1   AICc=119459.1   BIC=119496.4
model;tsdiag(model, gof.lag = 40)

###### whiten the output
y <- diff(data$cfs, lag = 365)
x <- diff(data$PRCP, lag = 365) # same transform as when I estimated the model
# phi1 <- as.numeric(model$coef[1])
# phi2 <- as.numeric(model$coef[2])
# phi3 <- as.numeric(model$coef[3])
# theta1 <- as.numeric(model$coef[4])
# ^ X_INPUT = TMIN
# PRCP
phi1 <- as.numeric(model$coef[1])
phi2 <- as.numeric(model$coef[2])
phi3 <- as.numeric(model$coef[3])
theta1 <- as.numeric(model$coef[4])
theta2 <- as.numeric(model$coef[5])

# Apply AR part filtering to y
ytilde <- stats::filter(y, filter = c(1, -phi1, -phi2, -phi3), method = "convolution", sides = 1)

# apply AR part filtering to w
w <- stats::filter(x, filter = c(1, -phi1, -phi2, -phi3), method = "convolution", sides = 1)

# remove first 3 values due to AR filter initialization
n <- length(y)
ytilde1 <- ytilde[seq(4, n)]
w1 <- w[seq(4, n)]

# apply MA(1) 
#ytilde1 <- ytilde1 - theta1 * lag(ytilde1, k = 1) # TMIN
ytilde1 <- ytilde1 - theta1 * lag(ytilde1, k = 1)
w1 <- w1 - theta1 * lag(w1, k = 1)

# Remove the first value due to MA filter initialization
ytilde1 <- ytilde1[-1]
w1 <- w1[-1]

# Compute cross-correlation
ccf_res <- ccf(ytilde1, w1, ylab = "CCF", lag.max = 40)
plot(ccf_res, xaxt = 'n'); axis(1, at = seq(-40, 40, by = 2))
# (input=TMIN)concerning that we have a significant corr at lag=-1 
# but otherwise it looks good, there are significant leaders at 0, 1, 2
# and it decays a bit, has me thinking models similar to:
# yt = y0...y3 + x0...x3

#############
# explore regressions now
max_behind <- 3
y0 <- y[seq(max_behind, n)]
y1 <- y[seq(max_behind-1, n-1)]
y2 <- y[seq(max_behind-2, n-2)]

x0 <- x[seq(max_behind, n)]
x1 <- x[seq(max_behind-1, n-1)]
x2 <- x[seq(max_behind-2, n-2)]

r_model <- lm(formula = y0 ~ x2 + y1 + y2 - 1)
summary(r_model)

# chosen regression model: 
# yt = -0.148*xt-2 +  1.387 * yt-1 + -0.446 * yt-2 + u_t
# with u_t = (1 - 1.387B + 0.446B^2)ηt
u_t <- r_model$residuals
omega_filter <- c(r_model$coef[2], r_model$coef[3], 0, 0)
eta_t <- stats::filter(u_t, filter = omega_filter, method = "recursive")
plot(eta_t) # definitely still not stationary, finish the model anyhow and see how it does
acf(eta_t) 
pacf(eta_t)

m_final <- auto.arima(eta_t, seasonal = FALSE)
forecast(m_final, h=21)
tsdiag(m_final)


####################
# Try using the TSA package instead in case I made a model error above
rm(list = ls())
library(TSA)
library(ggplot2)

data <- readRDS("data/data_final.rds")
x <- ts(data$PRCP, start = data$datetime[1], frequency = 365)

plot(x)
acf(x, lag.max = 365)
acf(diff(x, lag = 365), lag.max = 365)
pacf(x, lag.max = 365)
pacf(diff(x, lag = 365), lag.max = 365)
ccf(data$PRCP, data$cfs, lag.max = 176)
ccf(diff(data$PRCP, lag = 365), diff(data$cfs, lag = 365), lag.max = 176)

l <- .8 # lambda = 1 doesnt transform the series
y <- forecast::BoxCox(data$cfs, lambda = l)
plot(diff(y)^2)
plot(y)
acf(y, lag.max = 365)
acf(diff(y, lag = 365), lag.max = 365)
pacf(y, lag.max = 365)
pacf(diff(y, lag = 365), lag.max = 365)
ccf(data$PRCP, y, lag.max = 176)
ccf(diff(data$PRCP, lag = 365), diff(y, lag = 365), lag.max = 176)
# BEGIN Transfer Models using TSA Package
# also normalize the variance with boxcox
y <- diff(ts(y), lag = 365)
x <- diff(data$PRCP, lag = 365) # same transform as when I estimated the model
n <- length(y)
r <- 2 # autoregressive part of the transfer function. influence of past x on y
s <- 5 # number of periods after which the effect of x is seen in y

model <- TSA::arima(
  y,
  order = c (2, 0, 0),
  #seasonal = list(order = c(0, 1, 0), period)
  xtransf = x,
  transfer = list(c(r, s)), # our (r, s)
  include.mean = FALSE,
  method = "CSS-ML"
); lmtest::coeftest(model)
barplot(coef(model)[-(1:2)], las = 2, col = "blue")
# tsdiag(model, gof.lag = 80, na.action = na.pass)
u_t <- ts(model$residuals[seq(max(s, r)+1, n)])
plot(u_t)
acf(u_t)
pacf(u_t)

model_e <- stats::arima(
  u_t,
  order = c(2, 0, 1),
  seasonal = list(order = c(0, 0, 0), period = NA),
  include.mean = FALSE
); lmtest::coeftest(model_e)
tsdiag(model_e)

















