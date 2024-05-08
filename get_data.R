library(dataRetrieval)
library(ggplot2)

#######################
# Get gallatin river water data
# came from: https://nwis.waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=06043500&legacy=&period=&begin_date=1989-08-01&end_date=2024-05-05
siteNumber <- "06043500"

site <- whatNWISsites(
  sites = siteNumber
)
site

# read the text file from gallatin water data
gal_data = read.delim("data/gallatin_water.txt", 
                      sep = "\t", 
                      header = FALSE, 
                      skip = 30,
                      col.names = c("agency_cd", "site_no", "datetime", "cfs", "approval_status")
                      )
gal_data$datetime <- as.Date(gal_data$datetime, format="%Y-%m-%d")
gal_data <- gal_data[order(gal_data$datetime),]
str(gal_data)
# quick visual check
ggplot(gal_data, aes(x=datetime, y=cfs, group=1)) + 
  geom_line() + 
  labs(title = "gallatin discharge")

startDate = gal_data$datetime[1]
endDate = gal_data$datetime[dim(gal_data)[1]]

###################
# Get weather data
# source: https://www.ncei.noaa.gov/access/search/data-search/daily-summaries?pageNum=1&startDate=1989-08-01T23:59:59&endDate=2024-05-04T00:00:00&bbox=45.864,-111.415,45.195,-110.932
weather_data <- read.csv("data/weather_MSU.csv", header = TRUE)
weather_data$DATE <- as.Date(weather_data$DATE, format="%Y-%m-%d")
weather_data <- weather_data[order(weather_data$DATE),]
keep_cols = c("DATE", "PRCP", "SNOW", "TMAX", "TMIN")
weather_data <- weather_data[keep_cols]
str(weather_data)

###################
# merge datasets
merged_df <- merge(gal_data, weather_data, 
                  by.x = "datetime", 
                  by.y = "DATE",
                  all.x = TRUE
                  )

# replace missing values with nearby weather station
alt_weather <- read.csv("data/alt_weather.csv", header = TRUE)
alt_weather <- alt_weather[c("DATE", "PRCP", "TMAX", "TMIN")]
colnames(alt_weather) <- c("DATE", "APRCP", "ATMAX", "ATMIN")
alt_weather$DATE <- as.Date(alt_weather$DATE, format = "%Y-%m-%d")

merged_df <- merge(merged_df, alt_weather, 
                   by.x = "datetime", by.y = "DATE", 
                   all.x = TRUE)
merged_df <- merged_df %>%
  dplyr::mutate(
    PRCP = dplyr::coalesce(PRCP, APRCP),
    TMIN = dplyr::coalesce(TMIN, ATMIN),
    TMAX = dplyr::coalesce(TMAX, ATMAX)
  ) %>%
  dplyr::select(datetime, cfs, PRCP, TMAX, TMIN, SNOW) 

merged_df$SNOW <-replace(merged_df$SNOW, is.na(merged_df$SNOW), 0)
# merged_df$PRCP <-replace(merged_df$PRCP, is.na(merged_df$PRCP), 0)
# merged_df$TMAX <-zoo::na.approx(merged_df$TMAX, na.rm=FALSE)
# merged_df$TMIN <-zoo::na.approx(merged_df$TMIN, na.rm=FALSE)
# sapply(merged_df, function(x) sum(is.na(x)))

df_final <- head(merged_df, -2)
sapply(df_final, function(x) sum(is.na(x)))

# Add interaction variable between max temp and precipitation
df_final$TMAX_PRCP = df_final$TMAX * df_final$PRCP
df_final$TMIN_PRCP = df_final$TMIN * df_final$PRCP
# Save file as final data
saveRDS(df_final, "data/data_final.rds")

# create weekly version dataset
df_final_weekly <- df_final
df_final_weekly$week <- lubridate::floor_date(
  df_final_weekly$datetime,
  unit = "week",
  week_start = 1
)
df_final_weekly <- df_final_weekly %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    datetime = min(datetime, na.rm = TRUE),
    cfs = mean(cfs, na.rm = TRUE),
    PRCP = sum(PRCP, na.rm = TRUE),
    TMAX = max(TMAX, na.rm = TRUE),
    TMIN = min(TMIN, na.rm = TRUE),
    SNOW = sum(SNOW, na.rm = TRUE),
    TMAX_PRCP = mean(TMAX_PRCP, na.rm = TRUE),
    TMIN_PRCP = mean(TMIN_PRCP, na.rm = TRUE)
  )
saveRDS(df_final_weekly, "data/data_final_weekly.rds")

# create monthly dataset
df_final_monthly <- df_final
df_final_monthly$month <- lubridate::floor_date(
  df_final_monthly$datetime,
  unit = "month",
)
df_final_monthly <- df_final_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    datetime = min(datetime, na.rm = TRUE),
    cfs = mean(cfs, na.rm = TRUE),
    PRCP = sum(PRCP, na.rm = TRUE),
    TMAX = max(TMAX, na.rm = TRUE),
    TMIN = min(TMIN, na.rm = TRUE),
    SNOW = sum(SNOW, na.rm = TRUE),
    TMAX_PRCP = mean(TMAX_PRCP, na.rm = TRUE),
    TMIN_PRCP = mean(TMIN_PRCP, na.rm = TRUE)
  )

saveRDS(df_final_monthly, "data/data_final_monthly.rds")



