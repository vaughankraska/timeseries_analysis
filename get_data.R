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
gal_data = read.delim("gallatin_water.txt", 
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
weather_data <- read.csv("weather_MSU.csv", header = TRUE)
weather_data$DATE <- as.Date(weather_data$DATE, format="%Y-%m-%d")
weather_data <- weather_data[order(weather_data$DATE),]
keep_cols = c("DATE", "PRCP", "SNOW",
              "TMAX", "TMIN", "AWBT", "AWND", "FMTM", "RHAV", "RHMN",
              "RHMX"
)
weather_data <- weather_data[keep_cols]
str(weather_data)

###################
# merge datasets
merged_df = merge(gal_data, weather_data, 
                  by.x = "datetime", 
                  by.y = "DATE",
                  all.x = TRUE
                  )
merged_df$SNOW = replace(merged_df$SNOW, is.na(merged_df$SNOW), 0)
merged_df$PRCP = replace(merged_df$PRCP, is.na(merged_df$PRCP), 0)
sapply(merged_df, function(x) sum(is.na(x)))
