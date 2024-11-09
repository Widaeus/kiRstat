# Load necessary libraries
library(tidyverse)
library(lubridate)

# Step 1: Reading
data <- read.csv("Chronograf Data.csv")

data <- data[-c(1, 2), ]

names(data) <- gsub("_", "", as.character(data[1, ]))
data <- data[-1, ]

rownames(data) <- NULL

# Correct data types
data$time <- ymd_hms(data$time)
data$humidity <- as.integer(data$humidity)
data$temperature <- as.double(data$temperature)


# Step 2: Cleaning
clean_data <- data %>%
  drop_na() %>%
  filter(temperature >= -40 & temperature <= 50) %>%
  filter(humidity >= 0 & humidity <= 100)

# Step 3: Generate temperature and humidity graphs
ggplot(clean_data, aes(x = time)) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = (temperature + 25) * (80 / 50) + 20, color = "Temperature")) +
  scale_y_continuous(name = "Humidity (%)",
                     sec.axis = sec_axis(~ (. - 20) * (50 / 80) - 25,
                     name = "Temperature (°C)")) +
  labs(title = "Temperature and Humidity Over Time", x = "Time") +
  scale_color_manual(values = c("Temperature" = "red", "Humidity" = "blue"))


# Step 4: Calculate daily mean, min, and max
daily_stats <- clean_data %>%
  group_by(date = as.Date(time)) %>%
  summarise(
    mean_temp = mean(temperature),
    min_temp = min(temperature),
    max_temp = max(temperature),
    meanhumidity = mean(humidity),
    minhumidity = min(humidity),
    maxhumidity = max(humidity)
  )

# Plot, dotted min/max, solid mean
ggplot(daily_stats, aes(x = date)) +
  geom_line(aes(y = meanhumidity, color = "Mean Humidity", linetype = "solid"), alpha = 1) +
  geom_line(aes(y = minhumidity, color = "Min Humidity", linetype = "dotted"), alpha = 0.5) +
  geom_line(aes(y = maxhumidity, color = "Max Humidity", linetype = "dotted"), alpha = 0.5) +
  geom_line(aes(y = (mean_temp + 25) * (80 / 50) + 20, color = "Mean Temperature", linetype = "solid"), alpha = 1) +
  geom_line(aes(y = (min_temp + 25) * (80 / 50) + 20, color = "Min Temperature", linetype = "dotted"), alpha = 0.5) +
  geom_line(aes(y = (max_temp + 25) * (80 / 50) + 20, color = "Max Temperature", linetype = "dotted"), alpha = 0.5) +
  scale_y_continuous(name = "Humidity (%)",
                     sec.axis = sec_axis(~ (. - 20) * (50 / 80) - 25,
                     name = "Temperature (°C)")) +
  labs(title = "Daily Temperature and Humidity Statistics", x = "Date") +
  scale_color_manual(values = c("Mean Temperature" = "red", "Min Temperature" = "red", "Max Temperature" = "red",
                     "Mean Humidity" = "blue", "Min Humidity" = "blue", "Max Humidity" = "blue")) +
  scale_linetype_manual(values = c("solid" = "solid", "dotted" = "dotted"))