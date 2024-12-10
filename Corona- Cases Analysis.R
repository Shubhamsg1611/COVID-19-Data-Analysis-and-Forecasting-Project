# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
data <- read.csv("C:\\Users\\shubh\\Downloads\\Corona.csv", stringsAsFactors = FALSE)

# View the first few rows
head(data)

# Check the structure
str(data)

# Summarize the data
summary(data)

# Convert Date to Date format
data$Date <- mdy(data$Date)

# Fill missing values (example: replace NAs in numeric columns with 0)
data[is.na(data)] <- 0

# Check for duplicates
data <- data %>% distinct()

# Total cases, recoveries, and deaths by country
country_summary <- data %>%
  group_by(Country_Region) %>%
  summarise(
    Total_Positive = sum(positive, na.rm = TRUE),
    Total_Recovered = sum(recovered, na.rm = TRUE),
    Total_Deaths = sum(death, na.rm = TRUE)
  )

# Plot positive cases over time for a specific country (e.g., United States)
us_data <- data %>% filter(Country_Region == "United States", Province_State == "All States")

ggplot(us_data, aes(x = Date, y = positive)) +
  geom_line(color = "blue") +
  labs(title = "Positive Cases Over Time (United States)", x = "Date", y = "Positive Cases") +
  theme_minimal()

# Aggregate by Country for a specific date
latest_date <- max(data$Date)
latest_data <- data %>% filter(Date == latest_date, Province_State == "All States")

# Plot bar chart for top 10 countries by positive cases
top_countries <- latest_data %>%
  arrange(desc(positive)) %>%
  slice_head(n = 10)

ggplot(top_countries, aes(x = reorder(Country_Region, positive), y = positive)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by Positive Cases", x = "Country", y = "Positive Cases") +
  theme_minimal()

data <- data %>%
  mutate(CFR = ifelse(positive > 0, (death / positive) * 100, 0))

# View top countries by CFR
cfr_summary <- data %>%
  group_by(Country_Region) %>%
  summarise(Average_CFR = mean(CFR, na.rm = TRUE)) %>%
  arrange(desc(Average_CFR))

head(cfr_summary)

data <- data %>%
  group_by(Country_Region, Province_State) %>%
  arrange(Date) %>%
  mutate(Daily_Growth_Rate = (positive - lag(positive)) / lag(positive) * 100)

# Plot daily growth rate for a specific country
us_data <- data %>% filter(Country_Region == "United States", Province_State == "All States") %>%
  arrange(Date) %>%
  mutate(
    Date = as.Date(Date, format="%m/%d/%Y"),
    Daily_Growth_Rate = (positive - lag(positive)) / lag(positive) * 100
  )

head(us_data)  # Check the first few rows of the data frame to ensure the column exists

ggplot(us_data, aes(x = Date, y = Daily_Growth_Rate)) +
  geom_line(color = "red") +
  labs(title = "Daily Growth Rate of Cases in 2020 (United States)", x = "Date", y = "Growth Rate (%)") +
  theme_minimal()

data <- data %>%
  mutate(CFR = ifelse(positive > 0, (death / positive) * 100, 0))

# View top countries by CFR
cfr_summary <- data %>%
  group_by(Country_Region) %>%
  summarise(Average_CFR = mean(CFR, na.rm = TRUE)) %>%
  arrange(desc(Average_CFR))

head(cfr_summary)

# Filter data for India
india_data <- data %>%
  filter(Country_Region == "India") %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date)

# Summary statistics
summary(india_data)

# Check the structure of the dataset
str(india_data)

# Check the range of dates
range(india_data$Date)

# Number of unique provinces/states
unique_states <- length(unique(india_data$Province_State))
cat("Number of states/provinces in India:", unique_states, "\n")

library(ggplot2)

# Plot total positive cases
ggplot(india_data, aes(x = Date, y = positive, group = Province_State, color = Province_State)) +
  geom_line() +
  labs(title = "Trend of Positive Cases in India by State", x = "Date", y = "Positive Cases") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

# Calculate daily growth rate
india_data <- india_data %>%
  group_by(Province_State) %>%
  mutate(Daily_Positive = positive - lag(positive),
         Daily_Growth_Rate = (Daily_Positive / lag(positive)) * 100)

# Plot daily growth rate
ggplot(india_data, aes(x = Date, y = Daily_Growth_Rate, group = Province_State, color = Province_State)) +
  geom_line() +
  labs(title = "Daily Growth Rate of Cases in India by State", x = "Date", y = "Growth Rate (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Top 5 states with the highest total positive cases
top_states <- india_data %>%
  group_by(Province_State) %>%
  summarize(Total_Positive = max(positive, na.rm = TRUE)) %>%
  arrange(desc(Total_Positive)) %>%
  head(5)

# Filter data for these states
top_states_data <- india_data %>% filter(Province_State %in% top_states$Province_State)

# Plot trends for top states
ggplot(top_states_data, aes(x = Date, y = positive, color = Province_State)) +
  geom_line(size = 1) +
  labs(title = "Top 5 States with Highest Positive Cases", x = "Date", y = "Positive Cases") +
  theme_minimal()

# Reshape data for plotting
library(tidyr)

india_trends <- india_data %>%
  group_by(Date) %>%
  summarize(Total_Active = sum(active, na.rm = TRUE),
            Total_Recovered = sum(recovered, na.rm = TRUE),
            Total_Deaths = sum(death, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Total_Active, Total_Recovered, Total_Deaths), names_to = "Status", values_to = "Count")

# Plot
ggplot(india_trends, aes(x = Date, y = Count, fill = Status)) +
  geom_area(position = "stack", alpha = 0.7) +
  labs(title = "Active, Recovered, and Death Cases in India", x = "Date", y = "Count") +
  theme_minimal()

# Calculate positivity rate
india_data <- india_data %>%
  mutate(Positivity_Rate = ifelse(daily_tested > 0, (daily_positive / daily_tested) * 100, NA))

# Plot daily tests and positivity rate
ggplot(india_data, aes(x = Date)) +
  geom_line(aes(y = daily_tested, color = "Daily Tested")) +
  geom_line(aes(y = daily_positive, color = "Daily Positive")) +
  labs(title = "Daily Testing Trends in India", x = "Date", y = "Count") +
  scale_color_manual(values = c("Daily Tested" = "blue", "Daily Positive" = "red")) +
  theme_minimal()

# Positivity Rate
ggplot(india_data, aes(x = Date, y = Positivity_Rate)) +
  geom_line(color = "purple") +
  labs(title = "Positivity Rate Over Time in India", x = "Date", y = "Positivity Rate (%)") +
  theme_minimal()

# Filter data for India and aggregate deaths
india_deaths <- data %>%
  filter(Country_Region == "India") %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  group_by(Date) %>%
  summarize(Total_Deaths = sum(death, na.rm = TRUE)) %>%
  arrange(Date)

# Check for missing data
sum(is.na(india_deaths$Total_Deaths))

# Fill missing dates if necessary
all_dates <- seq(min(india_deaths$Date), max(india_deaths$Date), by = "day")
india_deaths <- india_deaths %>%
  complete(Date = all_dates, fill = list(Total_Deaths = 0))

# Create a time series object
ts_deaths <- ts(india_deaths$Total_Deaths, start = c(2020, 1), frequency = 365)

library(forecast)

# Fit an ARIMA model
model <- auto.arima(ts_deaths)

# Forecast for the next 365 days (2025)
forecasted_deaths <- forecast(model, h = 365)

# Plot the forecast
autoplot(forecasted_deaths) +
  labs(title = "Forecast of COVID-19 Deaths in India for 2025", x = "Year", y = "Deaths") +
  theme_minimal()

# Extract the predictions for 2025
forecast_2025 <- forecasted_deaths$mean
dates_2025 <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
predicted_2025 <- data.frame(Date = dates_2025, Predicted_Deaths = forecast_2025[1:365])

# Display a summary
summary(predicted_2025)

ggplot(india_deaths, aes(x = Date, y = Total_Deaths)) +
  geom_line(color = "blue") +
  labs(title = "Historical COVID-19 Deaths in India", x = "Date", y = "Deaths") +
  theme_minimal()

# Result: "predicted_2025" data show that the forecasted deaths in India for 2025 are consistently zero across all dates.