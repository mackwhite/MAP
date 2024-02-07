# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
data <- read_excel("path/to/your/mo215_feb2024.xlsx")

# Convert 'Date' to Date format and extract year, month, and day of year
data <- data %>%
      mutate(Date = as.Date(Date),
             Year = year(Date),
             Month = month(Date),
             DayOfYear = yday(Date))

# Filter data for the years 2018, 2021, 2023, and 2024
filtered_data <- data %>%
      filter(Year %in% c(2018, 2021, 2023, 2024))

# Calculate mean and standard error for each day of year across the selected years
mean_se_data <- filtered_data %>%
      group_by(DayOfYear) %>%
      summarise(MeanStage = mean(`Stage (cm)`),
                SE = sd(`Stage (cm)`) / sqrt(n()))

# Merge back to get labels for months
mean_se_data <- mean_se_data %>%
      mutate(MonthLabel = month(ymd(paste0("2023-", day_of_year_to_date(DayOfYear, leap_year = FALSE)), tz = "UTC"), label = TRUE))

# Plotting
ggplot() +
      geom_line(data = filtered_data, aes(x = DayOfYear, y = `Stage (cm)`, color = as.factor(Year)), size = 1) +
      geom_line(data = mean_se_data, aes(x = DayOfYear, y = MeanStage), size = 1.5, color = "black") +
      geom_ribbon(data = mean_se_data, aes(x = DayOfYear, ymin = MeanStage - SE, ymax = MeanStage + SE), fill = "grey", alpha = 0.2) +
      scale_x_continuous(breaks = mean_se_data$DayOfYear[mean_se_data$MonthLabel %in% month.abb], labels = unique(mean_se_data$MonthLabel)) +
      labs(title = "Daily Stage (cm) with Mean and Standard Error for Selected Years",
           x = "Month",
           y = "Stage (cm)",
           color = "Year") +
      theme_minimal()

# Note: Ensure the path to your .xlsx file is correctly specified in the read_excel function.
# This code snippet assumes the 'lubridate' and 'ggplot2' packages are installed and loaded,
# along with 'dplyr' for data manipulation. If not, you can install them using install.packages().
