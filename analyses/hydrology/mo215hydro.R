### project: USACE Shark River Monitoring & Assessment Program 
### goal: plot MO-215 data from EDEN
### author(s): Mack White

# load libraries ----------------------------------------------------------
# install.packages("librarian")
librarian::shelf(readr, readxl, dplyr, ggplot2, lubridate, tidyverse)

# read the data -----------------------------------------------------------
data <- read_excel("data/hydrology/mo215_current_filled.xlsx")

# data manipulation -------------------------------------------------------
data1 <- data |> 
      mutate(date = as.Date(Date),
             year = year(Date),
             month = month(Date),
             day_of_year = yday(Date))

data2 <- data1 |> 
      mutate(hydro_year = if_else(month >= 5, year + 1, year))

# calculate mean and SE for the entire dataset ----------------------------
mean_se_data <- data2 |> 
      group_by(day_of_year) |> 
      filter(year %in% c(1994:2025)) |> 
      summarise(mean_stage = mean(`Stage (cm)`),
                se = sd(`Stage (cm)`) / sqrt(n()))

# filter data for specific years for plotting -----------------------------
filtered_data <- data2 |> 
      filter(year %in% c(2021, 2022, 2023, 2024, 2025))

### projecting for jenn to determine month of late dry sampling ---
filtered_data2 <- data2 |> 
      filter(year == 2025) |> 
      arrange(Date) |> 
      mutate(daily_change = `Stage (cm)` - lag(`Stage (cm)`)) |> 
      filter(!is.na(daily_change))

last_date <- "2025-03-23"
last_stage <- 36.2712
avg_daily_change <- mean(filtered_data2$daily_change)
n_days = 120
future_dates <- seq.Date(as.Date(last_date) + 1, by = "day", length.out = n_days)
projected_stage <- last_stage + (1:n_days) * avg_daily_change
projection_df <- data.frame(
      date = future_dates,
      Stage_cm = projected_stage
) |> 
      mutate(date = as.Date(date),
             year = year(date),
             month = month(date),
             day_of_year = yday(date))

# year_palette <- c("2011" = "orange",
#                   "2015" = "red",
#                   "2016" = "purple",
#                   "2019" = "#D8E8F8",
#                   "2020" = "green",
#                   "2021" = "lightblue",
#                   "2022" = "#A4C9E9",
#                   "2023" = "#5A94CF",
#                   "2024" = "#1248A3",
#                   "2025" = "darkblue")

year_palette <- c("2020" = "#D8E8F8",
                  "2021" = "lightblue",
                  "2022" = "#A4C9E9",
                  "2023" = "#5A94CF",
                  "2024" = "#1248A3",
                  "2025" = "darkblue")

### past five year version for annual report

# generate breaks + labels for plotting -----------------------------------
breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]

# generate plot -----------------------------------------------------------
ggplot() +
      geom_line(data = filtered_data, 
                aes(x = day_of_year, y = `Stage (cm)`, color = as.factor(year)), size = 1) +
      # geom_line(data = projection_df,
      #           aes(x = day_of_year, y = Stage_cm), color = "red", size = 1, linetype = "dotted") +
      geom_line(data = mean_se_data, 
                aes(x = day_of_year, y = mean_stage), size = 1.5, color = "black") +
      geom_ribbon(data = mean_se_data, 
                  aes(x = day_of_year, ymin = mean_stage - se, ymax = mean_stage + se), fill = "#6E6E6E", alpha = 0.2) +
      scale_x_continuous(breaks = breaks, labels = labels) +
      scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
      geom_hline(yintercept = 30, color = "black", size = 1) +
      geom_hline(yintercept = 10, color = "black", size = 1) +
      scale_color_manual(values = year_palette) +
      labs(title = "Daily MO-215 Water Levels w/ Mean + Standard Error (1994-Present)",
           x = "Month",
           y = "Stage (cm)",
           color = "Calendar Year") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(hjust = 0.5, face = "bold"),
            legend.text = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = c(0.9,0.25),
            plot.background = element_rect(fill = "white"),
            legend.background = element_rect(fill = "white", colour = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(filename = "plots/hydro/mo215_usace_2025_q1_report.png",
       plot = last_plot(),
       width = 10, height = 5,
       dpi = 600)

