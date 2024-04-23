### project: USACE Shark River Monitoring & Assessment Program 
### goal: plot MO-215 data from EDEN
### author(s): Mack White

# load libraries ----------------------------------------------------------
# install.packages("librarian")
librarian::shelf(readr, readxl, dplyr, ggplot2, lubridate, tidyverse)

# read the data -----------------------------------------------------------
data <- read_excel("data/mo215_current.xlsx")

# data manipulation -------------------------------------------------------
data <- data %>%
      mutate(Date = as.Date(Date),
             Year = year(Date),
             Month = month(Date),
             DayOfYear = yday(Date))

# calculate mean and SE for the entire dataset ----------------------------
mean_se_data <- data %>%
      group_by(DayOfYear) %>%
      filter(Year %in% c(2005:2016)) |> 
      summarise(MeanStage = mean(`Stage (cm)`),
                SE = sd(`Stage (cm)`) / sqrt(n()))

# filter data for specific years for plotting -----------------------------
filtered_data <- data %>%
      filter(Year %in% c(2016:2024))

# generate breaks + labels for plotting -----------------------------------
breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]

# generate plot -----------------------------------------------------------
ggplot() +
      geom_line(data = filtered_data, aes(x = DayOfYear, y = `Stage (cm)`, color = as.factor(Year)), size = 1) +
      geom_line(data = mean_se_data, aes(x = DayOfYear, y = MeanStage), size = 1.5, color = "black") +
      geom_ribbon(data = mean_se_data, aes(x = DayOfYear, ymin = MeanStage - SE, ymax = MeanStage + SE), fill = "#6E6E6E", alpha = 0.2) +
      scale_x_continuous(breaks = breaks, labels = labels) +
      labs(title = "Daily MO-215 Water Levels w/ Mean + Standard Error (1994-Present)",
           x = "Month",
           y = "Stage (cm; elevation corrected)",
           color = "Year") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(hjust = 0.5, face = "bold"),
            legend.text = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = c(0.9,0.2),
            legend.background = element_rect(fill = "white", colour = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

# ggsave(filename = "plots/mo215_04232024_with2016.jpeg",
#        plot = last_plot(),
#        width = 10, height = 5,
#        dpi = 300)

