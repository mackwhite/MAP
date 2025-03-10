### project: USACE Shark River Monitoring & Assessment Program 
### goal: plot MO-215 data from EDEN
### author(s): Mack White

# load libraries ----------------------------------------------------------
# install.packages("librarian")
librarian::shelf(readr, readxl, dplyr, ggplot2, lubridate, tidyverse)

# read the data -----------------------------------------------------------
data <- read_excel("data/hydrology/mo215_current.xlsx")

# data manipulation -------------------------------------------------------
data1 <- data |> 
      mutate(date = as.Date(Date),
             year = year(Date),
             month = month(Date),
             day_of_year = yday(Date))

data2 <- data1 |>
      mutate(hydro_year = if_else(month >= 5, year + 1, year),
             hydro_day_of_year = if_else(month >= 5,
                                         day_of_year - yday(as.Date(paste0(year, "-05-01"))) + 1,
                                         day_of_year + (365 - yday(as.Date(paste0(year - 1, "-04-30"))))))


# calculate mean and SE for the entire dataset ----------------------------
mean_se_data <- data2 |> 
      group_by(hydro_day_of_year) |> 
      filter(hydro_year %in% c(1994:2025)) |> 
      summarise(mean_stage = mean(`Stage (cm)`),
                se = sd(`Stage (cm)`) / sqrt(n()))

# filter data for specific years for plotting -----------------------------
filtered_data <- data2 |> 
      filter(hydro_year %in% c(2015, 2018, 2021, 2024, 2025))

year_palette <- c("2015" = "#D8E8F8",
                  "2018" = "#A4C9E9",
                  "2021" = "#5A94CF",
                  "2024" = "#1248A3",
                  "2025" = "darkblue")

# generate breaks + labels for plotting -----------------------------------
breaks <- c(1, 32, 62, 93, 123, 154, 184, 215, 245, 276, 306, 336) # Approximate start days of months in the hydrologic year
labels <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

# generate plot -----------------------------------------------------------
ggplot() +
      geom_line(data = filtered_data, 
                aes(x = hydro_day_of_year, y = `Stage (cm)`, color = as.factor(hydro_year)), size = 1) +
      geom_line(data = mean_se_data, 
                aes(x = hydro_day_of_year, y = mean_stage), size = 1.5, color = "black") +
      geom_ribbon(data = mean_se_data, 
                  aes(x = hydro_day_of_year, ymin = mean_stage - se, ymax = mean_stage + se), fill = "#6E6E6E", alpha = 0.2) +
      scale_x_continuous(breaks = breaks, labels = labels) +
      scale_color_manual(values = year_palette)+
      labs(title = "Daily MO-215 Water Levels w/ Mean + Standard Error (1994-Present)",
           x = "Month",
           y = "Stage (cm)",
           color = "Hydrologic Year") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(hjust = 0.5, face = "bold"),
            legend.text = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = c(0.9,0.85),
            legend.background = element_rect(fill = "white", colour = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black")) 

ggsave(filename = "plots/hydro/mo215_longterm_average_plus_wet_dry_years_and_2025.jpeg",
       plot = last_plot(),
       width = 10, height = 5,
       dpi = 300)

