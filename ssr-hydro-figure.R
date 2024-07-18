###project: MAP
###author(s): Mack White
###goal(s): SSR Plots - Revisions based on USACE feedback
###date(s): July 2024
###note(s): 

### generate new summary stoplight indicator figure
# use original color scheme and add photos of fishes
### generate new z-scored box-plot of water levels/hydrograph
# generate z-scored figure of water levels at MO-215 such that the data
## are scaled by the mean and standard deviation of the baseline period
### add information to indicator summary table
# add information about how red, yellow, or green determined (percentiles)
### revise figure 4
# singular line for reporting period mean and single line for WY 2024
# increase the size of the axis labels

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, writexl, scales, ggplot2, ggpubr)

water_year_ssr <- read_csv("data/hydrologic_year.csv")
data <- read_csv("data/ssr_waterlevels.csv")

# data manipulation -------------------------------------------------------
data_1 <- data |> 
      mutate(date = mdy(date),
             year = year(date),
             month = month(date),
             DayOfYear = yday(date),
             month_chr = factor(month, levels = c("1", "2", "3", "4", "5", "6", 
                                                  "7", "8", "9", "10", "11", "12"),
                                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                           "Aug", "Sep", "Oct", "Nov", "Dec")),
             season = factor(month_chr, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                                   "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                             labels = c("dry", "dry", "dry", "dry", "wet", "wet", "wet", "wet", "wet", 
                                        "wet", "dry", "dry")),
             hydroyear = if_else(month >= 5, year + 1, year)) |> 
      select(-stage_feet) |>
      filter(hydroyear >= 2005) |> 
      mutate(period = if_else(hydroyear >= 2018, "Reporting", "Baseline"))
glimpse(data_1)

# Filter and compute mean, sd for Baseline period by season
baseline_stats <- data_1 |> 
      filter(period == "Baseline") |> 
      group_by(season) |> 
      summarise(mean_stage_cm = mean(stage_cm, na.rm = TRUE),
                sd_stage_cm = sd(stage_cm, na.rm = TRUE))

# Filter data for Reporting period
reporting_data <- data_1 |> 
      filter(period == "Reporting")

# Join baseline stats with reporting data and calculate z-scores
reporting_z_scores <- reporting_data |> 
      left_join(baseline_stats, by = "season") |> 
      mutate(z_score_stage_cm = (stage_cm - mean_stage_cm) / sd_stage_cm)  |> 
      select(-mean_stage_cm, -sd_stage_cm)

panel_b <- ggplot(reporting_z_scores, aes(as.factor(hydroyear), 
                                          z_score_stage_cm, 
                                          group = hydroyear)) +
           geom_boxplot() +
           facet_wrap(~season, labeller = labeller(season = c(dry = "Dry", wet = "Wet"))) +
           labs(x = "Water Year",
                y = "Marsh Depth (z-scored)") +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           theme(panel.background = element_rect(fill = "white"),
                axis.title = element_text(face = "bold", size = 18),
                axis.line = element_line("black"),
                axis.text = element_text(face = "bold", size = 18),
                legend.title = element_blank(),
                legend.text = element_text(face = "bold", size = 18),
                legend.position = "bottom",
                strip.text = element_text(size = 18, face = "bold"))

start_year <- year(min(reporting_data$date))
end_year <- year(max(reporting_data$date))
may_nov_dates <- sort(c(seq(as.Date(paste(start_year, "05", "01", sep="-")), as.Date(paste(end_year, "05", "01", sep="-")), by="year"),
                        seq(as.Date(paste(start_year, "11", "01", sep="-")), as.Date(paste(end_year, "11", "01", sep="-")), by="year")))

panel_a <- ggplot(reporting_data, aes(date, stage_cm)) +
      geom_line(size = 1.5) + 
      labs(x = "Date",
           y = "Marsh Depth (cm)") +
      scale_x_date(breaks = may_nov_dates, labels = date_format("%m/%Y"), date_minor_breaks = "1 month") +
      scale_y_continuous(breaks = seq(0, 90, by = 15)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_text(face = "bold", size = 18),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold", size = 18),
            legend.title = element_blank(),
            legend.text = element_text(face = "bold", size = 18),
            legend.position = "bottom")


### plot of supply trends by community
ggarrange(panel_a, panel_b,
          labels = c('a)','b)'),
          font.label = list(size = 20, face = "bold"),
          ncol = 1, nrow = 2, align = "v")

# saving for publication
ggsave("plots/ssr-hydro.tiff", units = "in", width = 14,
       height = 10, dpi =  600, compression = "lzw")
      

