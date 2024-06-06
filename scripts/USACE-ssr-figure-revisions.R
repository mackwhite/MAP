library(ggplot2)
library(dplyr)

# Your existing data and plot code here
ggplot(all_data, aes(x = month_chr, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADD8E6", "2019" = "#87CEFA", "2020" = "#4682B4", "2021" = "#4169E1", "2022" = "#0000FF", "2023" = "#00008B", "2024" = "#000080")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "snook_V2_bm_ribbon_1.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

# Adjusted ggplot code for visualization
ggplot(all_data, aes(x = distance, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADD8E6", "2019" = "#87CEFA", "2020" = "#4682B4", "2021" = "#4169E1", "2022" = "#0000FF", "2023" = "#00008B", "2024" = "#000080")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Snook Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Distance Downstream (km)", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "snook_V2_bm_ribbon_1_distance.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

# Adjusted ggplot code for visualization
ggplot(all_data, aes(x = month_chr, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADD8E6", "2019" = "#87CEFA", "2020" = "#4682B4", "2021" = "#4169E1", "2022" = "#0000FF", "2023" = "#00008B", "2024" = "#000080")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Bass Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "bass_V2_bm_ribbon_1.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

# Updated ggplot code for visualization
ggplot(all_data, aes(x = distance, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADD8E6", "2019" = "#87CEFA", "2020" = "#4682B4", "2021" = "#4169E1", "2022" = "#0000FF", "2023" = "#00008B", "2024" = "#000080")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Bass Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Distance Downstream (km)", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "bass_V2_bm_ribbon_1_distance.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

ggplot(all_data, aes(x = month_chr, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADFF2F", "2019" = "#7FFF00", "2020" = "#32CD32", "2021" = "#228B22", "2022" = "#008000", "2023" = "#006400", "2024" = "#004D00")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Sunfish Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "sunfish_V2_bm_ribbon_1.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

ggplot(all_data, aes(x = distance, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#ADFF2F", "2019" = "#7FFF00", "2020" = "#32CD32", "2021" = "#228B22", "2022" = "#008000", "2023" = "#006400", "2024" = "#004D00")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Sunfish Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Distance Downstream (km)", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "sunfish_V2_bm_ribbon_1_distance.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

ggplot(all_data, aes(x = month_chr, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#FFC0CB", "2019" = "#FF69B4", "2020" = "#FF6347", "2021" = "#FF4500", "2022" = "#FF0000", "2023" = "#B22222", "2024" = "#8B0000")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Nonnative Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Month", y = "Mean Biomass (g/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "nonnative_V2_bm_ribbon_1.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )

library(ggplot2)
library(dplyr)

# Updated ggplot code with red gradients for distance data
ggplot(all_data, aes(x = distance, y = mean_bm, group = group)) +
      geom_ribbon(data = filter(all_data, group == "Baseline"), 
                  aes(ymin = mean_bm - se, ymax = mean_bm + se, fill = 'grey80'), 
                  color = "black", alpha = 0.5) +
      geom_line(data = filter(all_data, group != "Baseline"), 
                aes(color = group, size = group)) +
      scale_color_manual(values = c("2018" = "#FFC0CB", "2019" = "#FF69B4", "2020" = "#FF6347", "2021" = "#FF4500", "2022" = "#FF0000", "2023" = "#B22222", "2024" = "#8B0000")) +
      scale_size_manual(values = c("2018" = 1, "2019" = 1.2, "2020" = 1.4, "2021" = 1.6, "2022" = 1.8, "2023" = 2, "2024" = 2.2)) +
      scale_fill_manual(values = c("Baseline" = "grey80")) +
      labs(title = "Monthly Mean Nonnative Biomass with Baseline and Reporting Water Years (2018-2024)",
           x = "Distance Downstream (km)", y = "Mean Biomass (kg/100m)", color = "Year", size = "Year") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.text = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color = "black"))

# ggsave(
#       filename = "nonnative_V2_bm_ribbon_1_distance.tiff",
#       path = "plots/",
#       width = 10, height = 10
# )
